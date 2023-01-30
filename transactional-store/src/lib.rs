use std::{
    fmt::{Debug, Display},
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};

use dashmap::DashMap;
use parking_lot::RwLock;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TransactionalStoreError<K>
where
    K: Eq + std::hash::Hash + Clone + Send + Sync + Debug + Display,
{
    CollectionNotFound { name: String },
    ConcurrentDeleteConflict,
    SerializationError,
    RecordNotFound { id: K },
    TransactionNotFound { id: u64 },
    TransactionAlreadyFinished { id: u64 },
}

pub type StoreResult<K, T> = std::result::Result<T, TransactionalStoreError<K>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IsolationLevel {
    ReadUncommitted,
    Serializable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TransactionStatus {
    Aborted,
    Committed { commit_id: u64 },
    Pending,
    RolledBack,
}

impl TransactionStatus {
    pub fn is_active(&self) -> bool {
        matches!(self, TransactionStatus::Pending,)
    }

    pub fn is_committed(&self) -> bool {
        matches!(self, TransactionStatus::Committed { .. },)
    }

    pub fn is_finished(&self) -> bool {
        matches!(
            self,
            TransactionStatus::Aborted
                | TransactionStatus::Committed { .. }
                | TransactionStatus::RolledBack,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TransactionState {
    pub status: TransactionStatus,
}

impl TransactionState {
    pub fn is_active(&self) -> bool {
        self.status.is_active()
    }

    pub fn is_committed(&self) -> bool {
        self.status.is_committed()
    }

    pub fn is_finished(&self) -> bool {
        self.status.is_finished()
    }

    pub fn new(status: TransactionStatus) -> Self {
        Self { status }
    }
}

pub struct TransactionalStore<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Sync + Debug + Display + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    last_commit_id: Arc<AtomicU64>,
    last_transaction_id: Arc<AtomicU64>,
    store: DashMap<String, Arc<DashMap<K, TransactionalRow<V>>>>,
    transaction_catalog: Arc<DashMap<u64, TransactionState>>,
}

pub struct TransactionalRow<V>
where
    V: Clone + Send + Sync + Debug + Display,
{
    row_versions: Arc<RwLock<VersionedRows<V>>>,
    last_locked_by: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionedRows<V>
where
    V: Clone + Send + Sync + Debug + Display,
{
    rows: Vec<VersionedRow<V>>,
    was_deleted: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VersionedRow<V>
where
    V: Clone + Send + Sync + Debug + Display,
{
    row: Option<V>,
    expired: bool,
    xmin: u64,
    xmax: Option<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TransactionContext {
    // Only generated when transaction has to lock a row
    pub id: Option<u64>,
    pub previous_commit_id: u64,
    pub isolation_level: IsolationLevel,
}

impl TransactionContext {
    pub fn is_serializable(&self) -> bool {
        self.isolation_level == IsolationLevel::Serializable
    }

    pub fn is_read_uncommitted(&self) -> bool {
        self.isolation_level == IsolationLevel::ReadUncommitted
    }

    pub fn is_read_committed(&self) -> bool {
        !self.is_serializable()
    }

    pub fn set_isolation_level(&mut self, isolation_level: IsolationLevel) {
        self.isolation_level = isolation_level;
    }
}

// Transaction id for finding transaction in catalog and given externally
// Also another transaction id generated on first write operation to use for row versions

pub trait TransactionalStoreExt<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Sync + Debug + Display + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    fn new() -> Self;
    fn collection(&self, collection: &str) -> StoreResult<K, Arc<DashMap<K, TransactionalRow<V>>>>;
    fn create_collection(&self, collection: &str) -> StoreResult<K, ()>;
    fn drop_collection(&self, collection: &str) -> StoreResult<K, ()>;
    fn get(
        &self,
        collection: &str,
        key: &K,
        transaction: Option<&TransactionContext>,
    ) -> StoreResult<K, Option<V>>;
    fn set(
        &self,
        collection: &str,
        key: K,
        value: V,
        transaction: Option<&mut TransactionContext>,
    ) -> StoreResult<K, ()>;
    fn delete(
        &self,
        collection: &str,
        key: K,
        transaction: Option<&mut TransactionContext>,
    ) -> StoreResult<K, ()>;
    fn begin_transaction(&self) -> StoreResult<K, TransactionContext>;
    fn commit_transaction(&self, transaction: &TransactionContext) -> StoreResult<K, ()>;
    fn rollback_transaction(&self, transaction: &TransactionContext) -> StoreResult<K, ()>;
    fn rows_iter(&self, collection: &str) -> StoreResult<K, RecordIteratorContainer<K, V>>;
    fn rows_iter_with_transaction_id(
        &self,
        collection: &str,
        transaction_id: u64,
    ) -> StoreResult<K, RecordTransactionIteratorContainer<K, V>>;
}

impl<K, V> TransactionalStore<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    fn lock_row(
        &self,
        row: &mut TransactionalRow<V>,
        transaction_id: u64,
        isolation_level: IsolationLevel,
    ) -> StoreResult<K, u64> {
        if transaction_id != row.last_locked_by {
            if let Some(transaction) = self.transaction_catalog.get(&row.last_locked_by) {
                if transaction.is_active() && isolation_level == IsolationLevel::Serializable {
                    // TODO: Costumize error based on action
                    return Err(TransactionalStoreError::SerializationError);
                }
            }
        }

        row.last_locked_by = transaction_id;

        Ok(transaction_id)
    }

    fn get_next_transaction_id(&self) -> u64 {
        self.last_transaction_id.fetch_add(1, Ordering::Relaxed) + 1
    }

    fn get_latest_commit_id(&self) -> u64 {
        self.last_commit_id.load(Ordering::Relaxed)
    }

    fn get_next_commit_id(&self) -> u64 {
        self.last_commit_id.fetch_add(1, Ordering::Relaxed) + 1
    }

    fn get_tx_id_from_opt(
        &self,
        transaction: Option<&mut TransactionContext>,
    ) -> StoreResult<K, u64> {
        if let Some(transaction_context @ TransactionContext { id: None, .. }) = transaction {
            let next_id = self.create_transaction_entry();
            transaction_context.id = Some(next_id);
            Ok(next_id)
        } else if let Some(TransactionContext { id: Some(id), .. }) = transaction {
            Ok(*id)
        } else {
            let mut transaction = self.begin_transaction()?;
            self.get_tx_id_from_opt(Some(&mut transaction))
        }
    }

    fn create_transaction_entry(&self) -> u64 {
        let key = self.get_next_transaction_id();
        self.transaction_catalog
            .insert(key, TransactionState::new(TransactionStatus::Pending));
        key
    }
}

impl<K, V> TransactionalStoreExt<K, V> for TransactionalStore<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    fn new() -> Self {
        Self {
            last_commit_id: Default::default(),
            last_transaction_id: Default::default(),
            store: DashMap::new(),
            transaction_catalog: DashMap::new().into(),
        }
    }

    fn collection(
        &self,
        collection_name: &str,
    ) -> StoreResult<K, Arc<DashMap<K, TransactionalRow<V>>>> {
        self.store
            .get(collection_name)
            .map(|collection| collection.value().clone())
            .ok_or(TransactionalStoreError::CollectionNotFound {
                name: collection_name.to_string(),
            })
    }

    fn create_collection(&self, collection_name: &str) -> StoreResult<K, ()> {
        self.store
            .entry(collection_name.to_string())
            .or_insert_with(|| DashMap::new().into());
        Ok(())
    }

    fn drop_collection(&self, collection_name: &str) -> StoreResult<K, ()> {
        self.store.remove(collection_name);
        Ok(())
    }

    fn get(
        &self,
        collection: &str,
        key: &K,
        transaction: Option<&TransactionContext>,
    ) -> StoreResult<K, Option<V>> {
        if let Some(&TransactionContext {
            isolation_level: IsolationLevel::Serializable,
            previous_commit_id: previous_write_transaction_id,
            ..
        }) = transaction
        {
            Ok(self.collection(collection)?.get(key).and_then(|row| {
                row.row_versions
                    .read()
                    .rows
                    .iter()
                    .rev()
                    .find(|row| row.xmin <= previous_write_transaction_id)
                    .and_then(|row| row.row.clone())
            }))
        } else {
            Ok(self.collection(collection)?.get(key).and_then(|row| {
                row.row_versions
                    .read()
                    .rows
                    .last()
                    .and_then(|row| row.row.clone())
            }))
        }
    }

    fn set(
        &self,
        collection_name: &str,
        key: K,
        value: V,
        transaction: Option<&mut TransactionContext>,
    ) -> StoreResult<K, ()> {
        let isolation_level = transaction
            .as_ref()
            .map(|transaction| transaction.isolation_level)
            .unwrap_or(IsolationLevel::Serializable);
        let tx_id = self.get_tx_id_from_opt(transaction)?;
        let mut inserted = false;
        let collection = self.collection(collection_name)?;
        let mut row = collection.entry(key).or_insert_with(|| {
            inserted = true;
            let commit_id = self.get_next_commit_id();
            self.transaction_catalog.insert(
                tx_id,
                TransactionState {
                    status: TransactionStatus::Committed { commit_id },
                },
            );
            TransactionalRow {
                row_versions: Arc::new(RwLock::new(VersionedRows {
                    rows: vec![VersionedRow {
                        expired: false,
                        row: Some(value.clone()),
                        xmin: tx_id,
                        xmax: None,
                    }],
                    was_deleted: false,
                })),
                last_locked_by: tx_id,
            }
        });

        if inserted {
            return Ok(());
        }

        self.lock_row(&mut *row, tx_id, isolation_level)?;
        let mut row = row.row_versions.write();
        row.rows.iter_mut().for_each(|row| row.expired = true);
        if let Some(last_version) = row.rows.last_mut() {
            last_version.xmax = Some(tx_id);
            last_version.expired = true;
        }
        row.rows.push(VersionedRow {
            row: Some(value),
            xmin: tx_id,
            xmax: None,
            expired: false,
        });

        Ok(())
    }

    fn delete(
        &self,
        collection_name: &str,
        key: K,
        transaction: Option<&mut TransactionContext>,
    ) -> StoreResult<K, ()> {
        let isolation_level = transaction
            .as_ref()
            .map(|transaction| transaction.isolation_level)
            .unwrap_or(IsolationLevel::Serializable);
        let collection = self.collection(collection_name)?;
        let mut row = if let Some(row) = collection.get_mut(&key) {
            row
        } else {
            return Err(TransactionalStoreError::RecordNotFound { id: key });
        };

        let transaction_id = self.get_tx_id_from_opt(transaction)?;

        self.lock_row(&mut row, transaction_id, isolation_level)?;

        drop(row);

        collection.remove(&key);

        let mut transaction = if let Some(tx) = self.transaction_catalog.get_mut(&transaction_id) {
            tx
        } else {
            return Err(TransactionalStoreError::TransactionNotFound { id: transaction_id });
        };
        transaction.status = TransactionStatus::Committed {
            commit_id: self.get_next_commit_id(),
        };

        Ok(())
    }

    fn begin_transaction(&self) -> StoreResult<K, TransactionContext> {
        Ok(TransactionContext {
            id: None,
            previous_commit_id: self.get_latest_commit_id(),
            isolation_level: IsolationLevel::Serializable,
        })
    }

    fn commit_transaction(&self, transaction: &TransactionContext) -> StoreResult<K, ()> {
        if let Some(id) = transaction.id {
            let mut transaction = if let Some(tx) = self.transaction_catalog.get_mut(&id) {
                tx
            } else {
                return Err(TransactionalStoreError::TransactionNotFound { id });
            };
            let commit_id = self.get_next_commit_id();
            if transaction.is_finished() {
                return Err(TransactionalStoreError::TransactionAlreadyFinished { id });
            }
            transaction.status = TransactionStatus::Committed { commit_id };
        }

        Ok(())
    }

    fn rollback_transaction(&self, transaction: &TransactionContext) -> StoreResult<K, ()> {
        if let Some(id) = transaction.id {
            let mut transaction = if let Some(tx) = self.transaction_catalog.get_mut(&id) {
                tx
            } else {
                return Err(TransactionalStoreError::TransactionNotFound { id });
            };
            if transaction.is_finished() {
                return Err(TransactionalStoreError::TransactionAlreadyFinished { id });
            }
            transaction.status = TransactionStatus::RolledBack;
        }

        Ok(())
    }

    fn rows_iter(&self, collection_name: &str) -> StoreResult<K, RecordIteratorContainer<K, V>> {
        let current_commit_id = self.get_latest_commit_id();
        let collection = self.collection(collection_name)?;
        let transaction_catalog = self.transaction_catalog.clone();
        let iter = RecordIteratorContainer::new(collection, transaction_catalog, current_commit_id);
        Ok(iter)
    }

    fn rows_iter_with_transaction_id(
        &self,
        collection_name: &str,
        transaction_id: u64,
    ) -> StoreResult<K, RecordTransactionIteratorContainer<K, V>> {
        let collection = self.collection(collection_name)?;
        let transaction_catalog = self.transaction_catalog.clone();
        let iter = RecordTransactionIteratorContainer::new(
            collection,
            transaction_catalog,
            transaction_id,
        );
        Ok(iter)
    }
}

pub struct RecordIteratorContainer<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    collection: Arc<DashMap<K, TransactionalRow<V>>>,
    transaction_catalog: Arc<DashMap<u64, TransactionState>>,
    current_commit_id: u64,
}

impl<K, V> RecordIteratorContainer<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    pub fn new(
        collection: Arc<DashMap<K, TransactionalRow<V>>>,
        transaction_catalog: Arc<DashMap<u64, TransactionState>>,
        current_commit_id: u64,
    ) -> Self {
        Self {
            collection,
            transaction_catalog,
            current_commit_id,
        }
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (K, V)> + 'a> {
        let iter = self.collection.iter().flat_map(move |record| {
            let key = record.key().clone();
            let mut found = None;
            let versions = record.row_versions.read();
            for row in versions.rows.iter() {
                let tx = self.transaction_catalog.get(&row.xmin);
                if let Some(TransactionState {
                    status: TransactionStatus::Committed { commit_id },
                }) = tx.as_deref()
                {
                    if commit_id > &self.current_commit_id {
                        continue;
                    }
                    found = Some(row);
                }
            }

            found.and_then(move |row| row.clone().row.map(|r| (key.clone(), r)))
        });
        Box::new(iter)
    }
}

pub struct RecordTransactionIteratorContainer<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    collection: Arc<DashMap<K, TransactionalRow<V>>>,
    transaction_catalog: Arc<DashMap<u64, TransactionState>>,
    transaction_id: u64,
}

impl<K, V> RecordTransactionIteratorContainer<K, V>
where
    K: Eq + std::hash::Hash + Clone + Send + Debug + Display + Sync + 'static,
    V: Clone + Send + Sync + Debug + Display + 'static,
{
    pub fn new(
        collection: Arc<DashMap<K, TransactionalRow<V>>>,
        transaction_catalog: Arc<DashMap<u64, TransactionState>>,
        transaction_id: u64,
    ) -> Self {
        Self {
            collection,
            transaction_catalog,
            transaction_id,
        }
    }

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (K, V)> + 'a> {
        let iter = self.collection.iter().flat_map(move |record| {
            let key = record.key().clone();
            let mut found = None;
            let versions = record.row_versions.read();
            for row in versions.rows.iter() {
                if row.xmin == self.transaction_id {
                    found = Some(row);
                    break;
                } else if self
                    .transaction_catalog
                    .get(&row.xmin)
                    .map(|tx| tx.is_committed())
                    .unwrap_or(true)
                {
                    found = Some(row);
                }
            }

            found.and_then(move |row| row.clone().row.map(|r| (key.clone(), r)))
        });
        Box::new(iter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{error::Error, str::FromStr};
    use test_case::test_case;

    pub enum StoreCommand<K, V>
    where
        V: FromStr,
    {
        Set {
            key: K,
            val: V,
            transaction_id: Option<u64>,
        },
        Delete {
            key: K,
            transaction_id: Option<u64>,
        },
        Get {
            key: K,
            transaction_id: Option<u64>,
        },
        BeginTransaction,
        CommitTransaction(u64),
        RollbackTransaction(u64),
        RowsIter,
        RowsIterWithTransactionId(u64),
    }

    pub fn store_command_handler<K, V>(
        tx_ctxs: &DashMap<u64, TransactionContext>,
        store: &TransactionalStore<K, V>,
        command: StoreCommand<K, V>,
        isolation_level: IsolationLevel,
    ) -> StoreResult<K, String>
    where
        K: Eq + std::hash::Hash + Clone + Send + Sync + Debug + Display + Ord + 'static,
        V: Clone + Send + Sync + Debug + Display + FromStr + 'static,
    {
        match command {
            StoreCommand::Set {
                key,
                val,
                transaction_id,
            } => {
                let mut tx_ctx = if let Some(transaction_id) = transaction_id {
                    tx_ctxs.get_mut(&transaction_id)
                } else {
                    None
                };
                store.set("test", key, val, tx_ctx.as_deref_mut())?;
                Ok(String::new())
            }
            StoreCommand::Delete {
                key,
                transaction_id,
            } => {
                let mut tx_ctx = if let Some(transaction_id) = transaction_id {
                    tx_ctxs.get_mut(&transaction_id)
                } else {
                    None
                };
                store.delete("test", key, tx_ctx.as_deref_mut())?;
                Ok(String::new())
            }
            StoreCommand::Get {
                key,
                transaction_id,
            } => {
                let tx_ctx = if let Some(transaction_id) = transaction_id {
                    tx_ctxs.get(&transaction_id)
                } else {
                    None
                };
                let value = store.get("test", &key, tx_ctx.as_deref());
                Ok(format!("{value:?}"))
            }
            StoreCommand::BeginTransaction => {
                let mut transaction = store.begin_transaction()?;
                transaction.set_isolation_level(isolation_level);
                let tx_id = tx_ctxs.iter().map(|tx| *tx.key()).max().unwrap_or(0) + 1;
                tx_ctxs.insert(tx_id, transaction);
                Ok(format!("{tx_id:?}"))
            }
            StoreCommand::CommitTransaction(transaction_id) => {
                let tx_ctx = tx_ctxs.get(&transaction_id).unwrap();
                store.commit_transaction(&tx_ctx)?;
                Ok(String::new())
            }
            StoreCommand::RollbackTransaction(transaction_id) => {
                let tx_ctx = tx_ctxs.get(&transaction_id).unwrap();
                store.rollback_transaction(&tx_ctx)?;
                Ok(String::new())
            }
            StoreCommand::RowsIter => {
                let mut rows = store
                    .rows_iter("test")
                    .unwrap()
                    .iter()
                    .collect::<Vec<_>>();
                rows.sort_by_key(|(key, _)| key.clone());
                Ok(format!("{rows:?}"))
            }
            StoreCommand::RowsIterWithTransactionId(transaction_id) => {
                let mut rows = store
                    .rows_iter_with_transaction_id("test", transaction_id)
                    .unwrap()
                    .iter()
                    .collect::<Vec<_>>();
                rows.sort_by_key(|(key, _)| key.clone());
                Ok(format!("{rows:?}"))
            }
        }
    }

    impl FromStr for StoreCommand<String, usize> {
        type Err = Box<dyn Error>;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut parts = s.split_whitespace();
            let command = parts.next().ok_or("No command")?;
            match command.to_ascii_lowercase().as_str() {
                "set" => {
                    let key = parts.next().ok_or("No key")?.trim().to_string();
                    let val = parts.next().ok_or("No value")?.parse()?;
                    let transaction_id = parts.next().map(|s| s.parse()).transpose()?;
                    Ok(StoreCommand::Set {
                        key,
                        val,
                        transaction_id,
                    })
                }
                "delete" => {
                    let key = parts.next().ok_or("No key")?.trim().to_string();
                    let transaction_id = parts.next().map(|s| s.parse()).transpose()?;
                    Ok(StoreCommand::Delete {
                        key,
                        transaction_id,
                    })
                }
                "get" => {
                    let key = parts.next().ok_or("No key")?.trim().to_string();
                    let transaction_id = parts.next().map(|s| s.parse()).transpose()?;
                    Ok(StoreCommand::Get {
                        key,
                        transaction_id,
                    })
                }
                "begin" => Ok(StoreCommand::BeginTransaction),
                "commit" => {
                    let transaction_id = parts.next().ok_or("No transaction id")?.parse()?;
                    Ok(StoreCommand::CommitTransaction(transaction_id))
                }
                "rollback" => {
                    let transaction_id = parts.next().ok_or("No transaction id")?.parse()?;
                    Ok(StoreCommand::RollbackTransaction(transaction_id))
                }
                "rows" => {
                    if let Some(transaction_id) = parts.next() {
                        let transaction_id = transaction_id.parse()?;
                        Ok(StoreCommand::RowsIterWithTransactionId(transaction_id))
                    } else {
                        Ok(StoreCommand::RowsIter)
                    }
                }
                _ => Err(format!("Unknown command: {command}"))?,
            }
        }
    }

    #[test_case("set a 1" => ""; "set")]
    #[test_case("get a" => "Ok(None)"; "get")]
    #[test_case("begin" => "1"; "begin_transaction")]
    #[test_case("rows" => "[]"; "rows_iter")]
    #[test_case("rows 0" => "[]"; "rows_iter_with_transaction_id")]
    fn test_store_command_handler(s: &str) -> &str {
        let store = TransactionalStore::new();
        store.create_collection("test").unwrap();
        let command = s.parse::<StoreCommand<String, usize>>().unwrap();
        let tx_ctxs = DashMap::new();
        let res = store_command_handler(&tx_ctxs, &store, command, IsolationLevel::ReadUncommitted)
            .unwrap();

        Box::leak(res.into_boxed_str())
    }

    #[test_case("SET a 1; SET b 2; SET c 3; ROWS"                                                                                               => r#"[("a", 1), ("b", 2), ("c", 3)]"#; "set")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; ROWS"                                                       => r#"[("a", 1), ("b", 2), ("c", 3)]"#; "begin_transaction")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; COMMIT 1; ROWS"                                             => r#"[("a", 4), ("b", 5), ("c", 6)]"#; "commit_transaction")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; ROLLBACK 1; ROWS"                                           => r#"[("a", 1), ("b", 2), ("c", 3)]"#; "rollback_transaction")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; COMMIT 2; ROWS"     => r#"[("a", 7), ("b", 8), ("c", 9)]"#; "commit_transaction_2")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; ROLLBACK 2; ROWS"   => r#"[("a", 1), ("b", 2), ("c", 3)]"#; "rollback_transaction_2")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; ROLLBACK 1; ROWS"   => r#"[("a", 1), ("b", 2), ("c", 3)]"#; "rollback_transaction_1")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; COMMIT 1; ROWS 2"   => r#"[("a", 4), ("b", 5), ("c", 6)]"#; "commit_transaction_1")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; COMMIT 2; COMMIT 1; ROWS" => r#"[("a", 7), ("b", 8), ("c", 9)]"#; "commit_transaction_2_and_1")]
    #[test_case("SET a 1; SET b 2; SET c 3; BEGIN; SET a 4 1; SET b 5 1; SET c 6 1; BEGIN; SET a 7 2; SET b 8 2; SET c 9 2; ROLLBACK 2; COMMIT 1; ROWS" => r#"[("a", 4), ("b", 5), ("c", 6)]"#; "rollback_transaction_2_and_commit_transaction_1")]
    fn test_store_command_handler_with_transaction(s: &str) -> &str {
        let store = TransactionalStore::new();
        store.create_collection("test").unwrap();
        let commands = s.split(';').map(|s| {
            s.trim()
                .parse::<StoreCommand<String, usize>>()
                .unwrap_or_else(|e| panic!("{s} : {e}"))
        });
        let tx_ctxs = DashMap::new();
        for command in commands {
            store_command_handler(&tx_ctxs, &store, command, IsolationLevel::ReadUncommitted)
                .unwrap();
        }

        let res = store_command_handler(
            &tx_ctxs,
            &store,
            StoreCommand::RowsIter,
            IsolationLevel::Serializable,
        )
        .unwrap();
        Box::leak(res.into_boxed_str())
    }

    #[test_case("BEGIN; SET a 4 1; COMMIT 1; COMMIT 1;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 1}))]
    #[test_case("BEGIN; SET a 4 1; COMMIT 1; BEGIN; SET a 4 2; COMMIT 2; COMMIT 2;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 1}))]
    #[test_case("BEGIN; SET a 4 1; ROLLBACK 1;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 1}))]
    #[test_case("BEGIN; SET a 4 1; BEGIN; SET a 4 2; ROLLBACK 2; ROLLBACK 2;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 2}))]
    #[test_case("BEGIN; SET a 4 1; BEGIN; SET a 4 2; COMMIT 2; ROLLBACK 2;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 2}))]
    #[test_case("BEGIN; SET a 4 1; BEGIN; SET a 4 2; ROLLBACK 2; COMMIT 2;" => Err(TransactionalStoreError::TransactionAlreadyFinished { id: 2}))]
    fn test_store_command_handler_with_transaction_fail(s: &str) -> StoreResult<String, ()> {
        let store = TransactionalStore::new();
        store.create_collection("test").unwrap();
        let commands = s.split(';').map(|s| {
            s.trim()
                .parse::<StoreCommand<String, usize>>()
                .unwrap_or_else(|e| panic!("{s} : {e}"))
        });
        let tx_ctxs = DashMap::new();

        for command in commands {
            store_command_handler(&tx_ctxs, &store, command, IsolationLevel::ReadUncommitted)?;
        }

        panic!("Should fail")
    }

    #[test]
    fn entry_transaction() {
        let map = DashMap::new();
        map.insert(1, "a");
        map.insert(2, "b");

        assert_eq!("a", *map.get(&1).unwrap());
        assert_eq!("b", *map.get(&2).unwrap());
        map.entry(2).and_modify(|v| *v = "b1");
        let entry2_2 = map.entry(2).and_modify(|v| *v = "b2");
        drop(entry2_2);

        assert_eq!("b2", *map.get(&2).unwrap());
    }
}
