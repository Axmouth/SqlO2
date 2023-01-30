use std::sync::atomic::AtomicI64;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use dashmap::DashMap;
use instant::Instant;

fn store_set_benchmark(c: &mut Criterion) {
    use transactional_store::{TransactionalStore, TransactionalStoreExt};
    let store = TransactionalStore::<isize, isize>::new();
    // let map = DashMap::new();
    let at = AtomicI64::new(0);
    // Bench here
    c.bench_function("store_set", |b| {
        b.iter(|| {
                let k = rand::random();
                store.set(black_box(k), k, None);
                // map.entry(i).and_modify(|v| *v = i).or_insert(i);
        })
    });
}

criterion_group!(
    benches,
    store_set_benchmark,
);
criterion_main!(benches);