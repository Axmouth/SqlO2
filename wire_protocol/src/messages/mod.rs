use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    convert::TryInto,
    io::{BufRead, Read},
    ops::Deref,
};

use byteorder::{ByteOrder, NetworkEndian, ReadBytesExt};

static AUTHENTICATION_OK: &[u8] = &['R' as u8, 0, 0, 0, 8, 0, 0, 0, 0];
static READY_FOR_QUERY: &[u8] = &['Z' as u8, 0, 0, 0, 5, 'I' as u8];
static SSL_REQUEST: [u8; 4] = [0x04, 0xd2, 0x16, 0x2f];
static SSL_CLIENT_CERTIFICATE: [u8; 3] = [0x16, 0x2f, 0x04];
static NEGATIVE: &[u8] = &['N' as u8];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Message {
    AuthenticationOk(AuthenticationOk),
    ReadyForQuery(ReadyForQuery),
    SSLRequest(SSLRequest),
}

pub trait SerializableMessage {
    fn serialize(&self) -> Vec<u8>;
}

pub trait DeserializableMessage<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError>
    where
        Self: Sized;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeserializationError {
    InvalidMessage { msg: String },
}

impl From<std::io::Error> for DeserializationError {
    fn from(err: std::io::Error) -> Self {
        DeserializationError::InvalidMessage {
            msg: format!("{}", err),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationOk {}

impl DeserializableMessage<'_> for AuthenticationOk {
    fn deserialize_content(bytes: &'_ [u8]) -> Result<Self, DeserializationError> {
        if bytes == AUTHENTICATION_OK {
            Ok(AuthenticationOk {})
        } else {
            Err(DeserializationError::InvalidMessage {
                msg: "Invalid AuthenticationOk".to_string(),
            })
        }
    }
}

impl SerializableMessage for AuthenticationOk {
    fn serialize(&self) -> Vec<u8> {
        AUTHENTICATION_OK.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationKerberosV5 {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationCleartextPassword {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationMD5Password {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationSCMCredential {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationGSS {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationSSPI {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct AuthenticationGSSContinue {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct BackendKeyData {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Bind<'a> {
    pub portal: Cow<'a, str>,
    pub stmt: Cow<'a, str>,
    pub formats: Vec<i16>,
    pub params: Vec<Cow<'a, str>>,
    pub results: Vec<i16>,
}

impl<'a> DeserializableMessage<'a> for Bind<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes.splitn(3, |c| *c == '\0' as u8);

        if let (Some(portal_bytes), Some(stmt_bytes), Some(mut data_bytes)) = (
            content_split.next(),
            content_split.next(),
            content_split.next(),
        ) {
            let portal = String::from_utf8_lossy(portal_bytes);
            let stmt = String::from_utf8_lossy(stmt_bytes);

            let numformats = data_bytes.read_i16::<NetworkEndian>()?;
            let mut formats = Vec::with_capacity(numformats as usize);
            let mut read_count: i16 = 0;
            while read_count < numformats {
                let format = data_bytes.read_i16::<NetworkEndian>()?;
                formats.push(format);
                read_count += 1;
            }
            if formats.len() != numformats as usize {
                return Err(DeserializationError::InvalidMessage {
                    msg: format!(
                        "Invalid Bind message: expected {} formats, got {}",
                        numformats,
                        formats.len()
                    ),
                });
            }

            let numparams = data_bytes.read_i16::<NetworkEndian>()?;
            let mut params = Vec::with_capacity(numparams as usize);
            let mut read_count: i16 = 0;
            while read_count < numparams {
                let param_len = data_bytes.read_i32::<NetworkEndian>()?;
                let mut param = Vec::with_capacity(param_len as usize);
                data_bytes.read_exact(&mut param)?;
                let param_str = Cow::Owned(String::from_utf8_lossy(&param).to_string());
                params.push(param_str);
                read_count += 1;
            }
            if params.len() != numparams as usize {
                return Err(DeserializationError::InvalidMessage {
                    msg: format!(
                        "Invalid Bind message: expected {} params, got {}",
                        numparams,
                        params.len()
                    ),
                });
            }

            let numresults = data_bytes.read_i16::<NetworkEndian>()?;
            let mut results = Vec::with_capacity(numresults as usize);
            let mut read_count: i16 = 0;
            while read_count < numresults {
                let result = data_bytes.read_i16::<NetworkEndian>()?;
                results.push(result);
                read_count += 1;
            }
            if results.len() != numresults as usize {
                return Err(DeserializationError::InvalidMessage {
                    msg: format!(
                        "Invalid Bind message: expected {} formats, got {}",
                        numresults,
                        results.len()
                    ),
                });
            }

            return Ok(Bind {
                portal,
                stmt,
                formats,
                params,
                results,
            });
        }

        Err(DeserializationError::InvalidMessage {
            msg: "Invalid Bind".to_string(),
        })
    }
}

impl<'a> SerializableMessage for Bind<'_> {
    fn serialize(&self) -> Vec<u8> {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct BindComplete {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CancelRequest {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Close {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CloseComplete {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CommandComplete<'a> {
    tag: Cow<'a, str>,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyData {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyDone {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyFail {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyInResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyOutResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct CopyBothResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct DataRow {
    len: i32,
    numfields: i16,
    fieldlen: i32,
    data: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DescribeType {
    Portal,
    PreparedStatement,
}

impl Default for DescribeType {
    fn default() -> DescribeType {
        DescribeType::Portal
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Describe<'a> {
    name: Cow<'a, str>,
    typ: DescribeType,
}

impl<'a> DeserializableMessage<'a> for Describe<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        if let Some((typ_byte, name_bytes)) = bytes.split_first() {
            let name = String::from_utf8_lossy(name_bytes);
            let typ = match *typ_byte as char {
                'P' => DescribeType::Portal,
                'S' => DescribeType::PreparedStatement,
                _ => {
                    return Err(DeserializationError::InvalidMessage {
                        msg: "Invalid Describe type".to_string(),
                    })
                }
            };
            return Ok(Describe { name, typ });
        }

        Err(DeserializationError::InvalidMessage {
            msg: "Invalid Describe".to_string(),
        })
    }
}

impl<'a> SerializableMessage for Describe<'_> {
    fn serialize(&self) -> Vec<u8> {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct EmptyQueryResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ErrorResponse<'a> {
    pub errors: Vec<(char, Cow<'a, str>)>,
}

impl<'a> DeserializableMessage<'a> for ErrorResponse<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes.split(|c| *c == '\0' as u8);

        let mut errors = vec![];
        while let Some(e) = content_split.next() {
            if let Some((c, err)) = e.split_first() {
                errors.push((*c as char, String::from_utf8_lossy(err)));
            } else {
                return Err(DeserializationError::InvalidMessage {
                    msg: "Invalid ErrorResponse, failed to deserialize errors".to_string(),
                });
            }
        }

        Ok(ErrorResponse { errors })
    }
}

impl<'a> SerializableMessage for ErrorResponse<'_> {
    fn serialize(&self) -> Vec<u8> {
        let mut content = vec!['E' as u8, 0, 0, 0, 0];
        for (c, s) in &self.errors {
            content.push(*c as u8);
            content.append(&mut s.as_bytes().to_vec());
            content.push('\0' as u8);
        }
        content.splice(1..4, (content.len() - 1).to_be_bytes());
        content
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Execute<'a> {
    portal: Cow<'a, str>,
    row_limit: i32,
}

impl<'a> DeserializableMessage<'a> for Execute<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes.splitn(2, |c| *c == '\0' as u8);

        if let (Some(portal_bytes), Some(mut row_limit_bytes)) =
            (content_split.next(), content_split.next())
        {
            let portal = String::from_utf8_lossy(portal_bytes);
            if row_limit_bytes.len() != 4 {
                return Err(DeserializationError::InvalidMessage {
                    msg: "Invalid Execute, invalid row limit".to_string(),
                });
            }
            let row_limit = row_limit_bytes.read_i32::<NetworkEndian>()?;

            return Ok(Execute { portal, row_limit });
        }

        Err(DeserializationError::InvalidMessage {
            msg: "Invalid Execute".to_string(),
        })
    }
}

impl<'a> SerializableMessage for Execute<'_> {
    fn serialize(&self) -> Vec<u8> {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Flush {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct FunctionCall {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct FunctionCallResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct NegotiateVersion {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct NoData {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct NotificationResponse {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ParameterDescription {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ParameterStatus<'a> {
    pub name: Cow<'a, str>,
    pub value: Cow<'a, str>,
}

impl<'a> DeserializableMessage<'a> for ParameterStatus<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes
            .split(|c| *c == '\0' as u8)
            .map(|v| String::from_utf8_lossy(v));

        if let (Some(name), Some(value), None) = (
            content_split.next(),
            content_split.next(),
            content_split.next(),
        ) {
            Ok(ParameterStatus { name, value })
        } else {
            Err(DeserializationError::InvalidMessage {
                msg: "Invalid ParameterStatus".to_string(),
            })
        }
    }
}

impl<'a> SerializableMessage for ParameterStatus<'a> {
    fn serialize(&self) -> Vec<u8> {
        let name_bytes = self.name.as_bytes();
        let value_bytes = self.value.as_bytes();
        let mut content = Vec::with_capacity(name_bytes.len() + value_bytes.len() + 2);
        content.append(&mut name_bytes.to_vec());
        content.push('\0' as u8);
        content.append(&mut value_bytes.to_vec());
        content.push('\0' as u8);
        content
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Parse<'a> {
    stmt: Cow<'a, str>,
    query: Cow<'a, str>,
    paramoid: Vec<i32>,
}

impl<'a> DeserializableMessage<'a> for Parse<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes.splitn(3, |c| *c == '\0' as u8);

        if let (Some(stmt_bytes), Some(query_bytes), Some(params_bytes)) = (
            content_split.next(),
            content_split.next(),
            content_split.next(),
        ) {
            let stmt = String::from_utf8_lossy(stmt_bytes);
            let query = String::from_utf8_lossy(query_bytes);

            let mut params_iter = params_bytes.iter();
            let param_len: usize =
                if let (Some(b1_byte), Some(b2_byte)) = (params_iter.next(), params_iter.next()) {
                    let b_bytes = [*b1_byte, *b2_byte];

                    let n = NetworkEndian::read_i16(&b_bytes);
                    if let Ok(n) = n.try_into() {
                        n
                    } else {
                        return Err(DeserializationError::InvalidMessage {
                            msg: "Failed to convert length".to_string(),
                        });
                    }
                } else {
                    return Err(DeserializationError::InvalidMessage {
                        msg: "Failed to get length bytes".to_string(),
                    });
                };

            let mut paramoid = Vec::with_capacity(param_len);
            while let (Some(b1_byte), Some(b2_byte), Some(b3_byte), Some(b4_byte)) = (
                params_iter.next(),
                params_iter.next(),
                params_iter.next(),
                params_iter.next(),
            ) {
                let b_bytes = [*b1_byte, *b2_byte, *b3_byte, *b4_byte];
                paramoid.push(NetworkEndian::read_i32(&b_bytes));
            }

            if param_len != paramoid.len() {
                return Err(DeserializationError::InvalidMessage {
                    msg: "Invalid parameter count".to_string(),
                });
            }

            Ok(Parse {
                stmt,
                query,
                paramoid,
            })
        } else {
            Err(DeserializationError::InvalidMessage {
                msg: "Invalid Parse".to_string(),
            })
        }
    }
}

impl<'a> SerializableMessage for Parse<'a> {
    fn serialize(&self) -> Vec<u8> {
        // TODO
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ParseComplete {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct PasswordMessage {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct PortalSuspended {}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Query<'a> {
    q: Cow<'a, str>,
}

impl<'a> DeserializableMessage<'a> for Query<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let q = String::from_utf8_lossy(bytes);
        Ok(Query { q })
    }
}

impl<'a> SerializableMessage for Query<'a> {
    fn serialize(&self) -> Vec<u8> {
        let mut content = Vec::with_capacity(self.q.len() + 5);
        content.push('Q' as u8);

        content.append(&mut self.q.len().to_be_bytes().to_vec());
        content.append(&mut self.q.as_bytes().to_vec());
        READY_FOR_QUERY.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ReadyForQuery {}

impl DeserializableMessage<'_> for ReadyForQuery {
    fn deserialize_content(bytes: &[u8]) -> Result<Self, DeserializationError> {
        if bytes == READY_FOR_QUERY {
            Ok(ReadyForQuery {})
        } else {
            Err(DeserializationError::InvalidMessage {
                msg: "Invalid ReadyForQuery".to_string(),
            })
        }
    }
}

impl SerializableMessage for ReadyForQuery {
    fn serialize(&self) -> Vec<u8> {
        READY_FOR_QUERY.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct RowDescription<'a> {
    numfields: i16,
    col: Cow<'a, str>,
    tableoid: i32,
    colno: i16,
    typeoid: i32,
    typelen: i16,
    typemod: i32,
    format: i16,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct SSLRequest {}

impl DeserializableMessage<'_> for SSLRequest {
    fn deserialize_content(bytes: &[u8]) -> Result<Self, DeserializationError> {
        if bytes == SSL_REQUEST {
            Ok(SSLRequest {})
        } else {
            Err(DeserializationError::InvalidMessage {
                msg: "Invalid SSLRequest".to_string(),
            })
        }
    }
}

impl SerializableMessage for SSLRequest {
    fn serialize(&self) -> Vec<u8> {
        SSL_REQUEST.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct StartupMessage<'a> {
    pub options: HashMap<Cow<'a, str>, Cow<'a, str>>,
}

impl<'a> DeserializableMessage<'a> for StartupMessage<'a> {
    fn deserialize_content(bytes: &'a [u8]) -> Result<Self, DeserializationError> {
        let mut content_split = bytes.split(|c| *c == '\0' as u8).into_iter();

        let mut protocol = Vec::with_capacity(2);
        match content_split.next() {
            Some(ref mut b) => protocol.append(&mut b.to_vec()),
            _ => {}
        };
        match content_split.next() {
            Some(ref mut b) => protocol.append(&mut b.to_vec()),
            _ => {}
        };
        let mut options_list = content_split
            .filter(|v| !v.is_empty())
            .map(|v| String::from_utf8_lossy(v));
        let mut options = HashMap::new();
        loop {
            let (k, v) = (options_list.next(), options_list.next());
            if let (Some(k), Some(v)) = (k, v) {
                options.insert(k, v);
            } else {
                break;
            }
        }
        Ok(StartupMessage { options })
    }
}

impl SerializableMessage for StartupMessage<'_> {
    fn serialize(&self) -> Vec<u8> {
        SSL_REQUEST.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Sync {}

impl DeserializableMessage<'_> for Sync {
    fn deserialize_content(bytes: &'_ [u8]) -> Result<Self, DeserializationError> {
        if bytes.len() == 0 {
            return Ok(Sync {});
        }

        Err(DeserializationError::InvalidMessage {
            msg: "Invalid Sync".to_string(),
        })
    }
}

impl SerializableMessage for Sync {
    fn serialize(&self) -> Vec<u8> {
        SSL_REQUEST.to_vec()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct Terminate {}
