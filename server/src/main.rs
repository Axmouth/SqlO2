use std::{
    io::{BufReader, BufWriter, Write},
    net::TcpListener,
    // thread,
};

use sqlo2::backend_memory::MemoryBackend;
use sqlo2_wire_protocol::{
    messages::{
        AuthenticationOk, Bind, Describe, DeserializableMessage, Execute, Parse, Query,
        ReadyForQuery, SerializableMessage, Sync,
    },
    read_message, read_startup_message,
};

fn main() {
    let _ = dotenv::dotenv().is_ok();
    env_logger::init();

    log::info!("SqlO2 server starting..");

    let mut db = MemoryBackend::new();
    let port = 9876;
    let listener = TcpListener::bind(("127.0.0.1", port)).unwrap();
    let mut last_used_session_id = 1;

    log::info!("SqlO2 server operational!");

    for stream in listener.incoming() {
        let stream = stream.unwrap();
        last_used_session_id += 1;
        let sessid = last_used_session_id;
        // thread::spawn(move || {
        log::info!(
            "New connection from {}, session: {}",
            stream.peer_addr().unwrap(),
            sessid
        );
        let mut sockreader = BufReader::with_capacity(16384, &stream);
        let mut sockwriter = BufWriter::with_capacity(16384, &stream);

        let mut content: Vec<u8> = Vec::new();
        read_startup_message(&mut sockreader, &mut sockwriter, &mut content).unwrap();

        let out = AuthenticationOk::default().serialize();
        sockwriter.write_all(&out).unwrap();

        sockwriter
            .write_all(&ReadyForQuery::default().serialize())
            .unwrap();
        sockwriter.flush().unwrap();

        loop {
            let msg_typ = read_message(&mut sockreader, &mut content).unwrap();
            if msg_typ == 'P' {
                let parse = Parse::deserialize_content(&content).unwrap();
                log::debug!("Parse: {:?}", parse);
                let parsed = db.parse(&parse.query);
                if let Ok(ast) = &parsed {
                    log::debug!("Parsed: {:?}", ast);
                } else if let Err(err) = &parsed {
                    log::error!("{}", err);
                }
            } else if msg_typ == 'B' {
                let bind = Bind::deserialize_content(&content).unwrap();
                log::debug!("Bind: {:?}", bind);
            } else if msg_typ == 'D' {
                let describe = Describe::deserialize_content(&content).unwrap();
                log::debug!("Describe: {:?}", describe);
            } else if msg_typ == 'E' {
                let execute = Execute::deserialize_content(&content).unwrap();
                log::debug!("Execute: {:?}", execute);
            } else if msg_typ == 'S' {
                let sync = Sync::deserialize_content(&content).unwrap();
                log::debug!("Sync: {:?}", sync);
            } else if msg_typ == 'Q' {
                let query = Query::deserialize_content(&content).unwrap();
                let _ = db.eval_query(&query.q).is_ok();
                log::debug!("Query: {:?}", query);
            }
        }
        // });
    }
}
