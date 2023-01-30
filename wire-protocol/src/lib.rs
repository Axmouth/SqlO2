extern crate log;

pub mod messages;
use std::{
    io::{BufReader, BufWriter, Read, Write},
    mem::size_of,
    net::TcpStream,
};

use byteorder::{NetworkEndian, ReadBytesExt};

use crate::messages::{DeserializableMessage, SSLRequest, StartupMessage};

pub fn read_startup_message(
    stream_reader: &mut BufReader<&TcpStream>,
    stream_writer: &mut BufWriter<&TcpStream>,
    content: &mut Vec<u8>,
) -> std::io::Result<()> {
    log::debug!("Startup Message");
    let len = stream_reader.read_i32::<NetworkEndian>()?;
    log::debug!("Startup Content length: {:?}", len);
    content.resize(len as usize - size_of::<i32>(), 0);
    stream_reader.read_exact(content.as_mut_slice())?;

    if SSLRequest::deserialize_content(content.as_slice()).is_ok() {
        log::debug!("SSL Request");
        stream_writer.write_all(&[b'N']).unwrap();
        stream_writer.flush().unwrap();
        return read_startup_message(stream_reader, stream_writer, content);
    }

    if let Ok(msg) = StartupMessage::deserialize_content(content.as_slice()) {
        log::debug!("Received: {:?}", msg);
    }
    Ok(())
}

pub fn read_message(
    stream_reader: &mut BufReader<&TcpStream>,
    content: &mut Vec<u8>,
) -> std::io::Result<char> {
    let msg_typ = stream_reader.read_u8()? as char;
    log::debug!("Received message type: {:?}", msg_typ);
    let len = stream_reader.read_i32::<NetworkEndian>()?;
    log::debug!("Content length: {:?}", len);
    content.resize(len as usize - size_of::<i32>(), 0);
    stream_reader.read_exact(content.as_mut_slice())?;
    log::debug!("Received: {:?}", content);
    Ok(msg_typ)
}
