use std::io::{Error, ErrorKind, Write};
use std::str::from_utf8;

/// A dummy writer that can write to a captured stdout
#[derive(Debug)]
pub struct CaptureWriter;

impl Write for CaptureWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        print!(
            "{}",
            from_utf8(buf).map_err(|e| Error::new(ErrorKind::Other, e))?
        );
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
