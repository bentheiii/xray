use std::io::Write;

pub struct MemoryWriter<W> {
    pub memory: Vec<u8>,
    inner: W,
}

impl<W: Write> Write for MemoryWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.memory.write_all(buf)?;
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.memory.flush()?;
        self.inner.flush()
    }
}

impl<W> MemoryWriter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            memory: Vec::new(),
            inner,
        }
    }
}
