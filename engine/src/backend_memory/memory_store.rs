#[derive(Debug, Default, Clone)]
pub struct MemorySection {
    data: Vec<u8>,
}

impl MemorySection {
    pub fn new(size: usize) -> Self {
        Self {
            data: vec![0; size],
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn bytes(&self) -> &[u8] {
        &self.data
    }

    pub fn bytes_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }
}

#[derive(Debug, Default, Clone)]
pub struct MemoryStore {
    sections: Vec<MemorySection>,
}

impl MemoryStore {
    pub fn new() -> Self {
        Self {
            sections: vec![],
        }
    }

    pub fn len(&self) -> usize {
        self.sections.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sections.is_empty()
    }
}