#[derive(Debug, Clone)]
pub struct MemorySection<const N: usize> {
    data: [u8; N],
}

impl<const N: usize> MemorySection<N> {
    pub fn new() -> Self {
        Self { data: [0u8; N] }
    }

    pub fn page_size() -> usize {
        N
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

impl<const N: usize> Default for MemorySection<N> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default, Clone)]
pub struct MemoryStore<const N: usize> {
    sections: Vec<MemorySection<N>>,
}

impl<const N: usize> MemoryStore<N> {
    pub fn new() -> Self {
        Self { sections: vec![] }
    }

    pub fn len(&self) -> usize {
        self.sections.len()
    }

    pub fn is_empty(&self) -> bool {
        self.sections.is_empty()
    }

    pub fn section(&self, index: usize) -> Option<&MemorySection<N>> {
        self.sections.get(index)
    }
}
