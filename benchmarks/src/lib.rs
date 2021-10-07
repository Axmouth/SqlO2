#[cfg(test)]
mod tests {

    use alloc_counter::{count_alloc, AllocCounterSystem};

    #[global_allocator]
    static A: AllocCounterSystem = AllocCounterSystem;

    #[test]
    fn count_alloc_works() {
        let ((allocations, reallocations, deallocations), _) = count_alloc(|| {
            let a = "dfdddfdf";
            let _ = a.as_bytes().iter().copied().collect::<Vec<u8>>();
        });
        println!("Allocations : {}", allocations);
        println!("Rellocations : {}", reallocations);
        println!("Dellocations : {}", deallocations);

        assert_eq!(allocations, 1);
        assert_eq!(reallocations, 0);
        assert_eq!(deallocations, 1);
    }
}
