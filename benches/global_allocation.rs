use flux_lang::gc::global_allocator::GlobalAllocator;

use criterion::*;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::atomic::Ordering::{SeqCst};
use std::thread;
use std::thread::JoinHandle;
use flux_lang::gc::block::{BLOCK_SIZE, Block};
use std::collections::LinkedList;

fn global_allocator(c: &mut Criterion) {


    c.bench_function("Global allocator", |b| b.iter(|| {
        let global_alloc = Arc::new(GlobalAllocator::new(10_000));
        let start = Arc::new(AtomicBool::new(false));

        let mut thread_handles: Vec<JoinHandle<()>> = vec![];
        for _ in 0..32 {
            let allocator = global_alloc.clone();
            let starter = start.clone();
            let join_handle = thread::spawn(move || {

                let mut blocks: LinkedList<Box<Block>> = LinkedList::new();

                'spin: loop {
                    if starter.load(SeqCst) {
                        break 'spin;
                    }
                }

                'inner: loop {
                    match allocator.allocate_block() {
                        Ok(block) => blocks.push_back(block),
                        Err(_) => break 'inner
                    }
                }
            });
            thread_handles.push(join_handle);
        }

        start.store(true, Ordering::SeqCst);
        for handle in thread_handles {
            handle.join().unwrap();
        }

        assert_eq!(global_alloc.get_max_size(), 10_000 * BLOCK_SIZE);
    }));
}

criterion_group!{
    name = global_allocation;
    config = Criterion::default().sample_size(10);
    targets = global_allocator
}
criterion_main!(global_allocation);