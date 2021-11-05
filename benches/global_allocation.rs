use flux_lang::gc::global_allocator::GlobalAllocator;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::atomic::Ordering::Relaxed;
use std::thread;
use std::thread::JoinHandle;

fn global_allocator(c: &mut Criterion) {
    c.bench_function("Global allocator", |b| b.iter(|| {

        let global_alloc = Arc::new(GlobalAllocator::new(10_000_000_000));
        let start = Arc::new(AtomicBool::new(false));

        let thread_handles = (0..32).into_iter().map(|_| {
            let allocator = global_alloc.clone();
            let starter = start.clone();
            thread::spawn(move || {
                'spin: loop {
                    if starter.load(Relaxed) {
                        break 'spin;
                    }
                }

                'inner: loop {
                    if black_box(allocator.allocate_block()).is_err() {
                        break 'inner;
                    };
                }
            })
        }).collect::<Vec<JoinHandle<()>>>();
        start.store(true, Ordering::SeqCst);
        black_box(thread_handles);
    }));
}

criterion_group!(global_allocation, global_allocator);
criterion_main!(global_allocation);