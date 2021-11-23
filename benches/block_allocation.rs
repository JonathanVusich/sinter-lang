use flux_lang::class::class::Class;
use flux_lang::gc::block::Block;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn small_class_allocation(c: &mut Criterion) {

    let small_class = Class::new(64);

    c.bench_function("Small class allocation", |b| b.iter(|| {
        let mut block = black_box(Block::boxed().unwrap());
        let mut heap_ptr = block.allocate_small(&small_class);
        while heap_ptr.is_some() {
            heap_ptr = block.allocate_small(&small_class);
        }
        heap_ptr
    }));
}

fn large_class_allocation(c: &mut Criterion) {

    let small_class = Class::new(256);

    c.bench_function("Large class allocation", |b| b.iter(|| {
        let mut block = black_box(Block::boxed().unwrap());
        let mut heap_ptr = block.allocate_small(&small_class);
        while heap_ptr.is_some() {
            heap_ptr = block.allocate_small(&small_class);
        }
        heap_ptr
    }));
}

criterion_group!(block_allocation, small_class_allocation, large_class_allocation);
criterion_main!(block_allocation);