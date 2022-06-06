use flux_lang::class::class_builder::ClassBuilder;
use flux_lang::gc::block::BLOCK_SIZE;

use criterion::{criterion_group, criterion_main, Criterion};
use flux_lang::heap::region::Region;
use flux_lang::pool::internal_string::InternalString;

fn small_class_allocation(c: &mut Criterion) {
    let small_class = ClassBuilder::new()
        .set_size(64)
        .build(|_val| InternalString(0));
    let region = Region::new(BLOCK_SIZE).unwrap();
    let mut block = region.allocate_block().unwrap();

    c.bench_function("Small class allocation", |b| {
        b.iter(|| loop {
            let heap_ptr = block.allocate(&small_class);
            if !heap_ptr.is_some() {
                break;
            }
        })
    });
}

fn large_class_allocation(c: &mut Criterion) {
    let small_class = ClassBuilder::new()
        .set_size(256)
        .build(|_val| InternalString(0));
    let region = Region::new(BLOCK_SIZE).unwrap();
    let mut block = region.allocate_block().unwrap();

    c.bench_function("Large class allocation", |b| {
        b.iter(|| {
            let mut heap_ptr = block.allocate(&small_class);
            while heap_ptr.is_some() {
                heap_ptr = block.allocate(&small_class);
            }
            heap_ptr
        })
    });
}

criterion_group!(
    block_allocation,
    small_class_allocation,
    large_class_allocation
);
criterion_main!(block_allocation);
