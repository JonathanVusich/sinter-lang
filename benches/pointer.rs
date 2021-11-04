use criterion::{black_box, Criterion, criterion_group, criterion_main};

use flux_lang::pointers::pointer::Pointer;

fn dereference(c: &mut Criterion) {

    let value: i32 = 1234567;
    let ptr = Pointer::new(&value);

    c.bench_function("Dereference", |b| b.iter(|| {
        black_box(*ptr)
    }));
}

criterion_group!(pointer, dereference);
criterion_main!(pointer);