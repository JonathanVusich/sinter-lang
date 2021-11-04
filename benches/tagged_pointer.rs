use criterion::{black_box, Criterion, criterion_group, criterion_main};

use flux_lang::pointers::tagged_pointer;
use flux_lang::pointers::tagged_pointer::TaggedPointer;

fn set_bit(c: &mut Criterion) {

    let value: i128 = 1234567;
    let mut ptr = TaggedPointer::new(&value);

    c.bench_function("Set bit", |b| b.iter(|| {
        ptr.set_bit(black_box(1));
    }));
}

fn get_bit(c: &mut Criterion) {

    let value: i128 = 1234567;
    let ptr = TaggedPointer::new(&value);

    c.bench_function("Get bit", |b| b.iter(|| {
        ptr.is_bit_set(black_box(1));
    }));
}

fn clear_bit(c: &mut Criterion) {

    let value: i128 = 1234567;
    let mut ptr = TaggedPointer::new(&value);

    c.bench_function("Clear bit", |b| b.iter(|| {
        ptr.clear_bit(black_box(1));
    }));
}

fn get_mark_word(c: &mut Criterion) {
    let value: i128 = 1234567;
    let ptr = TaggedPointer::new(&value);

    c.bench_function("Get mark word", |b| b.iter(|| {
        black_box(ptr.get_mark_word());
    }));
}

fn set_mark_word(c: &mut Criterion) {
    let value: i128 = 1234567;
    let mut ptr = TaggedPointer::new(&value);

    c.bench_function("Set mark word", |b| b.iter(|| {
        ptr.set_mark_word(black_box(12))
    }));
}

fn default_constructor(c: &mut Criterion) {
    let value: i128 = 1234567;

    c.bench_function("Default constructor", |b| b.iter(|| {
        black_box(TaggedPointer::new(black_box(&value)));
    }));
}

fn constructor_with_mark_word(c: &mut Criterion) {
    let value: i128 = 1234567;
    let mark_word: u8 = 123;

    c.bench_function("Constructor with mark word", |b| b.iter(|| {
        black_box(TaggedPointer::new_with_mark_word(black_box(&value), black_box(mark_word)));
    }));
}

fn dereference(c: &mut Criterion) {
    let value: i32 = 1234567;
    let mut tagged_pointer = TaggedPointer::new(&value);
    tagged_pointer.set_mark_word(23);

    c.bench_function("Dereference", |b| b.iter(|| {
        black_box(*tagged_pointer)
    }));
}

criterion_group!(tagged_pointer, set_bit, get_bit, clear_bit, get_mark_word, set_mark_word,
    default_constructor, constructor_with_mark_word, dereference);
criterion_main!(tagged_pointer);