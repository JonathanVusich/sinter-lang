use std::sync::Arc;
use std::time::Instant;
use concourse::Concourse;
use criterion::{criterion_group, criterion_main, Criterion};

fn contended_writes(c: &mut Criterion) {
    struct Point {
        x: u64,
        y: u64,
    }

    impl Point {
        pub fn new(x: u64, y: u64) -> Self {
            Self {
                x, y
            }
        }
    }

    c.bench_function("Contended writes", |b| {
        b.iter_custom(|iters| {
            let concourse = Arc::new(Concourse::<Point>::new());

            let mut join_handles = Vec::new();
            for _ in 0..8 {
                let conc_ref = concourse.clone();
                let thread = std::thread::spawn(move || {
                    for i in 0..iters {
                        conc_ref.alloc(Point::new(i, i));
                    }
                });
                join_handles.push(thread);
            }
            let start = Instant::now();

            join_handles.into_iter().for_each(|handle| handle.join().unwrap());

            start.elapsed()
        })
    });
}

criterion_group!(
    contended_allocation,
    contended_writes,
);

criterion_main!(contended_allocation);
