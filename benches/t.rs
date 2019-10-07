use criterion::{black_box, criterion_group, criterion_main, Criterion};

const T_SOURCE: &str = include_str!("./t.lua");

fn criterion_benchmark(criterion: &mut Criterion) {
    criterion.bench_function("parse t", |b| {
        b.iter(|| full_moon::parse(black_box(T_SOURCE)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = criterion_benchmark
}

criterion_main!(benches);
