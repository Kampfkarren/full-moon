use criterion::{criterion_group, criterion_main, Criterion};
use full_moon::node::Node;

const T_SOURCE: &str = include_str!("./t.lua");

fn parse(criterion: &mut Criterion) {
    criterion.bench_function("get ast from parsed t", move |b| {
        b.iter(|| full_moon::parse(T_SOURCE))
    });
}

fn range(criterion: &mut Criterion) {
    let ast = full_moon::parse(T_SOURCE).unwrap();

    criterion.bench_function("get range of ast of t", move |b| {
        b.iter(|| ast.nodes().range())
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = parse, range
}

criterion_main!(benches);
