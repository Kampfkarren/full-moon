use criterion::{criterion_group, criterion_main, Criterion};
use full_moon::node::Node;

const DATE_SOURCE: &str = include_str!("./date.lua");

fn parse(criterion: &mut Criterion) {
    criterion.bench_function("get ast from parsed date", move |b| {
        b.iter(|| full_moon::parse(DATE_SOURCE))
    });
}

fn range(criterion: &mut Criterion) {
    let ast = full_moon::parse(DATE_SOURCE).unwrap();

    criterion.bench_function("get range of ast of date", move |b| {
        b.iter(|| ast.nodes().range())
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(20);
    targets = parse, range
}

criterion_main!(benches);
