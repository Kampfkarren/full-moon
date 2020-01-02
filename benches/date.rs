use criterion::{black_box, criterion_group, criterion_main, Criterion};
use full_moon::node::Node;

const DATE_SOURCE: &str = include_str!("./date.lua");

fn tokenize(criterion: &mut Criterion) {
    criterion.bench_function("tokenize date", |b| {
        b.iter(|| full_moon::tokenizer::tokens(black_box(DATE_SOURCE)))
    });
}

fn parse(criterion: &mut Criterion) {
    let tokens = full_moon::tokenizer::tokens(DATE_SOURCE).unwrap();

    criterion.bench_function("get ast from parsed date", move |b| {
        b.iter(|| full_moon::ast::Ast::from_tokens(black_box(tokens.clone())))
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
    targets = tokenize, parse, range
}

criterion_main!(benches);
