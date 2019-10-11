use criterion::{black_box, criterion_group, criterion_main, Criterion};
use full_moon::node::Node;

const T_SOURCE: &str = include_str!("./t.lua");

fn tokenize(criterion: &mut Criterion) {
    criterion.bench_function("tokenize t", |b| {
        b.iter(|| full_moon::tokenizer::tokens(black_box(T_SOURCE)))
    });
}

fn parse(criterion: &mut Criterion) {
    let tokens = full_moon::tokenizer::tokens(T_SOURCE).unwrap();

    criterion.bench_function("get ast from parsed t", move |b| {
        b.iter(|| full_moon::ast::Ast::from_tokens(black_box(tokens.clone())))
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
    targets = tokenize, parse, range
}

criterion_main!(benches);
