use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};

fn cmp<T : PartialEq>(a : &[T], b : &[T]) -> bool {
    if(a.len() != b.len()){
        return false
    } else {
        for i in (0.. a.len()){
            if(a[i] != b[i]){
                return false
            }
        }
    }
    return true
}


fn custom(c: &mut Criterion) {
    let a = [1;1_000_000].to_vec();
    let d = [1;1_000_000].to_vec();
    c.bench_function("custom", |b| b.iter(|| assert!(cmp(a.as_slice(),&d.as_slice()))));
    c.bench_function("builtin", |b| b.iter(|| assert!(a.as_slice() == d.as_slice())));
}

// fn builtIn(c: &mut Criterion) {
//     c.bench_function("custom", |b| b.iter(|| fibonacci(black_box(20))));
// }
// fn bench_equals(c: &mut Criterion) {
//     let mut group = c.benchmark_group("cmp");
//
//
//     let a = [1;1_000];
//     let b = [1;1_000];
//     group.bench_with_input(BenchmarkId::new("custom", "custom"), &a,
//                            |bencher, &a| bencher.iter(|| assert!(cmp(&a,&b))));
//     group.bench_with_input(BenchmarkId::new("builtin", "builtin"), &a,
//                            |bencher, &a| bencher.iter(|| assert!(&a == &b)));
//     group.finish();
// }

criterion_group!(benches, custom);
criterion_main!(benches);