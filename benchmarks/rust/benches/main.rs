mod data;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use serde::{Deserialize, Serialize};

use crate::data::{Author, Book, ReflParsable, ReflSerialisable};

fn serde_serialise<T: Serialize>(obj: &T) -> String {
    serde_json::to_string(obj).unwrap()
}

fn serde_parse<T>(s: &String) -> T
where
    T: for<'a> Deserialize<'a>,
{
    serde_json::from_str::<T>(s.as_str()).unwrap()
}

fn refl_serialise<T>(obj: &T) -> String
where
    T: ReflSerialisable<Vec<u8>>,
{
    let mut writer = Vec::with_capacity(128);

    obj.serialise(&mut writer);
    unsafe { String::from_utf8_unchecked(writer) }
}

fn refl_parse<T>(s: &String) -> ()
where
    T: ReflParsable<T>,
{
    T::parse(s.as_str()).unwrap();
}

pub fn parse_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse");
    let book = Book {
        title: "Hello World".to_owned(),
        release_year: 2018,
        page_count: 124,
        rating: 0.8,
    };
    let obj = refl_serialise(&book);

    group.bench_with_input(BenchmarkId::new("object/refl", 0), &obj, |b, obj_| {
        b.iter_with_large_drop(|| refl_parse::<Book>(obj_));
    });
    group.bench_with_input(BenchmarkId::new("object/serde", 0), &obj, |b, obj_| {
        b.iter_with_large_drop(|| serde_parse::<Book>(obj_));
    });

    for i in vec![1, 10, 50, 100].iter() {
        let mut author = Author {
            name: "John Doe".to_owned(),
            age: 32,
            active: true,
            books: Vec::new(),
        };
        for _ in 0..(i.clone()) {
            author.books.push(book.clone());
        }
        let bs = refl_serialise(&author);
        group.bench_with_input(BenchmarkId::new("refl", i), &bs, |b, i_| {
            b.iter_with_large_drop(|| refl_parse::<Author>(i_))
        });

        group.bench_with_input(BenchmarkId::new("serde", i), &bs, |b, i_| {
            b.iter_with_large_drop(|| serde_parse::<Author>(i_))
        });
    }
    group.finish();
}

pub fn serialise_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("serialise");
    let obj = Book {
        title: "Hello World".to_owned(),
        release_year: 2018,
        page_count: 124,
        rating: 0.8,
    };

    group.bench_with_input(BenchmarkId::new("object/refl", 0), &obj, |b, obj_| {
        b.iter_with_large_drop(|| refl_serialise(obj_));
    });
    group.bench_with_input(BenchmarkId::new("object/serde", 0), &obj, |b, obj_| {
        b.iter_with_large_drop(|| serde_serialise(obj_));
    });

    for i in vec![1, 10, 50, 100].iter() {
        let mut author = Author {
            name: "John Doe".to_owned(),
            age: 32,
            active: true,
            books: Vec::new(),
        };
        let mut books = Vec::new();
        for _ in 0..(i.clone()) {
            books.push(obj.clone());
        }
        author.books = books;
        group.bench_with_input(BenchmarkId::new("refl", i), &author, |b, i_| {
            b.iter_with_large_drop(|| refl_serialise(i_))
        });

        group.bench_with_input(BenchmarkId::new("serde", i), &author, |b, i_| {
            b.iter_with_large_drop(|| serde_serialise(i_))
        });
    }

    group.finish();
}

criterion_group!(benches, parse_benchmark, serialise_benchmark);
criterion_main!(benches);
