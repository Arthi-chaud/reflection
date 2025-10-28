use nom::{
    IResult, Parser,
    branch::alt,
    bytes::{complete::tag, take_till},
    character::digit1,
    combinator::{map, map_res, opt},
};
use serde::{Deserialize, Serialize};
use serde_json::from_str;
use std::{
    char,
    io::{Error, Write},
};

#[derive(Serialize, Deserialize, Clone)]
pub struct Book {
    pub title: String,
    pub release_year: u16,
    pub page_count: u16,
    pub rating: f32,
}

#[derive(Serialize, Deserialize)]
pub struct Author {
    pub name: String,
    pub age: u16,
    pub books: Vec<Book>,
    pub active: bool,
}

pub trait ReflSerialisable<W: Write> {
    fn serialise(&self, dest: &mut W);
}

impl<W: Write> ReflSerialisable<W> for Book {
    fn serialise(&self, dest: &mut W) {
        let _ = write!(
            dest,
            "{{\"title\":\"{}\",\"release_year\":{},\"page_count\":{},\"rating\":{}}}",
            self.title, self.release_year, self.page_count, self.rating
        );
        ()
    }
}

impl<W: Write, A: ReflSerialisable<W>> ReflSerialisable<W> for Vec<A> {
    fn serialise(&self, dest: &mut W) {
        let _ = write!(dest, "[");
        let length = self.len();
        let mut i = 0;
        for b in self {
            b.serialise(dest);
            if i < length - 1 {
                let _ = write!(dest, ",");
            }
            i = i + 1;
        }
        let _ = write!(dest, "]");
        ()
    }
}

impl<W: Write> ReflSerialisable<W> for Author {
    fn serialise(&self, dest: &mut W) {
        let _ = write!(
            dest,
            "{{\"name\":\"{}\",\"age\":{},\"books\":",
            self.name, self.age
        );
        self.books.serialise(dest);
        let _ = write!(dest, ",\"active\":{}}}", self.active);
        ()
    }
}

pub trait ReflParsable<T> {
    fn parse(str: &str) -> IResult<&str, T>;
}

impl ReflParsable<Book> for Book {
    fn parse(str_: &str) -> IResult<&str, Book> {
        let mut str = str_;
        let mut book = Book {
            title: String::new(),
            release_year: 0,
            page_count: 0,
            rating: 0.0,
        };

        (str, _) = tag("{\"title\":\"")(str)?;
        (str, book.title) = map_res(take_till(|c| c == '"'), |s: &str| {
            Ok(s.to_string()) as Result<String, Error>
        })
        .parse(str)?;
        (str, _) = tag("\",\"release_year\":")(str)?;
        (str, book.release_year) = map_res(digit1(), |s| from_str::<u16>(s)).parse(str)?;
        (str, _) = tag(",\"page_count\":")(str)?;
        (str, book.page_count) = map_res(digit1(), |s| from_str::<u16>(s)).parse(str)?;

        (str, _) = tag(",\"rating\":")(str)?;

        (str, book.rating) =
            map_res(take_till(|c: char| c == '}'), |s| from_str::<f32>(s)).parse(str)?;
        let (str, _) = tag("}")(str)?;

        Ok((str, book))
    }
}

impl<A> ReflParsable<Vec<A>> for Vec<A>
where
    A: ReflParsable<A>,
{
    fn parse(str_: &str) -> IResult<&str, Vec<A>> {
        let mut str = str_;
        let mut res = Vec::with_capacity(100);
        (str, _) = tag("[")(str)?;
        loop {
            match A::parse(str) {
                Ok((str1, tmp)) => {
                    res.push(tmp);
                    (str, _) = opt(tag(",")).parse(str1)?;
                }
                _ => {
                    (str, _) = tag("]").parse(str)?;
                    return Ok((str, res));
                }
            }
        }
    }
}

impl ReflParsable<Author> for Author {
    fn parse(str_: &str) -> IResult<&str, Author> {
        let mut str = str_;
        let mut author = Author {
            name: String::new(),
            age: 0,
            active: false,
            books: Vec::new(),
        };

        (str, _) = tag("{\"name\":\"")(str)?;
        (str, author.name) = map_res(take_till(|c| c == '"'), |s: &str| {
            Ok(s.to_string()) as Result<String, Error>
        })
        .parse(str)?;
        (str, _) = tag("\",\"age\":")(str)?;
        (str, author.age) = map_res(digit1(), |s| from_str::<u16>(s)).parse(str)?;
        (str, _) = tag(",\"books\":")(str)?;
        (str, author.books) = Vec::parse(str)?;

        (str, _) = tag(",\"active\":")(str)?;

        (str, author.active) =
            map(alt((tag("true"), tag("false"))), |s: &str| s.eq("true")).parse(str)?;
        let (str, _) = tag("}")(str)?;

        Ok((str, author))
    }
}
