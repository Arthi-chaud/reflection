use serde::{Deserialize, Serialize};
use std::io::Write;

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
            "{{\"title\":{},\"release_year\":{},\"page_count\":{},\"rating\":{}}}",
            self.title, self.release_year, self.page_count, self.rating
        );
        ()
    }
}

impl<W: Write, A: ReflSerialisable<W>> ReflSerialisable<W> for Vec<A> {
    fn serialise(&self, dest: &mut W) {
        let _ = write!(dest, "[");
        for b in self {
            b.serialise(dest);
            let _ = write!(dest, ",");
        }
        let _ = write!(dest, "]");
        ()
    }
}

impl<W: Write> ReflSerialisable<W> for Author {
    fn serialise(&self, dest: &mut W) {
        let _ = write!(
            dest,
            "{{\"name\":{},\"age\":{},\"books\":",
            self.name, self.age
        );
        self.books.serialise(dest);
        let _ = write!(dest, ",\"active\":{}}}", self.active);
        ()
    }
}
