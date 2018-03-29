#![feature(lang_items)]

use std::rc::Rc;

#[lang="gc"]
pub type Gc<T> = Rc<T>;
