#![feature(proc_macro)]

extern crate cauldron;

use cauldron::auto_gc;
use std::rc::Rc;


#[test]
fn closure() {
    auto_gc! {
        let x = 1;
        let y = |a: i32, b: i32| { x + a + b };
        let z = y(x, 1);
    };
    assert_eq!(z, 3);
}
