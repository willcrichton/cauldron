#![feature(proc_macro)]

extern crate cauldron;

use cauldron::auto_gc;
use std::rc::Rc;

macro_rules! assert_eq_float {
    ($e1:expr, $e2:expr, $eps:expr) => {
        assert!(($e1 - $e2).abs() < $eps)
    }
}

#[test]
fn arith1() {
    auto_gc! {
        let x: i32 = 1;
        let y = 1 + x;
    };
    assert_eq!(y, 2);
}

#[test]
fn arith2() {
    auto_gc! {
        let x = 1.0;
        let y: f64 = 1.0 + x;
    };
    assert_eq_float!(2.0, y, std::f64::EPSILON);
}

#[test]
fn ifthen() {
    auto_gc! {
        let x = 1;
        let y = if x == 1 { 1 } else { 2 };
    };
    assert_eq!(y, 1);
}

#[test]
fn closure() {
    auto_gc! {
        let x = 1;
        let y = |a: i32, b: i32| { x + a + b };
        let z = y(x, 1);
    };
    assert_eq!(z, 3);
}

#[test]
fn string() {
    auto_gc! {
        let x = "Hello";
        let y = if x == "Hello" { 1 } else { 2 };
    };
    assert_eq!(y, 1);
}
