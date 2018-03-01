#![feature(custom_attribute)]

extern crate cauldron;

#[cfg(test)]
mod tests {
    use cauldron::*;

    #[test]
    fn it_works() {
        halide! {{
            I(x, y) = x + y;
        }};
        // println!("{}", test(1, 2));
    }
}
