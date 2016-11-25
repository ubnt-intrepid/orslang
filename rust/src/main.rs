#[macro_use]
extern crate nom;

mod orelang;

fn main() {
  println!("{:?}", orelang::parse_from_str("hoge"));
  println!("{:?}", orelang::parse_from_str("(print (= 1 (+ 2 3)))"));
  println!("{:?}",
           orelang::parse_from_file("../examples/example_sum.ore"));
}
