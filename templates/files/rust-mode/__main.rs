// __main.rs -*- mode: rustic -*-
////-- public module declarations
pub mod $1;

////-- end public module declarations

////-- private modules
mod $2;

////-- end private modules

////-- standard imports
use std::io;
use log;
use simple_logger;

////-- end standard imports

fn main() {
    simple_logger::init().unwrap();

    ${0:println!("Hello, world!");}

}
