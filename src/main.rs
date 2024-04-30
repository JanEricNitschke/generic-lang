use std::{io::Write, path::PathBuf};

use clap::Parser;

use vm::InterpretResult;

use crate::vm::VM;

mod bitwise;
mod chunk;
mod compiler;
mod config;
mod heap;
mod natives;
mod scanner;
mod stdlib;
mod types;
mod value;
mod vm;

#[allow(clippy::struct_excessive_bools)]
#[derive(Parser, Debug)]
#[command(version)]
struct Args {
    file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    args.file.map_or_else(repl, run_file);
}

fn repl() {
    let mut vm = VM::new(PathBuf::from(""));
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let mut line = String::new();
        if std::io::stdin().read_line(&mut line).unwrap() > 0 {
            vm.interpret(line.as_bytes());
        } else {
            println!();
            break;
        }
    }
}

fn run_file(file: PathBuf) {
    match std::fs::read(&file) {
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(74);
        }
        Ok(contents) => {
            let mut vm = VM::new(file);
            match vm.interpret(&contents) {
                InterpretResult::CompileError => std::process::exit(65),
                InterpretResult::RuntimeError => std::process::exit(70),
                InterpretResult::Ok => {}
            }
        }
    }
}
