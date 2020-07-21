use std::{env, io};
use std::fs::File;
use std::io::{Read, Write};

use rlox_vm::chunk::Chunk;
use rlox_vm::vm::VM;
use rlox_vm::scanner::Scanner;
use rlox_vm::compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();

    let app = App {
        vm: VM::new(Chunk::new()),
    };

    match args.len() {
        1 => app.run_prompt(),
        2 => app.run_file(&args[1]),
        _ => eprintln!("Invalid number of arguments."),
    }
}

struct App {
    vm: VM,
}

impl App {
    fn run_prompt(mut self) {
        loop {
            print!(">>> ");
            io::stdout().flush().expect("Failed to flush stdout.");

            let mut input = String::new();
            io::stdin().read_line(&mut input).expect("Error reading input.");

            if input == "exit()\n" {
                break;
            }

            self.run(input);
        }
        self.vm.free_objects();
    }

    fn run_file(mut self, path: &str) {
        let mut f = File::open(path).expect("Error opening file.");

        let mut content = String::new();
        f.read_to_string(&mut content).expect("Error reading file.");

        self.run(content.chars().collect());
        self.vm.free_objects();
    }

    fn run(&mut self, source: String) {
        let source: Vec<char> = source.chars().collect();
        let scanner = Scanner::new(&source);

        match Compiler::compile(scanner) {
            Ok((chunk, objects)) => {
                chunk.disassemble("Test");
                let result = self.vm.interpret(chunk, objects);
                println!("Interpret result: {:?}", result);
            },
            Err(error) => println!("Error [{}]: {}", error.line, error.message),
        }
    }
}