use std::{env, io};
use std::fs::File;
use std::io::{Read, Write};

use rlox_vm::vm::VMInitializedState;
use rlox_vm::scanner::Scanner;
use rlox_vm::parser::Parser;
use rlox_vm::allocator::DefaultAllocator;
use rlox_vm::chunk::Chunks;

type VM = rlox_vm::vm::VM<DefaultAllocator, VMInitializedState>;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => eprintln!("Invalid number of arguments."),
    }
}

fn run_prompt() {
    let mut chunks = Chunks::new();

    let mut vm = VM::new(DefaultAllocator::new());

    loop {
        print!(">>> ");
        io::stdout().flush().expect("Failed to flush stdout.");

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Error reading input.");

        if input == "exit()\n" {
            break;
        }

        vm = run(&input, vm, &mut chunks);
    }
    vm.free_objects();
}

fn run_file(path: &str) {
    let mut f = File::open(path).expect("Error opening file.");

    let mut content = String::new();
    f.read_to_string(&mut content).expect("Error reading file.");

    let mut chunks = Chunks::new();
    let vm = VM::new(DefaultAllocator::new());

    run(&content, vm, &mut chunks).free_objects();
}

fn run(source: &str, mut vm: VM, chunks: &mut Chunks) -> VM {
    match Parser::parse(Scanner::new(source), &mut vm.allocator, chunks) {
        Ok(function) => {
            chunks.get_chunk(function.chunk_index).disassemble("Script");

            let (new_vm, result) = vm.interpret(function, &chunks, true);
            vm = new_vm;

            if let Err(error) = result {
                println!("{}", error);
            }
        },
        Err(error) => println!("{}", error),
    }

    vm
}
