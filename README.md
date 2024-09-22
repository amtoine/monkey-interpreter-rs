# monkey-interpreter-rs
Writing an interpreter for Monkey in Rust.

## Introduction

this project is a Rust implementation of [_Writing An Interpreter In Go_](https://interpreterbook.com/).

see the Monkey language [here](https://monkeylang.org/#what-is-monkey).

## Example
there is currently a very simple CLI in [`main.rs`](src/main.rs) that can run with
```shell
cargo run
```

> **Note**
>
> press `<c-c>` to quit the REPL

then, typing Monkey code should output the resulting AST, e.g.
```c
let f = fn(x, y) { return x + y; }; f(1, 2);
```
will generate the following AST
```js
Program {
    statements: [
        Let(
            "f",
            Function(
                [ "x", "y"  ],
                [ Return(Infix(Identifier("x"), Plus, Identifier("y"))) ],
            ),
        ),
        Expression(
            Call(
                Identifier("f"),
                [ IntegerLitteral(1), IntegerLitteral(2) ],
            ),
        ),
    ],
}
```

## Roadmap
- [x] lexer
- [x] parser
    - [x] _let_ statements
    - [x] _return_ statements
    - [x] expressions
    - [x] extensions
        - [x] boolean literals
        - [x] grouped expressions
        - [x] _if_ expressions
        - [x] function literals
        - [x] call expressions
- [ ] evaluation
- [ ] extensions
    - [ ] strings
    - [ ] built-in functions
    - [ ] arrays
    - [ ] hash maps
- [ ] CLI
