# Toy Language Interpreter

A minimalist Scheme-like programming language interpreter written in Python. This project implements a custom programming language with support for basic arithmetic operations, conditionals, and functional programming concepts.

## Features

- Lambda functions and closures
- Basic arithmetic operations `(+, -, *, /)`
- Comparison operators `(<, >, <=, >=)`
- Boolean operations `(and, or, not)`
- Conditional statements `(if)`
- Local variable bindings
- Comments support

## Installation

```bash
git clone https://github.com/M-krishna/toy_lang.git
cd toy_lang
python3 main.py
```

## Quick start

```lisp
; Hello World
(display "Hello, World!")

; Basic arithmetic
(+ 1 2 3)  ; => 6
(* 4 5)    ; => 20

; Lambda functions
(lambda (x) (* x x))

; Conditional statements
(if (> 5 3)
    "yes"
    "no")
```

## Language Features

Arithmetic Operations

* Addition: `(+ 1 2 3)`
* Subtraction: `(- 10 5)`
* Multiplication: `(* 2 3 4)`
* Division: `(/ 10 2)`

Comparison Operators

* Less than: `(< 5 10)`
* Greater than: `(> 10 5)`
* Less than or equal: `(<= 5 5)`
* Greater than or equal: `(>= 10 10)`


Boolean Operations

* And: `(and #t #f)`
* Or: `(or #t #f)`
* Not: `(not #t)`


Functions

```lisp
; Define a function
(lambda (x) (* x x))

; Using begin for multiple expressions
(begin
  (define square (lambda (x) (* x x)))
  (square 5))
```

## Development

To set up the development environment:
```bash
git clone https://github.com/M-krishna/toy_lang.git
cd toy_lang
```

## Contributing
* Fork the repository
* Create your feature branch (`git checkout -b feature/amazing-feature`)
* Commit your changes (`git commit -m 'Add some amazing feature'`)
* Push to the branch (`git push origin feature/amazing-feature`)
* Open a Pull Request


## License

This project is licensed under the MIT License - see the LICENSE file for details.