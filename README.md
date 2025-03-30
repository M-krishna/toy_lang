![Toy lang](./assets/toy_lang.png)

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
- List data structure

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

; Strings
"Hello World"
```

## Language Features

**Arithmetic Operations**

* Addition: `(+ 1 2 3)`
* Subtraction: `(- 10 5)`
* Multiplication: `(* 2 3 4)`
* Division: `(/ 10 2)`

**Comparison Operators**

* Less than: `(< 5 10)`
* Greater than: `(> 10 5)`
* Less than or equal: `(<= 5 5)`
* Greater than or equal: `(>= 10 10)`


**Boolean Operations**

* And: `(and #t #f)`
* Or: `(or #t #f)`
* Not: `(not #t)`


**Functions**

```lisp
; Define a function
(lambda (x) (* x x))

; Using begin for multiple expressions
(begin
  (define square (lambda (x) (* x x)))
  (square 5))
```

**List data structure**

```lisp
; An empty list
(list)
()

; A list with single value
(list 1)
(1)

(list "a")
(a)

; A list with multiple values
(list 1 2 3 4 5 6 7 8)
(1 2 3 4 5 6 7 8)

(list "a" "b" "c")
(a b c)

; Nested list
(list 1 (list 2 3) 4 5)
(1 (2 3) 4 5)

; Defining a list
(define my_list (list 1 2 3))
my_list ; (1 2 3)

; Defining a nested list
(define my_list (list 1 2 (list 3 4) 5))
my_list ; (1 2 (3 4) 5)
```

**Functions for list data structure**

```lisp
(car (list 1 2 3)) ; returns 1

(cdr (list 1 2 3)) ; returns (2 3)
```

**Cons for constructing pairs**

```lisp
(cons 1 2) ; (1 . 2)

(cons 1 (cons 2 3)) ; (1 2 . 3)

(cons 1 (list 2 3 4)) ; (1 2 3 4)

(cons (list 1 2) 3) ; ((1 2) . 3)

(define pair1 (cons 10 20))
(define pair2 (cons 30 40))
(define combined (cons pair1 pair2))
combined ; ((10 . 20) . (30 . 40))
(car combined) ; (10 . 20)
(cdr combined) ; (30 . 40)
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