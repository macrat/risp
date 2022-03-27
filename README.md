Risp
====

Risp is not a Lisp. But looks like a Lisp.

Just a learning project.

## example

```
$ cat hello-world.risp
(println "hello world!")

$ cargo run <./hello-world.risp
hello world!
```

```
$ cat factorial.risp
(def factorial (func (x)
  (def f (func (x n)
    (if (> n 1)
      (f (* x n) (- n 1))
      x)))
  (f x (- x 1))))

(println "5! =" (factorial 5))

$ cargo run <./factorial.risp
5! = 120
```

```
$ cat factorial-short.risp
(def f (func (x)
  (fold (seq x) *)))

(println "5! =" (factorial 5))

$ cargo run <./factorial-short.risp
5! = 120
```

```
$ cat lib.risp
(def greeting (func (target)
  (println "hello" (+ target "!"))))

$ cat main.risp
(def lib (import "./lib.risp"))

((lib greeting) "world")

$ cargo run <./main.risp
hello world!
```

```
$ cargo run
> (+ 1
|    2)
< 3

> (println (+ 1 2))
3
```

## functions

### variable
- `(def symbol value)`: define new variable named `symbol` in current scope.
- `(set symbol value)`: update value of variable named `symbol` which is already exist.

### flow
- `(if cond true-branch)` or `(if cond true-branch false-branch)`: check the `cond` is true or false, and execute branch. `0`, `""`, and `()` is false, otherwise true.
- `(while cond values...)`: compute values while `cond` is true value, and returns the last value.
- `(do values...)`: compute values in a new scope and returns the last value.
- `(panic! message...)`: report critical error and exit program.

### function
- `(func (args...) body...)`: make new function.

### list
- `(list values...)`: make list that have `values`. this is the same as `'(values...)` in major Lisp implementations.
- `(car list-or-string)`: take first item of the list, or take first character of the string.
- `(cdr list-or-string)`: take second and later items of the list, or take second and later characters of the string.
- `(seq to)` or `(seq from to)`: make list of numbers. for example, `(seq 5)` makes `(list 1 2 3 4 5)`.
- `(map list function)`: apply each values in the list to the function, and returns the results as a list. the function's signature is `(func (current-value))`.
- `(fold list function)`: apply each values in the list to the function, and accumulate result. the function's signature is `(func (accumulated-value current-value))`.

### type
- `(is-number values...)`: check the values are numbers.
- `(is-string values...)`: check the values are strings.
- `(is-list values...)`: check the values are lists.
- `(is-func values...)`: check the values are functions.

### io
- `(import string)`: import module from file.
- `(print values...)`: print `values` without `\n`.
- `(println values...)`: print `values` with `\n`.

### operator
- `(+ numbers-or-strings...)`: add numbers, or join strings.
- `(- numbers...)`: subtract numbers from the first number. `(- x)` means the same as `(- 0 x)`.
- `(* numbers...)`: multiply numbers.
- `(/ numbers...)`: divide numbers.
- `(= values...)`: returns `1` if the all values are the same, otherwise returns `0`.
- `(!= values...)`: returns `1` if the all values are not the same, otherwise returns `0`.
- `(not value)`: returns `1` if the value is true, otherwise returns `0`.
- `(< values...)`, `(<= values...)`, `(> values...)`, `(>= values...)`: compare values.
