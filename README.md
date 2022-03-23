Risp
====

Risp is not a Lisp. But looks like a Lisp.

Just a learning project.

## example

```
$ cat hello-world.lisp
(println "hello world!")

$ cargo run <./hello-world.lisp
hello world!
```

```
$ cat factorial.lisp
(def factorial (func (x)
  (def f (func (x n)
    (if (> n 1)
      (f (* x n) (- n 1))
      x)))
  (f x (- x 1))))

(println "5! =" (factorial 5))

$ cargo run <./factorial.lisp
5! = 120
```

```
$ cat factorial-short.lisp
(def f (func (x)
  (fold (seq x) *)))

(println "5! =" (factorial 5))

$ cargo run <./factorial-short.lisp
5! = 120
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

### data types
#### function
- `(func (args...) body...)`: make new function.

#### list
- `(list values...)`: make list that have `values`. this is the same as `'(values...)` in major Lisp implementations.
- `(car list-or-string)`: take first item of the list, or take first character of the string.
- `(cdr list-or-string)`: take second and later items of the list, or take second and later characters of the string.
- `(seq to)` or `(seq from to)`: make list of int numbers. for example, `(seq 5)` makes `(list 1 2 3 4 5)`.

#### type checking
- `(is-int values...)`: check the values are int numbers.
- `(is-string values...)`: check the values are strings.
- `(is-list values...)`: check the values are lists.
- `(is-func values...)`: check the values are functions.

### utilities
- `(print values...)`: print `values` without `\n`.
- `(println values...)`: print `values` with `\n`.

### flows
- `(if cond true-branch)` or `(if cond true-branch false-branch)`: check the `cond` is true or false, and execute branch. `0`, `""`, and `()` is false, otherwise true.
- `(while cond values...)`: compute values while `cond` is true value, and returns the last value.
- `(map list function)`: apply each values in the list to the function, and returns the results as a list. the function's signature is `(func (current-value))`.
- `(fold list function)`: apply each values in the list to the function, and accumulate result. the function's signature is `(func (accumulated-value current-value))`.
- `(do values...)`: compute values in a new scope and returns the last value.
- `(panic! message...)`: report critical error and exit program.

### operators
- `(+ numbers-or-strings...)`: add numbers, or join strings.
- `(- numbers...)`: subtract numbers from the first number. `(- x)` means the same as `(- 0 x)`.
- `(* numbers...)`: multiply numbers.
- `(/ numbers...)`: divide numbers.
- `(= values...)`: returns `1` if the all values are the same, otherwise returns `0`.
- `(!= values...)`: returns `1` if the all values are not the same, otherwise returns `0`.
- `(< values...)`, `(<= values...)`, `(> values...)`, `(>= values...)`: compare values.
