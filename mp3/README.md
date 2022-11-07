CS 440 MP3
==========

In this machine problem you will be updating the interpreter we started
building in class in order to implement a number of new language constructs.

The core language constructs you will be adding include:

- Boolean values: `#t` and `#f`
- If-expression: `if`
- Relational expressions: `=`, `<`

You will also add the following "syntactic sugar" forms:

- Subtraction: `-`
- Boolean expressions: `and` and `or`
- Cond-expression: `cond`
- Relational expressions: `<=`, `>`, `>=`

Finally, you will also be adding the `define` form, which can be used to define
functions using the same syntax as Racket. These functions will be loaded
*before* the REPL is started (or some expression is evaluated). Unlike the
functions supported by the interpreter we built in class, `define`'d functions
will support recursion.

## Details

### Core language additions

- As with integer values, the Boolean values `#t` and `#f` evaluate to
  themselves (note that the reader already recognizes Boolean values, and you
  can match them in the parser using the `boolean?` predicate)

- The `if` expression is a simplified version of Racket's. It has the following
  form: 

      (if BOOL-EXP TRUE-EXP FALSE-EXP)

  where `BOOL-EXP` is a form that evaluates to a Boolean value, and
  `TRUE-EXP` and `FALSE-EXP` are any valid form. Its semantics are the same as
  Racket's.

- The relational expressions `=` and `<` have the forms:

      (= LHS RHS)

      (< LHS RHS)

  where `LHS` and `RHS` are forms that evaluate to integer values. `=` evaluates 
  to `#t` if  `LHS` and `RHS` are equal and `#f` otherwise. `<` evaluates to
  `#t` if `LHS` is less than `RHS` and `#f` otherwise.


### Syntactic sugar additions

- `-` (subtraction) takes two arguments that evaluate to integer values and
  evaluates to their difference. It is syntactic sugar for addition of the first
  value to the negative of the second. I.e.,

      (- EXP1 EXP2)

  desugars to:

      (+ EXP1 (* -1 EXP2))

- `and` takes one or more argument forms, and evaluates to `#t` if and only if
  all its arguments evaluate to `#t`; otherwise it evaluates to `#f`. It is syntactic sugar for one or more `if`  forms. E.g.,

      (and BEXP1 BEXP2 BEXP3)

  desugars to:

      (if BEXP1 (if BEXP2 (if BEXP3 #t #f) #f) #f)

- `or` takes one or more argument forms, and evaluates to `#t` if any of its
  arguments evaluate to `#t`; otherwise it evaluates to `#f`. It is syntactic sugar for one or more `if`  forms. E.g.,

      (or BEXP1 BEXP2 BEXP3)

  desugars to:

      (if BEXP1 #t (if BEXP2 #t (if BEXP3 #t #f)))

- `cond` is a multi-way conditional, similar to Racket's. It is syntactic sugar
  for one or more `if` forms. E.g.,

      (cond [BEXP1 REXP1]
            [BEXP2 REXP2]
            [BEXP3 REXP3]
            [else REXP4])

  desugars to:

      (if BEXP 
          REXP
          (if BEXP2
              REXP2
              (if BEXP3
                  REXP3
                  REXP4)))

  note that the last "default" case is mandatory, and that `else` is
  syntactically required but not an identifier/variable.

- `<=`, `>`, `>=` are syntactic sugar for combinations of `<`, `=`, and `or`.
  E.g., 

      (>= LHS RHS)
  
  desugars to:

      (or (< RHS LHS) (= LHS RHS))

### `define`

The `define` form will be used to define one or more functions in a separate
source file. This source file will be loaded and evaluated to create an
environment within which we can either run a REPL or evaluate an expression. 

The syntax of `define` is identical to that of Racket's (though we will not
support "rest" parameters and any other options). E.g., below we define a
function named `sum` with two parameters `x` and `y`, which returns their sum.

    (define (sum x y)
      (+ x y))

You will implement the function `load-defs`, which takes the name of a file and
returns an associative list containing all the name &rarr; function-value
mappings defined in that file. E.g., given a file named "test1.defs" with the
following contents:

    (define (fn-a x)
      (+ x 10))

    (define (fn-b x)
      (* x 20))

    (define (fn-c x)
      (fn-a (fn-b x)))

Calling `(load-defs "test1.defs")` would return the following (nested closures
are omitted):

    (list
      (cons 'fn-a
            (fun-val 'x
                      (arith-exp "+" (var-exp 'x) (int-exp 10))
                      '(...)))
      (cons 'fn-b
            (fun-val 'x
                      (arith-exp "*" (var-exp 'x) (int-exp 20))
                      '(...)))
      (cons 'fn-c
            (fun-val 'x
                      (app-exp (var-exp 'fn-a)
                              (app-exp (var-exp 'fn-b) (var-exp 'x)))
                      '(...))))

This list is suitable for passing as an initial `env` argument to `eval`. I.e.,
after modifying `eval` to take an initial environment, we can do:

    > (eval (desugar (parse '(fn-c 10)))
            (load-defs "test1.defs"))
    210

Critically, `define` will allow us to **define recursive functions**. Note that
our implementations of `lambda` and function application in class did not
support recursion (it's worth taking some time to make sure you understand why
not!). After correctly implementing `define`, however, we can evaluate a
definition like: 

    (define (sum-to n)
      (if (= n 0)
          0
          (+ n
            (sum-to (- n 1)))))

Et voila:

    > (eval (desugar (parse '(sum-to 10)))
            (load-defs "test2.defs"))
    55

This will likely be the toughest part of this machine problem (though it doesn't
translate into much code!). The most straightforward implementation
does require a new mechanism: *a cyclic structure*. If you feel up for a
challenge and want to figure it out for yourself, check out [Immutable Cyclic
Data](https://docs.racket-lang.org/reference/pairs.html#%28part._.Immutable_.Cyclic_.Data%29)
in the Racket documentation.

For more detailed hints, see the "Hint" section in the next section.

## Implementation and Starter code

All your changes should be made to "mp2.rkt". It is the only source file we will
evaluate.

We provide you with the (slightly amended) interpreter that we wrote together in
class. We also provide you with the following starter code for `load_defs`:

    (define (load-defs filename)
      (let* ([sexps (file->list filename)]
             [fn-names (map (compose first second) sexps)])
        fn-names))

which reads all the s-expressions (corresponding to `define` forms) from the
named file and returns a list of the function names being defined. You should
use it as a starting point.

You are free to add new `struct` definitions, alter existing `struct`s, define
new functions, alter existing functions, etc. Just take care that you *do not*
change the APIs of the `parse`, `desugar`, `eval`, `load_defs`, and `repl`
functions, as we will be testing those directly.

### Hint: On implementing recursion

First of all, recall that a "function value" is a structure that contains a
function definition (consisting of parameter names and a body) *and* a closure.
A closure represents the environment at the time the function is created, and is
in our case just an associative list.

Here's the structure we defined:

    (struct fun-val (id body env) #:transparent)

When we apply this function to an argument, we evaluate its `body` in the
environment `env`, with a new mapping for the parameter and argument. 

Here's the relevant bit from `eval`:

    [(app-exp f arg)
        (match-let ([(fun-val id body clenv) (eval f env)]
                    [arg-val (eval arg env)])
          (eval body (cons (cons id arg-val) clenv)))]

Now imagine that the body of the function contains a recursive call (i.e., a
call to the function itself). Would we be able to locate the value corresponding
to the function's own name?

No! The problem is that when we create a closure, we are saving the "outside"
environment, but we are not saving the name of the function itself (which should
map to the self-same function value). Here's where we create function values
from `lambda`s in `eval`:

    [(lambda-exp id body)
        (fun-val id body env)] -- env is the closure

See how the function value (and closure) doesn't see its own "name"?

To fix this, your implementation will need to create a *cyclic structure*.
Specifically, you want the closure to refer to the function value in which it is
contained.

To create a cyclic structure in Racket, we can use the `make-placeholder`,
`placeholder-set!`, and `make-reader-graph` functions. Intuitively,
`make-placeholder` creates a bookmark that can later be filled in by
`placeholder-set!`, and `make-reader-graph` constructs a graph (which, unlike a
tree, may contain cycles) based on these bookmarks.

E.g., to create a cyclic list of the infinitely repeating sequence: 1, 2, 3, 1,
2, 3, 1, 2, 3, ..., we can do:

    (define inf-list
      (let* ([ph (make-placeholder '())]          ; placeholder with val '()
             [lst (cons 1 (cons 2 (cons 3 ph)))])  ; acyclic list ending with ph
        (placeholder-set! ph lst)  ; replace ph val with list head
        (make-reader-graph lst)))  ; read off the resulting cyclic list

We can use placeholders with Racket `struct`s to create cyclic structures, too.
We just need to mark those `struct`s as "prefab", first. E.g., if we modify our
function value `struct` as follows:

    (struct fun-val (id body env) #:prefab)

We can do:

    (define cyc-env
      (let* ([ph (make-placeholder '())]
             [env (list (cons 'f (fun-val 'x (int-exp 10) ph)))])
        (placeholder-set! ph env)
        (make-reader-graph env)))

And now we have a closure that refers back to the environment in which its
associated function is defined!

Check it out:

    > cyc-env
    #0=(list (cons 'f (fun-val 'x (int-exp 10) #0#)))

    > (fun-val-env (cdr (assoc 'f cyc-env)))
    #0=(list (cons 'f (fun-val 'x (int-exp 10) #0#)))

(The `#0=` and `#0#` notation is to help us visualize the cyclical structure --
those values aren't actually present as data.)


## Testing

We have provided you with test cases in "mp2-test.rkt" and sample definition
files in "test1.defs" and "test2.defs". Feel free to add to and alter any and
all tests, as we will be using our own test suite to evaluate your work.

Note that passing all the tests *does not guarantee full credit*! In particular,
we will be checking that your desugaring function correctly transforms the
syntax of syntactic sugar to core language forms, and that you aren't using any
metacircular hacks. 

## Grading

The core language constructs are worth a total of 24 points:

- Boolean values: `#t` and `#f` (8 points)
- If-expression: `if` (8 points)
- Relational expressions: `=`, `<` (8 points)

---

The syntactic sugar are worth a total of 32 points: 

- Subtraction: `-` (8 points)
- Boolean expressions: `and` and `or` (8 points)
- Cond-expression: `cond` (8 points)
- Relational expressions: `<=`, `>`, `>=` (8 points)

Note that you will only earn full points if you implement them correctly as part
of your desugaring process. If you alter your `eval` function to support them
you will lose points.

---

A fully working `define` is worth 20 points. You may receive partial credit for
a `define` implementation that is partially correct (e.g., doesn't support
recursion).

-- 

The maximum possible points = 24 + 32 + 20 = 76.

## Submission

When you are done with your work, simply commit your changes and push them to
our shared private GitHub repository. Please note that your submission date will
be based on your most recent push (unless you let us know to grade an earlier
version). 
