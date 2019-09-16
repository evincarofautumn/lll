# LLL

**A Linear-Logical Language**

LLL is a toy programming language with a type system based on linear logic. It
makes two contributions. First, it gives an intuitionistic interpretation of the
classical “par” (⅋) connective of multiplicative disjunction to represent
concurrent tasks that must obey two rules:

  * **Race-freedom:** the type system ensures that linear resources must be
    divided into disjoint subsets when spawning tasks, so there is no
    possibility of a data race amongst them.

  * **Deadlock-freedom:** tasks communicate over *channels*, and the type system
    ensures that the communication graph induced by these channels is *acyclic*,
    so there is no possibility of a deadlock.

Second, it introduces an interpretation of *fractional types* and *negative
types* to represent mutable borrowing of substructures, in such a way that code
can operate on a purely functional representation

## Specification

### Multiplicative Conjunction / Tensor Product

The multiplicative conjunction is a strict pair, of which both values must be
consumed. It’s introduced by tuple syntax and eliminated by irrefutable
pattern-matching in a `let` binding.

#### Introduction

> Γ ⊢ e₁ : A
>
> Δ ⊢ e₂ : B
>
> ----
>
> Γ, Δ ⊢ (e₁, e₂) : A * B

#### Elimination

> Γ ⊢ e₁ : A `*` B
>
> Γ, x : A, y : B ⊢ e₂ : C
>
> ----
>
> Γ ⊢ `let` x`,` y `=` e₁ `in` e₂ : C

### Additive Disjunction / Sum

An additive disjunction is a standard strict sum type. It’s introduced by left
and right injections and eliminated by pattern matching with a `match`
expression.

#### Introduction

> Γ ⊢ e : A
>
> ----
>
> Γ ⊢ `left` e : A `+` B

> Γ ⊢ e : B
>
> ----
>
> Γ ⊢ `right` e : A `+` B

#### Elimination

> Γ ⊢ e₁ : A + B
>
> Γ, x : A ⊢ e₂ : C
>
> Γ, y : B ⊢ e₃ : C
>
> ----
>
> Γ ⊢ `match` e₁ `with`  
>   `|` `left` x `->` e₂  
>   `|` `right` y `->` e₃
> : C

### Additive Conjunction / Lazy Pairs

An additive conjunction is a lazy pair type of which exactly one element may be
used. It’s introduced by supplying a pair of computations, and eliminated by
projections that select which of the computations will be evaluated and which
will be discarded.

#### Introduction

> Γ ⊢ e₁ : A
>
> Γ ⊢ e₂ : B
>
> ----
>
> Γ ⊢ `[` e₁ `,` e₂ `]` : A `&` B

#### Elimination

> Γ ⊢ e : A `&` B
>
> ----
>
> Γ ⊢ `first` e₁ : A

> Γ ⊢ e : A `&` B
>
> ----
>
> Γ ⊢ `second` e : B

### Multiplicative Disjunction / Parallel Pair

A multiplicative disjunction is a lazy type with both conjunctive and
disjunctive properties: like the additive conjunction, it’s introduced by
supplying a pair of computations; like the additive disjunction, it’s eliminated
by a pattern match with multiple branches; but like the multiplicative
conjunction, *both* those branches are evaluated.

#### Introduction

> Γ ⊢ e₁ : A
>
> Γ ⊢ e₂ : B
>
> ----
>
> Γ ⊢ e₁ | e₂ : A `|` B

#### Elimination

There are two rules for elimination because there are two basic possibilities:
either the left branch returns a value and the right branch doesn’t, or vice
versa.

The precise sense of “doesn’t return” is important here, though: it doesn’t
refer to `abort`, which terminates a task *unsuccessfully*. Rather, the point of
multiplicative disjunction is that two computations may be spun up in parallel,
and the type system enforces two things:

  * One of the branches joins the other by calling the `send` primitive, which
    consumes a value and a one-shot channel and terminates the current task
    *successfully*, but without a result. It’s a *type error* for both tasks to
    try to return a value, although both branches may call `send`.

  * Because both branches are taken, and in the same scope, it’s a *type error*
    again to try to share any linear state between them.

These two properties guarantee that the communication graph among tasks is
acyclic, so there can be no deadlocks, and that tasks cannot implicitly share
any resources, so there can be no races.

> Γ ⊢ e₁ : A `|` B
>
> Γ, x : A ⊢ e₂ : C
>
> Γ, y : B ⊢ e₃ : `bottom`
>
> ----
>
> Γ ⊢ `fork` e₁ `into`  
>   `|` x `->` e₂  
>   `|` y `->` e₃  
> : C

> Γ ⊢ e₁ : A `|` B
>
> Γ, x : A ⊢ e₂ : `bottom`
>
> Γ, y : B ⊢ e₃ : C
>
> ----
>
> Γ ⊢ `fork` e₁ `into`  
>   `|` x `->` e₂  
>   `|` y `->` e₃  
> : C

## Primitives

### `send`

```
send : @a -> ~a -> a -> done
```

`send` takes a one–shot channel accepting values of type `a` and a a value of
type `a`. It sends the value to the channel, thereby closing it, and terminates
the current task, returning `done`.

### `open`

```
open : @a -> () -> ~a | a
```

`open` opens a one-shot channel accepting values of type `a` and returns a lazy
value of type `a` which, when forced, performs a blocking read on that
channel.

Functions can be analyzed in terms of `open` and `fork`. Supposing `Expression`
denotes some expression, and `Expression[y/x]` denotes replacing every instance
of `x` with `y` in `Expression`:

```
let function = \ parameter -> Expression in
function argument
```

⇓

```
let channel = open () in
fork channel into
  | input ->
    send input argument
  | output ->
    Expression[output/parameter]
```

This forks two tasks, one of which immediately sends the `argument` to the
`input` channel (and thus quits); the other, `Expression`, proceeds until it
reads from its argument, the `output` channel, whereupon it blocks awaiting the
receipt of a value—in this case, it unblocks immediately and returns the result
of `Expression`.

### `split`

```
split : @ a b -> (a -> b) –> ~a | b
```

`split` “breaks open” a function type and returns it as its classical equivalent
in terms of multiplicative disjunction. This permits “lifting” synchronous code
into asynchronous code by sending the input and output channels of a function to
separate tasks. The example for `open` can thus be written directly in LLL:

```
let function = \ parameter -> Expression in
function argument
```

⇓

```
let function = \ parameter -> Expression in
let channel = split function in
fork channel into
  | input ->
    send input argument
  | output ->
    Expression[output/parameter]
```

<!--

```
[[ go (\c. e) ]]
->
[[ _ : source a ]] | (\c. e) (_ : sink a)

go : (~a -> done) -> ~~a & done ?

[[ yield (_ : source a) ]] | [[ (_ : sink a) v ]]
->
[[ v ]] | [[ closed ]]
```

-->
