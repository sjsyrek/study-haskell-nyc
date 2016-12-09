theme: Next, 1
build-lists: true
autoscale: true

# [fit] λ calculus

^ Note: This presentation is designed to be viewed with [http://www.decksetapp.com/](http://www.decksetapp.com/).

---

# definition

_<expression>  := <name> | <function> | <application>_
_<function>    := λ <name>.<expression>_
_<application> := <expression><expression>_

- `x`
- `λx. x`
- `(λx. x)`
- `(λx. (λy. x y))`
- `(λx. x) y`
- `(λx. x)(λy. y)`

---

# parentheses

_E ≡ (E)_

_E<sub>1</sub>E<sub>2</sub>E<sub>3</sub>...E<sub>n</sub> ≡ (...((E<sub>1</sub>E<sub>2</sub>)E<sub>3</sub>)...E<sub>n</sub>)_

- `a b c d`
- `(a b) c) d)`
- `λa. λb. λc. λd. a b c d`
- `(λa. (λb. (λc. (λd. ((a b) c) d))))`
- `λabcd. abcd`
- `(λa. (λb. (λc. (a b c ((λd. d) z)) q)))`

---

# α-equivalence

- `λa. a ≡`
- `λz. z ≡`
- `λpoo. poo ≡`
- `λ`:shit:`. `:shit:

---

# renaming

- `(λx. (λy. x y)) y`
- `λy .y y`
- ~~`λy .y y`~~
- `(λx. (λz. x z)) y`
- `λz. y z`

---

# β-reduction

- `(λx. x) y`
- `[x := y] x`
- `y`
- `(λx. λy. x y) p q`
- `(λx. λy. x y)(p) q`
- `([x := p] λy. x y) q`
- `(λy. p y) q`
- `[y := q] p y`
- `p q`
- `(λx. x x)(λx. x x)`
- `[x := (λx. x x)] (λx. x x)`
- `(λx. x x)(λx. x x)`

---

# booleans

- `true ≡ (λx. λy. x)`
- `false ≡ (λx. λy. y)`
- `and ≡ (λa. λb. a b false) ≡ (λa. λb. a b (λx. λy. y))`
- `or  ≡ (λa. λb. a true b) ≡ (λa. λb. a (λx. λy. x) b)`
- `not ≡ (λa. a false true) ≡ (λa. a (λx. λy. y)(λx. λy. x))`
- `and true false ≡ (λa. λb. a b (λx. λy. y))(λx. λy. x)(λx. λy. y) = (λx. λy. y) ≡ false`
- `not false ≡ (λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y) = (λx. λy. x) ≡ true`

---

# numbers

- `0 ≡ (λf. λx. x)`
- `1 ≡ (λf. λx. f(x))`
- `2 ≡ (λf. λx. f(f(x)))`
- `3 ≡ (λf. λx. f(f(f(x))))`
- `...`
- `succ ≡ (λn. λf. λx. f(n f x))`
- `pred ≡ (λn. n (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)`
- `succ 1 ≡ (λn. λf. λx. f(n f x))(λf. x. f(x)) = (λf. λx. f(f(x))) ≡ 2`
- `pred 1 ≡ (λn. n (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)(λf. λx. f(f(x))) = (λf. λx. x) ≡ 0`

---

# arithmetic

- `add ≡ (λn. λm. λf. λx. n f(m f x))`
- `sub ≡ (λm. λn. n pred m)`
- `mult ≡ (λn. λm. λf. n(m f)) or (λn. λm. m (add n) 0)`
- `exp ≡ (λx. λy. y x)`
- `add 1 2 ≡ (λn. λm. λf. λx. n f(m f x))(λf. λx. f(x))(λf. λx. f(f(x))) = (λf. λx. f(f(f(x)))) ≡ 3`
- `sub 2 1 ≡ (λm. λn. n (λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) m)(λf. λx. f(f(x)))(λf. λx. f(x)) = (λf. λx. f(x)) ≡ 1`
- `mult 3 0 ≡ (λn. λm. λf. n(m f))(λf. λx. f(f(f(x))))(λf. λx. x) = (λf. λx. x) ≡ 0`
- `exp 2 3 ≡ (λx. λy. y x)(λf. λx. f(f(x)))(λf. λx. f(f(f(x)))) = (λf. λx. f(f(f(f(f(f(f(f(x))))))))) ≡ 8`

---

# predicates

- `isZero ≡ (λx. x false not false)`
- `isZero 0 ≡ (λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y))(λf. λx. x) = (λx. λy. x) ≡ true`
- `lessThanOrEqual ≡ (λm. λn. isZero (sub m n))`
- `equals ≡ (λm. λn. and (lessThanOrEqual m n)(lessThanOrEqual n m))`
- _P E<sub>1</sub> E<sub>2</sub> ≡ if P is true E<sub>1</sub> else E<sub>2</sub>_

---

# recursion

- `Y ≡ (λy. (λx. y(x x))(λx. y(x x)))`
- `F ≡ (λf. λn. ((isZero n) 1 (mult n (f(pred n)))))`
- `fact ≡ (Y F)`
- `((λy. (λx. y(x x))(λx. y(x x))) F)`
- `((λx. F(x x))(λx. F(x x)))`
- `(F((λx. F(x x))(λx. F(x x))))`
- `(F(Y F))`

---

# factorial

- `fact 2`
- `(Y F) 2`
- `((λy. (λx. y(x x))(λx. y(x x))) F) 2`
- `((λx. F(x x))(λx. F(x x))) 2`
- `(F((λx. F(x x))(λx. F(x x)))) 2`
- `(F(Y F)) 2`
- `((λf. λn. ((isZero n) 1 (mult n (f(pred n)))))(Y F)) 2`
- `(λn. ((isZero n) 1 (mult n ((Y F)(pred n))))) 2`
- `((isZero 2) 1 (mult 2 ((Y F)(pred 2))))`
- `(mult 2 ((Y F)(pred 2)))`
- `(mult 2 ((Y F) 1))`
- `(mult 2 1)`
- `2`

---

# factorial (expanded)

- `((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f))n(f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))(λf. λx. f(f(x)))`
- `((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y))n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))(λf. λx. f(f(x)))`
- `((λx. (λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p(λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))(x x))(λx. (λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))(x x)))(λf. λx. f(f(x)))`
- `((λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y))n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y))n)))))((λx. (λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y))n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))(x x))(λx. (λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))(x x))))(λf. λx. f(f(x)))`
- `((λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y))n)))))((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))))(λf. λx. f(f(x)))`
- `((λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n) (λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n)))))))(λf. λx. f(f(x)))`
- `(λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))) (λf. λx. f(f(x)))`
- `(((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y))(λf. λx. f(f(x))))(λf. λx. f(x))((λn. λm. λf. n(m f))(λf. λx. f(f(x)))(((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y))(λf. λx. f(f(x)))))))`
- `((λn. λm. λf. n(m f))(λf. λx. f(f(x)))(((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y))(λf. λx. f(f(x))))))`
- `((λn. λm. λf. n(m f))(λf. λx. f(f(x)))(((λy. (λx. y(x x))(λx. y(x x)))(λf. λn. (((λx. x (λx. λy. y)(λa. a (λx. λy. y)(λx. λy. x))(λx. λy. y)) n)(λf. λx. f(x))((λn. λm. λf. n(m f)) n (f((λn. n (λp. λz. z((λn. λf. λx. f(n f x))(p (λx. λy. x)))(p (λx. λy. x)))(λz. z (λf. λx. x)(λf. λx. x))(λx. λy. y)) n))))))(λf. λx. f(x))))`
- `((λn. λm. λf. n(m f))(λf. λx. f(f(x)))(λf. λx. f(x)))`
- `(λf. λx. f(f(x)))`

---

# Turing completeness[^1]

- numbers
- branching
- recursion

[^1]: Source: _the Internet_
