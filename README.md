A simple dependent type language with an repl inspired by the 
[blog](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/)
and [blog](http://augustss.blogspot.tw/2007/10/simpler-easier-in-recent-paper-simply.html).

### Examples
```
λ> Parameter A : Type 0
A is assumed.
λ> Parameter B : A -> Type 0
B is assumed.
λ> Parameter Pair : (x : A) -> B x -> Type 0
Pair is assumed.
λ> Parameter N : Type 0
N is assumed.
λ> Parameter z : N
z is assumed.
λ> Parameter s : N -> N
s is assumed.
λ> Definition three := fun f : N -> N => fun x : N => f (f (f x))
three is assumed.
λ> Check three
((N -> N) -> (N -> N))
λ> Eval (three (three s)) z
(s (s (s (s (s (s (s (s (s z)))))))))
```
