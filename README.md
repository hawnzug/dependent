A simple dependent type language with an repl inspired by the 
[blog](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/)
and [blog](http://augustss.blogspot.tw/2007/10/simpler-easier-in-recent-paper-simply.html).

### Examples
```
λ> Parameter Nat : Type 0
Nat is assumed.
λ> Parameter z : Nat
z is assumed.
λ> Parameter s : Nat -> Nat
s is assumed.
λ> Definition three := fun f : Nat -> Nat => fun x : Nat => f (f (f x))
three is assumed.
λ> Check three
((Nat -> Nat) -> (Nat -> Nat))
λ> Eval (three (three s)) z
(s (s (s (s (s (s (s (s (s z)))))))))
λ> Parameter Bool : Type 0
Bool is assumed.
λ> Parameter true : Bool
true is assumed.
λ> Parameter false : Bool
false is assumed.
λ> Definition DependentSum := fun A : Type 0 => fun B : A -> Type 0 => fun a : A => fun b : B a => fun C : Type 0 => fun f : A -> B a -> C => f a b
DependentSum is assumed.
λ> Check DependentSum
(forall A : Type0 -> (forall B : (A -> Type0) -> (forall a : A -> ((B a) -> (forall C : Type0 -> ((A -> ((B a) -> C)) -> C))))))
λ> Definition JustBool := fun n : Nat => Bool
JustBool is assumed.
λ> Check JustBool
(Nat -> Type0)
λ> Definition pair := DependentSum Nat JustBool z true
pair is assumed.
λ> Check pair Nat (fun x : Nat => fun y : Bool => x)
Nat
λ> Eval pair Nat (fun x : Nat => fun y : Bool => x)
z
λ> Eval pair Bool (fun x : Nat => fun y : Bool => y)
true
```
