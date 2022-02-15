# lambda-calculus-type-checker

type checker for simply typed lambda calculus and polymorphic lambda calculus (system F)

## Grammar

### Type

`τ ::= α | τ → τ | ∀α. τ`

### Term

`e ::= x | e e | e τ | λx : τ. e | Λα. e`

## Input

One line: firstly, context (list of pairs "<*variable*> : <*type*>", separated by ","), then "|-" and then "<*term*> : <*type*>"

## Output

Parse error if input is incorrect, "Correct!" if term have this type on this context, error message otherwise

## Examples

| Input                                                               | Output                                                                                   |
|---------------------------------------------------------------------|------------------------------------------------------------------------------------------|
| `⊢ x : a`                                                           | `Correct!`                                                                               |
| `⊢ x : b -> b`                                                      | `Correct!`                                                                               |
| `⊢ x y : a`                                                         | `Correct!`                                                                               |
| `⊢ \x : a . x : a`                                                  | `error: "Occurs check: a = a -> a"; actual type is a -> a`                               |
| `⊢ \x : a . x : b`                                                  | `error: "Couldn't match expected type a -> a with actual type b"; actual type is a -> a` |
| `x : a ⊢ x : a`                                                     | `Correct!`                                                                               |
| `x : a ⊢ x : b`                                                     | `error: "Couldn't match expected type a with actual type b"; actual type is a`           |
| `x : a -> b -> c, y : a -> b, z : a ⊢ x z (y z) : c`                | `Correct!`                                                                               |
| `⊢  x : @a. a`                                                      | `Correct!`                                                                               |
| `⊢ /\ a. \f : a -> a. \x : a. f (f (f x)) : @a. (a -> a) -> a -> a` | `Correct!`                                                                               |
