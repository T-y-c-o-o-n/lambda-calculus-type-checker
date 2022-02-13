# lambda-calculus-type-checker

## Grammar

### Type

`τ ::= α | τ → τ | ∀α. τ`

### Term

`e ::= x | e e | e τ | λx : τ. e | Λα. e`
