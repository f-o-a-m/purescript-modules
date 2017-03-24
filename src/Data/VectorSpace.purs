module Data.VectorSpace
  ( class VectorSpace
  , vzero
  , vadd, (+^)
  , vsub, (-^)
  , vmul, (*^)
  , vmulFlipped, (^*)
  , vnegate
  ) where

import Prelude

-- | Instances must satisfy the following laws:
-- |
-- |  - Associativity of addition: `u +^ (v +^ w) = (u +^ v) +^ w`
-- |  - Commutativity of addition: `u +^ v = v +^ u`
-- |  - Additive identity: `v +^ vzero = vzero +^ v = v`
-- |  - Additive inverse: `v -^ v = vzero`
-- |  - Compatibility with scalar multiplication: `(v *^ a) *^ b = v *^ (a * b)`
-- |  - Scalar multiplicative identity: `v *^ one = v`
-- |  - Distributivity: `(u +^ v) *^ a = u *^ a +^ v *^ a`
-- |  - Distributivity: `v *^ (a + b) = v *^ a + v *^ b`
class (Semiring s) <= VectorSpace v s | v -> s where
  vzero :: v
  vadd  :: v -> v -> v
  vsub  :: v -> v -> v
  vmul  :: v -> s -> v

vmulFlipped :: ∀ v s. VectorSpace v s => s -> v -> v
vmulFlipped = flip vmul

infixl 6 vadd as +^
infixl 6 vsub as -^
infixl 7 vmul as *^
infixl 7 vmulFlipped as ^*

vnegate :: ∀ v s. VectorSpace v s => v -> v
vnegate = (vzero -^ _)
