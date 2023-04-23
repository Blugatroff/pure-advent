module Data.Primitive (class Primitive, class PrimitiveKey, primitiveKey) where

class Primitive :: Type -> Constraint
class Primitive t

instance primitiveInt :: Primitive Int
instance primitiveString :: Primitive String

class Primitive t <= PrimitiveKey k t | k -> t where
  primitiveKey :: k -> t

