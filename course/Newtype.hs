{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor => Monoid Xor where
    mempty = Xor False
    mappend a b = Xor $ getXor a /= getXor b
