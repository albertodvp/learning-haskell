{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module TypeLit () where

import           Data.Proxy   (Proxy (..))
import qualified GHC.Natural
import           GHC.TypeLits (KnownNat, KnownSymbol, Symbol (..), natVal,
                               symbolVal)

newtype Pointer (align :: GHC.Natural.Natural) = Pointer Integer


zeroPtr :: Pointer align
zeroPtr = Pointer 0

inc :: Pointer align -> Pointer align
inc (Pointer p) = Pointer $ p + 1

ptrValue :: forall align. KnownNat align => Pointer align -> Integer
ptrValue (Pointer p) = p * natVal (Proxy :: Proxy align)

maybePtr :: forall align. KnownNat align => Integer -> Maybe (Pointer align)
maybePtr i
  | rem == 0 = Just $ Pointer quot
  | otherwise = Nothing
  where
    (quot, rem) = divMod i $ natVal (Proxy :: Proxy align)



data SuffixedString (suffix :: Symbol) = SS String

suffixed :: String -> SuffixedString suffix
suffixed = SS

asString :: forall symbol. KnownSymbol symbol => SuffixedString symbol -> String
asString (SS s) = s ++ "@" ++ symbolVal (Proxy :: Proxy symbol)
