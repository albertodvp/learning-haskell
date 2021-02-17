-- |

module Ask where


newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra


ask :: Reader a a
ask = Reader id
