module Data.Functor.Constraint.Jop where


-- | Like `Op`, but holds the values, is a `Functor`, etc
newtype Jop a b = Jop { runJop :: (b, (b -> a) -> a) }

instance Eq b => Eq (Jop a b) where
  x == y = extractJopR x == extractJopR y

instance Ord b => Ord (Jop a b) where
  compare x y = compare (extractJopR x) (extractJopR y)

instance Show b => Show (Jop a b) where
  showsPrec n = showsPrec n . extractJopR


instance Functor (Jop a) where
  fmap f (Jop (x, xs)) = Jop (f x, xs . (. f))


-- | Make a `Jop`
mkJop :: b -> Jop a b
mkJop x = Jop (x, ($ x))

-- | Apply the inner function to the given function
extractJopL :: (b -> a) -> Jop a b -> a
extractJopL f ~(Jop (_, g)) = g f

-- | Extract the return value from `Jop`
extractJopR :: Jop a b -> b
extractJopR   ~(Jop (x, _)) = x


