type Set a = a -> Bool

belongs :: Eq a => Set a -> a -> Bool
belongs s x = s x

empty :: Set a
empty = \x -> False

singleton :: a -> Set a
singleton x = \y -> y == x

union :: Set a -> Set a -> Set a
union f g = \x -> f x || g x

intersection :: Set a -> Set a -> Set a
intersection f g = \x -> f x && g x

