{-# LANGUAGE RankNTypes #-}

module Dialogue where

data Dialogue x y z = Eta z | Beta (y -> Dialogue x y z) x

example1 :: Dialogue Int Int Int
example1 = Eta 2

example2 :: Dialogue Int Int Int
example2 = Beta (\_ -> Eta 7) 3

eta :: z -> Dialogue x y z
eta z = Eta z

beta :: (y -> Dialogue x y z) -> x -> Dialogue x y z
beta = Beta

dialogue :: Dialogue x y z -> (x -> y) -> z
dialogue (Eta z)      f = z
dialogue (Beta phi x) f = dialogue (phi (f x)) f

type IntDialogue x y z = forall a. (z -> a) -> ((y -> a) -> x -> a) -> a

intEta :: z -> IntDialogue x y z
intEta z = \f _ -> f z

intBeta :: (y -> IntDialogue x y z) -> x -> IntDialogue x y z
intBeta phi x = \f g -> g (\y -> phi y f g) x

toInternal :: Dialogue x y z -> IntDialogue x y z
toInternal (Eta z)      = intEta z
toInternal (Beta phi x) = intBeta (\y -> toInternal (phi y)) x

toExternal :: IntDialogue x y z -> Dialogue x y z
toExternal d = d Eta Beta

modulusAt :: (Int -> y) -> Dialogue Int y z -> Int
modulusAt f (Eta _)      = 0
modulusAt f (Beta phi n) = max n (modulusAt f (phi (f n)))
