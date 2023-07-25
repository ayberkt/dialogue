{-# LANGUAGE ExistentialQuantification #-}

module Dialogue where

data Dialogue x y z = Eta z | Beta (y -> Dialogue x y z) x

eta :: z -> Dialogue x y z
eta z = Eta z

beta :: (y -> Dialogue x y z) -> x -> Dialogue x y z
beta = Beta

dialogue :: Dialogue x y z -> (x -> y) -> z
dialogue (Eta z)      f = z
dialogue (Beta phi x) f = dialogue (phi (f x)) f

data IntDialogue x y z = forall a. IntDialogue { intDialogue :: (z -> a) -> ((y -> a) -> x -> a) -> a }

intEta :: z -> IntDialogue x y z
intEta z = IntDialogue $ \f _ -> f z

intBeta :: (y -> IntDialogue x y z) -> x -> IntDialogue x y z
intBeta phi x = IntDialogue $ \f g -> g (\k -> phi k f g) x
