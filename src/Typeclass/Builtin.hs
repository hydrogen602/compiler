module Typeclass.Builtin where
import           Core.Types (FunctionName (FunctionName))

-- * Should include build-in typeclasses. Currently TODO - done in an ad hoc way until I come up with a system.

-- Destroyable

destroy :: FunctionName
destroy = FunctionName "destroy"
