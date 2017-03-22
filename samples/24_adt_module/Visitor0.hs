module Visitor (Mashroom(..), select) where

data Mashroom = White | Red deriving Show

select :: Mashroom -> String
select White = "You keep dreaming."
select Red = "You wake up."
