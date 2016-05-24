data DT = DT Integer deriving Show

newtype NT = NT Integer deriving Show

checkDT :: DT -> String
checkDT (DT _) = "OK!"

checkNT :: NT -> String
checkNT (NT _) = "OK!"
