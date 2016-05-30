import IOMcn

message :: Bool -> IOMcn String ()
message True = arr reverse >>> putLn
message False = putLn
