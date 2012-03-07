module SchemeDataTypes where

data SchemeInst =  Atom String
                 | List [SchemeInst]
                 | DottedList [SchemeInst] SchemeInst
                 | Number Int
                 | String String
                 | Bool Bool
                 | EmptyVal
                 deriving (Show, Eq)
  

data SchemeType = TInt
                | TBool
                | TVoid
                deriving (Show, Eq)

