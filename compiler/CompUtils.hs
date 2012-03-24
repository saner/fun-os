module CompUtils where

-- clean name
-- some identifiers are not allowed in assemblers
cleanName :: String -> String
cleanName [] = []
cleanName (l:ls) =
  -- "!#$%&|+-*/:<>=?@^_~,."
  let cL = case l of
            '!' -> "EM"
            '?' -> "QM"
            '-' -> "_"
            _ -> [l]
  in cL ++ (cleanName ls)

