import Data.List

splitBy :: String -> String -> [String]
splitBy splitter str_to_split = splitByHelper splitter str_to_split []
    where
    splitByHelper :: String -> String -> [String] -> [String]
    splitByHelper splitter str_to_split accum
        | not (isInfixOf splitter str_to_split) && not (null str_to_split) = reverse (str_to_split : accum)
        | str_to_split == "" = reverse accum
        | otherwise          =
            let word = takeWhile (/= head splitter) str_to_split  -- Extract the word until the next splitter
            in splitByHelper splitter (drop (length word + length splitter) str_to_split) (word : accum)



main = do
  print $ splitBy "||" "asdsad||asdasd||asddds"

