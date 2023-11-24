binary_to_decimal :: String -> Int
binary_to_decimal []         = 0
binary_to_decimal all@(x:xs)
                         | x == '1' = 2 ^ (length(all) - 1) + binary_to_decimal xs
                         | x == '0' = binary_to_decimal xs
                         | otherwise = error("It is not a decimal number")


main = do
  print $ binary_to_decimal("000110")