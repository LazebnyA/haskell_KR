findMaxDistance :: (Ord a, Floating a) => [(a, a)] -> a
findMaxDistance [] = error "Empty list"
findMaxDistance pairs = maximum $ map (\x -> findMaxForPoint x pairs 0) pairs
  where
    findMaxForPoint _ [] accum   = accum
    findMaxForPoint point (x:xs) accum = findMaxForPoint point xs (max (findDistance point x) accum)

    findDistance :: Floating a => (a, a) -> (a, a) -> a
    findDistance (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2)



main = do
  print (findMaxDistance [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)])
  print (findMaxDistance [(1.0, 1.0), (2.0, 2.0), (3.0, 3.0)])
  print (findMaxDistance [(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), (4.0, 4.0)])