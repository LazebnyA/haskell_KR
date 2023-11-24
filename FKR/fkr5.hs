
data Tree a = Node a (Tree a) (Tree a) | Empty

instance Show a => Show (Tree a)
  where
    show Empty = "[]"
    show (Node x left right) = showTree 1 (Node x left right)
      where
        indent :: Int -> String
        indent n
           | n == 0 = ""
           | otherwise = "   " ++ indent (n-1)

        showTree :: (Show a) => Int -> Tree a -> String
        showTree n Empty = indent n ++ "Empty" ++ "\n"
        showTree n (Node x left right) =
           indent n ++ "Node " ++ show x ++ "\n" ++
           showTree (n+1) left ++ "\n" ++
           showTree (n+1) right



bf_search :: Tree a -> [a]
bf_search tree = bf_search_helper [tree]
  where
    bf_search_helper :: [Tree a] -> [a]
    bf_search_helper []        = []
    bf_search_helper treeNodes = map getValue (filter notEmpty treeNodes) ++ bf_search_helper (concatMap getChildren treeNodes)

    getValue (Node value _ _) = value

    notEmpty (Node _ _ _) = True
    notEmpty Empty        = False

    getChildren (Node _ left right) = [left, right]
    getChildren Empty               = []



treeExample =
 Node 2
  (Node 1
    (Node 3
          Empty
          Empty
        )
    Empty
  )
  (Node 5
    Empty

    (Node 4
      Empty
      Empty
    )
  )

main = do
  print(treeExample)
  print(bf_search treeExample)
