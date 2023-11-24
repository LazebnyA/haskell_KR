data HList a = Atom a | List [HList a] deriving Show

lst1 = List [Atom 1, List [Atom 2, Atom 3], Atom 4]

listSplit :: HList a -> (HList a, HList a)
listSplit (List xs) = (List (getAtomsList (List xs)), List (getOther (List xs)))
  where
    isAtom :: HList a -> Bool
    isAtom (Atom _) = True
    isAtom _ = False

    getAtomsList :: HList a -> [HList a]
    getAtomsList (List []) = []
    getAtomsList (List (x:xs))     = if isAtom x then x : getAtomsList (List xs)
                                                 else getAtomsList (List xs)

    getOther :: HList a -> [HList a]
    getOther (List[]) = []
    getOther (List (x:xs))        = if isAtom x == False then x : getOther (List xs)
                                                         else getOther (List xs)



main = do
  print $ listSplit (lst1)