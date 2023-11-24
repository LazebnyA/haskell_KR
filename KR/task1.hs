import Data.List -- used in problem 1, 2
import Data.Ord -- used in problem 2

{- PROBLEM 1
It is often needed to convert a number written in arabic symbols,
to a string of its textual representation, i.e. for financial documents.
Write a function intToWords that transcribes an integer into its
textual representation in format "digit-digit-digit-...".
-}

intToWords :: (Num a, Show a) => a -> String
intToWords x = intercalate "-" listOfDigitsAsWords
  where
    listOfDigitsAsWords = map digitToStr (show x)
    digitToStr :: Char -> String
    digitToStr y =
      case y of
        '0' -> "zero"
        '1' -> "one"
        '2' -> "two"
        '3' -> "three"
        '4' -> "four"
        '5' -> "five"
        '6' -> "six"
        '7' -> "seven"
        '8' -> "eight"
        '9' -> "nine"
        '-' -> "minus"
        _   -> error "Invalid input value, it must've been a number"

problem1 = do
  putStrLn "PROBLEM 1:"

  print $ intToWords  150  -- "one-five-zero"
  print $ intToWords    0  -- "zero"
  print $ intToWords (-10) -- "minus-one-zero"

  putStrLn ""



{- PROBLEM 2
Write a function findMaxFrequency that for a given homogenous list of type a
returns a pair (a, Int) of the most frequent element (any, if there are more than one) and its frequency. For an empty list throw an error.
-}

findMaxFrequency :: (Ord a) => [a] -> (a, Int)
findMaxFrequency [] = error "ERROR: Empty List"
findMaxFrequency lst = maximumBy (comparing snd) freqsMap
  where
    freqsMap = map (\x -> (head x, length x)) (group (sort lst))

problem2 = do
  putStrLn "PROBLEM 2:"
  print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
  print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
  print $ findMaxFrequency "some sentence" -- ('e', 4)
  -- print $ findMaxFrequency ([] :: [Char])   -- error

  putStrLn ""



{- PROBLEM 3
For a given system of types that represent a file system structure
write a function search that given a name returns a list of all paths
that correspond to that name.
-}

type Name   = String
type Path   = String
data FSNode = File Name | Dir Name [FSNode]

search :: Name -> FSNode -> [Path]
search name root = searchHelper name root "/"
  where
    searchHelper :: Name -> FSNode -> Path -> [Path]
    searchHelper name (File n) path = [path ++ "/" ++ n | n == name]
    searchHelper name (Dir n children) path =
      let childPaths         = map (\child -> searchHelper name child (path ++ n)) children
          matchingChildPaths = filter (not . null) childPaths
      in
        if n == name
          then [path ++ "/" ++ n] ++ concat matchingChildPaths
        else concat matchingChildPaths

root = Dir "/"
  [
    Dir "folder1"
    [
      File "file1",
      Dir  "folder2"
      [
        File "file2",
        File "file3"
      ],
      Dir  "folder3"
      [
        File "file3",
        File "file4"
      ],
      File "file5"
    ]
  ]

problem3 = do
  putStrLn "PROBLEM 3:"
  print $ search "file1" root -- ["//folder1/file1"]
  print $ search "file3" root -- ["//folder1/folder2/file3", "//folder1/folder3/file3"]
  print $ search "file4" root -- ["//folder1/folder3/file4"]
  print $ search "file6" root -- []

-- please, make sure your code runs without errors
-- comment out unsolved tasks here
main = do
  problem1
  problem2
  problem3