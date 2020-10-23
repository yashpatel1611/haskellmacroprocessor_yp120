module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp s l
  = [x | (c, x) <- l, c == s]



-- splitText function takes in a char array and a string
-- outputs a tuple of string and string array
-- This function seperates a string using the chars given
-- Uses recursion to go through list, checking if each char is
-- part of the separators given
-- If it is, it will append the special char into the first part of the tuple
-- If not, it will append the char into the second part of the tuple's head
splitText :: [Char] -> String -> (String, [String])
splitText cs s@(x : xs)
  | isChar x = (p, ([x] ++ start) : end)
  | otherwise = ((x : p), "" : s')
  where
    isChar y
      | elem y cs = False
      | otherwise = True
    (p, s') = splitText cs xs
    start = head s'
    end = tail s'

-- Base case for splitText where separators are given but
-- a string is not
splitText _ ""
  = ("", [""])

-- Combine function which is the reverse of splitText function
-- This function takes in a string and a string array and outputs
-- a string array
-- The function will take the given string and place each character in
-- the string back into the string array
-- Uses recursion to go through list
combine :: String -> [String] -> [String]
combine (x : xs) (y : ys)
  = y : head ([[x] : (x')])
  where
    x' = combine xs ys

-- Base case for combine function where string is not given
-- but a string array is given
combine "" s
  = s

-- getKeywordDefs function which takes in a string array
-- and returns KeywordDefs
-- Uses recursion to go through list and splitText to separate
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs (y : ys)
  = (kw, (unwords kd)) : kwd'
  where
    (_, (kw : kd)) = splitText " \n" y
    kwd' = getKeywordDefs ys

-- Base case for getKeywordDefs which does not match the last one
getKeywordDefs _
  = []  

-- Expand function which takes in 2 inputs of FileContents type
-- and returns a FileContents type (i.e. string)
expand :: FileContents -> FileContents -> FileContents
-- Base case for expand function when string given is empty
expand "" _
  = ""

-- Expand function where s and defs are not empty
-- First the string is separated into words and joined together
-- This creates a list where each index in the list corresponds to either
-- a word or a special character
-- Next, through list comprehension, the function replaceWord is applied on every x
-- from c
-- Finally, using foldr1 and the (++) operator, all extra whitespace is removed,
-- leaving with the final result.
expand s defs
  = foldr1 (++) ([replaceWord x (getKeywordDefs defs') | x <- c])
  where
    (_, defs') = splitText "\n" defs
    (a, b) = splitText separators s
    c = combine a b



-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- Implemented it through recursion
-- This function will take in a string and compare it to each keyworddef
-- in the definitions
replaceWord :: String -> KeywordDefs -> String

replaceWord s (d : ds)
  | s == x = y
  | otherwise = replaceWord s ds
  where
    (x, y) = d

replaceWord s []
  = s
-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
