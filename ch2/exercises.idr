palindrome1 : String -> Bool
palindrome1 str = str == reverse str

palindrome2 : String -> Bool
palindrome2 str = toLower str == reverse (toLower str)

palindrome3 : String -> Bool
palindrome3 str = length str > 10 && toLower str == reverse (toLower str)

palindrome4 : Nat -> String -> Bool
palindrome4 len str = length str > len && toLower str == reverse (toLower str)

||| Returns (word count, char count) for a given input String.
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten as = take 10 (reverse (sort as))

||| Filters from a list then counts the remaining elements.
over_length1 : Nat -> List String -> Nat
over_length1 len strs = length (filter (is_over len) strs)
  where
    is_over : Nat -> String -> Bool
    is_over len str = length str > len

||| Folds a list, incrementing the counter for every element that is long
||| enough.
over_length2 : Nat -> List String -> Nat
over_length2 len strs = foldl (inc_if_over len) 0 strs
  where
    inc_if_over : Nat -> Nat -> String -> Nat
    inc_if_over len acc str = if length str > len then acc + 1 else acc

{-
||| Main function for a palindrome checking repl. This uses where syntax to
||| define a local function.
main : IO ()
main = repl "Enter a string: " print
  where
    print: String -> String
    print word = show (palindrome1 word) ++ "\n"

||| Main function for a word and char counting repl. This uses an anonymous
||| helper function.
main : IO ()
main = repl "Enter a string: " (\x : String => show (counts x) ++ "\n")
-}
