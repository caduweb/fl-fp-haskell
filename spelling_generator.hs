
-- Generate the spelling text for a list of words
speller :: [String] -> String
speller xs = foldl (\acc (i, w) -> concat_phrases acc (letter_phrase w) i last_i) "" 
             (zip [0..] xs)
             where last_i = length xs - 1

-- Concatenate phrases
concat_phrases :: String -> String -> Int -> Int -> String
concat_phrases ps p i last_i
  | i == 0      = ps ++ p
  | i == last_i = ps ++ ", and " ++ p
  | otherwise   = ps ++ ", " ++ p

-- Generate the letter phrase for a word
letter_phrase :: String -> String
letter_phrase w
  | null w = []
  | otherwise = head w : (" is for " ++ w)

