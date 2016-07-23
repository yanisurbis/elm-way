module Main exposing (..)

import Html  
import String exposing (length, toUpper, left)   

-- main should be html, svg or program
main = 
    wordsCount "hello my darling"
    |> toString
    |> Html.text

add a b =
    a + b

rezult1 =
    add (add 1 2) 3

-- partial functions
-- pipe operator
rezult2 =
    add 1 2
    |> add 3

rezult3 = 
    add 1 2
    |> \a -> a % 2 == 0

exercise1_2 : String -> String
exercise1_2 string = 
    if length string > 10 then
        toUpper string 
        ++ " - name length: " 
        ++ toString (length string)
    else
        string 
        ++ " - name length: " 
        ++ toString (length string)
    
exercise2_1 =
    (~=)

(~=) a b =
    left 1 a == left 1 b

exercise2_2 sentence =
    wordsCount sentence

wordsCount =
    String.words >> List.length