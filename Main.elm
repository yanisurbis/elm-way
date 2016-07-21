module Main exposing (..)

import Html  
import String exposing (length, toUpper)   

-- main should be html, svg or program
main = 
    Html.text (exercise2 "James Moore")

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

exercise2 : String -> String
exercise2 string = 
    if length string > 10 then
        toUpper string 
        ++ " - name length: " 
        ++ toString (length string)
    else
        string 
        ++ " - name length: " 
        ++ toString (length string)
