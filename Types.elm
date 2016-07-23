module MyTypes exposing (..)

import Html

type alias Element =
    { name: String
    , qty: Float
    , freeQty: Float
    }

cart : List Element
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]

discount : Float -> Float -> Element -> Element
discount whenDiscount discount elm = 
    if elm.qty >= whenDiscount then
        { elm | freeQty = discount}
    else 
        elm

main : Html.Html nothing
main =
    cart
    |> List.map (discount 5 1 >> discount 10 3)
    |> toString 
    |> Html.text