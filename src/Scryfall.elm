module Scryfall exposing (Card, CardSearchResponse, LandType(..), getLands, landTypeToString)

import Http
import Json.Decode exposing (bool, field, int, list, map, map4, map6, maybe, string)


type LandType
    = Forest
    | Mountain
    | Swamp
    | Plains
    | Island


type alias PurchaseURLs =
    { cardmarket : String
    }


type alias Card =
    { id : String
    , name : String
    , set : String
    , setName : String
    , image : String
    , purchaseUrls : PurchaseURLs
    }


type alias CardSearchResponse =
    { totalCards : Int
    , hasMore : Bool
    , nextPage : Maybe String
    , data : List Card
    }


landTypeToString : LandType -> String
landTypeToString landType =
    case landType of
        Forest ->
            "Forest"

        Mountain ->
            "Mountain"

        Island ->
            "Island"

        Swamp ->
            "Swamp"

        Plains ->
            "Plains"


purchaseURIDecoder : Json.Decode.Decoder PurchaseURLs
purchaseURIDecoder =
    map PurchaseURLs
        (field "cardmarket" string)


cardDecoder : Json.Decode.Decoder Card
cardDecoder =
    map6 Card
        (field "id" string)
        (field "name" string)
        (field "set" string)
        (field "set_name" string)
        (field "image_uris" (field "border_crop" string))
        (field "purchase_uris" purchaseURIDecoder)


cardSearchResponseDecoder : Json.Decode.Decoder CardSearchResponse
cardSearchResponseDecoder =
    map4 CardSearchResponse
        (field "total_cards" int)
        (field "has_more" bool)
        (maybe (field "next_page" string))
        (field "data" (list cardDecoder))


getLands : LandType -> Int -> (Result Http.Error CardSearchResponse -> msg) -> Cmd msg
getLands landType page msg =
    Http.get
        { url = "https://api.scryfall.com/cards/search?order=released&unique=prints&q=!\"" ++ landTypeToString landType ++ "\"&page=" ++ String.fromInt page
        , expect = Http.expectJson msg cardSearchResponseDecoder
        }
