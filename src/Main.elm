module Main exposing (..)

import Browser
import Css exposing (auto, center, displayFlex, justifyContent, margin2, pct, px, width)
import Html.Styled exposing (Attribute, Html, button, div, h1, img, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (src)
import Html.Styled.Events exposing (onClick)
import Http
import Scryfall exposing (LandType(..))



---- MODEL ----


type alias Model =
    { cards : List Scryfall.Card
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = GetLands LandType
    | GotLands LandType Int (Result Http.Error Scryfall.CardSearchResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetLands landType ->
            ( { model | cards = [] }, Scryfall.getLands landType 1 (GotLands landType 1) )

        GotLands landType previousPage result ->
            case result of
                Ok data ->
                    ( { model | cards = model.cards ++ data.data }
                    , if data.hasMore then
                        Scryfall.getLands landType (previousPage + 1) (GotLands landType (previousPage + 1))

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


chunk : Int -> List a -> List (List a)
chunk size list =
    case List.take size list of
        [] ->
            []

        head ->
            head :: chunk size (List.drop size list)



---- VIEW ----


cardView : Scryfall.Card -> Html msg
cardView card =
    div []
        [ img [ src card.image ] []
        ]


cardRow : List Scryfall.Card -> Html msg
cardRow cards =
    styled div [ displayFlex, justifyContent center ] [] (List.map cardView cards)


cardGrid : List Scryfall.Card -> Html msg
cardGrid cards =
    styled div [] [] (cards |> chunk 5 |> List.map cardRow)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Lets see some lands" ]
        , styled button [] [ onClick (GetLands Forest) ] [ text "Forest" ]
        , styled button [] [ onClick (GetLands Mountain) ] [ text "Mountain" ]
        , styled button [] [ onClick (GetLands Plains) ] [ text "Plains" ]
        , styled button [] [ onClick (GetLands Island) ] [ text "Island" ]
        , styled button [] [ onClick (GetLands Swamp) ] [ text "Swamp" ]
        , cardGrid model.cards
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
