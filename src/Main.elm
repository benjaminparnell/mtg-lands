module Main exposing (..)

import Browser
import Css exposing (auto, center, column, displayFlex, flexDirection, justifyContent, margin2, marginBottom, marginTop, pct, px, width)
import Html.Styled exposing (Attribute, Html, button, div, h1, img, p, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (disabled, src, title)
import Html.Styled.Events exposing (onClick)
import Http
import Scryfall exposing (LandType(..))
import Html.Styled exposing (a)
import Html.Styled.Attributes exposing (href, target)



---- MODEL ----


type alias Model =
    { cards : List Scryfall.Card
    , loading : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { cards = [], loading = False }, Cmd.none )



---- UPDATE ----


type Msg
    = GetLands LandType
    | GotLands LandType Int (Result Http.Error Scryfall.CardSearchResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetLands landType ->
            ( { model | cards = [], loading = True }, Scryfall.getLands landType 1 (GotLands landType 1) )

        GotLands landType previousPage result ->
            case result of
                Ok data ->
                    ( { model | cards = model.cards ++ data.data, loading = data.hasMore }
                    , if data.hasMore then
                        Scryfall.getLands landType (previousPage + 1) (GotLands landType (previousPage + 1))

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )


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
    styled div
        [ displayFlex, flexDirection column ]
        []
        [ styled img [ marginBottom (px 10), marginTop (px 10) ] [ src card.image ] []
        , styled div
            []
            []
            [ styled p [ marginTop (px 5), marginBottom (px 5) ] [ title card.setName ] [ text (String.toUpper card.set) ]
            , a [ href card.purchaseUrls.cardmarket, target "_blank" ] [ text "mkm" ]
            ]
        ]


cardRow : List Scryfall.Card -> Html msg
cardRow cards =
    styled div [ displayFlex, justifyContent center ] [] (List.map cardView cards)


loadingRow : Html msg
loadingRow =
    styled div [ marginTop (px 30) ] [] [ text "Loading more cards..." ]


cardGrid : List Scryfall.Card -> Bool -> Html msg
cardGrid cards loading =
    styled div
        []
        []
        ((cards |> chunk 5 |> List.map cardRow)
            ++ (if loading then
                    [ loadingRow ]

                else
                    [ text "" ]
               )
        )


landButton : LandType -> Bool -> Html Msg
landButton landType isDisabled =
    styled button [] [ disabled isDisabled, onClick (GetLands landType) ] [ text (Scryfall.landTypeToString landType) ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "MTG land art" ]
        , landButton Forest model.loading
        , landButton Mountain model.loading
        , landButton Plains model.loading
        , landButton Island model.loading
        , landButton Swamp model.loading
        , cardGrid model.cards model.loading
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
