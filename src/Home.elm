module Home exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Random
import Random exposing (Generator)

import Random.Char
import Random.String exposing (string)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { value : Int
    , rString : String
     }


init : () -> ( Model, Cmd Msg )
init _ =
    (Model 1 "" , Cmd.none )



-- UPDATE


type Msg
    = Rolled String
    | NewString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rolled newValue ->
            ( { model | rString = newValue }, Cmd.none )

        NewString ->
            ( model, Random.generate Rolled (fiveLetterEnglishWord) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ ]
        [ div [] [ button [ onClick NewString ] [ text "Roll The Dice" ] ]
        , div [] [text(model.rString)]
        ]


fiveLetterEnglishWord : Generator String
fiveLetterEnglishWord =
     string 5 Random.Char.english