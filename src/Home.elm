module Home exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Random exposing (Generator)

import Random.Char
import Random.String exposing (string)
import Http
import Http exposing (Body)
import Http exposing (request)
import Http exposing (expectStringResponse)
import Random.Extra exposing (maybe)
import Random exposing (Generator)
import Random exposing (Generator)
import Random exposing (Generator)
import Maybe exposing (Maybe)
import Http
import Http exposing (Header)



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
    , randomString : String
    , responseString : String
     }


init : () -> ( Model, Cmd Msg )
init _ =
    (Model 1 "" "", Cmd.none )



-- UPDATE


type Msg
    = Rolled String
    | HTTPRequest
    | HTTPResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rolled newValue ->
            ( { model | randomString = newValue }, Cmd.none )

        HTTPRequest ->
            ( model, getData)

        HTTPResponse (Ok response) ->
            ( { model | responseString = response },  Random.generate Rolled (fiveLetterEnglishWord) )

        HTTPResponse (Err error) ->
            ( { model | responseString =  "An Error Occured"}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ ]
        [ div [] [ button [onClick HTTPRequest] [ text "Create New Room" ] ]
        , div [] [text(model.randomString)]
        , div [] [text(model.responseString)]
        ]


fiveLetterEnglishWord : Generator String
fiveLetterEnglishWord =
     string 5 Random.Char.english


getData : Cmd Msg
getData =
    Http.request
    {method = "GET"
    , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
    , url = "https://parseapi.back4app.com/classes/RoomEntry"
    , body = Http.emptyBody
    , expect = Http.expectString HTTPResponse
    , timeout = Nothing
    , tracker = Nothing
    }
   
