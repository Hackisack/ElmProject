module Home exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Random exposing (Generator)
import Random.String as Rstring exposing(string)
import Random.Char
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
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode




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
    {randomString : String
    , responseString : String
    , rooms : List MyObject
     }


--Start
type alias MyObject =
    { objectId : String
    , specifiedDates : List String
    , users : List String
    , acceptedDates : List String
    , roomName : String
    , createdAt : String
    , updatedAt : String
    }

type alias MyResults =
    { results : List MyObject }
    

myObjectDecoder : Decoder MyObject
myObjectDecoder =
    Decode.map7 MyObject
        (field "objectId" Json.Decode.string)
        (field "specifiedDates" (list Json.Decode.string))
        (field "users" (list Json.Decode.string))
        (field "acceptedDates" (list Json.Decode.string))
        (field "roomName" Json.Decode.string)
        (field "createdAt" Json.Decode.string)
        (field "updatedAt" Json.Decode.string)

myResultsDecoder : Decoder MyResults
myResultsDecoder =
    Decode.map MyResults
        (field "results" (list myObjectDecoder))



--End

init : () -> ( Model, Cmd Msg)
init _ =
    (Model "" "" [], Cmd.none)



-- UPDATE


type Msg
    = Rolled String
    | HTTPRequest
    | HTTPResponse (Result Http.Error (List String))
    | HTTPFailed String
    | GotData (Result Http.Error MyResults)


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        Rolled newValue ->
            ( { model | randomString = newValue }, Cmd.none)

        HTTPRequest ->
            ( model, getData)

        

        HTTPResponse (Err _) ->
            ( { model
                | responseString = "Error"
              }
            , Cmd.none
            )

        HTTPFailed _ ->
            ( { model | responseString = "HTTPFailed" }, Cmd.none )

        GotData (Ok response) ->
            ( { model | rooms = response.results }, Cmd.none )

        HTTPResponse (Ok _) ->
            Debug.todo "branch 'HTTPResponse (Ok _)' not implemented"

        GotData (Err _) ->
            Debug.todo "branch 'GotData (Err _)' not implemented"


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
        , div [] [text(model.rooms |> List.map .roomName |> String.join ", ")]
        ]


fiveLetterEnglishWord : Generator String
fiveLetterEnglishWord =
     Rstring.string 5 Random.Char.english


getData : Cmd Msg
getData =
    Http.request
    {method = "GET"
    , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
    , url = "https://parseapi.back4app.com/classes/RoomEntry"
    , body = Http.emptyBody
    , expect = Http.expectJson GotData myResultsDecoder
    , timeout = Nothing
    , tracker = Nothing
    }
   

checkAvilability : Model -> Bool
checkAvilability model=

    if model.randomString == "[]"
        then True
        else False
        
