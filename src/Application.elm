module Application exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Random exposing (Generator)
import Random.Char
import Http
import Browser.Navigation
import Json.Decode exposing (Decoder)
import Json.Decode exposing (field)
import Json.Decode
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Random exposing (Generator)
import Random.String as Rstring
import Random.Char
import Http
import Http exposing (..)
import Http exposing (..)
import Http exposing (..)
import Random exposing (Generator)
import Random exposing (Generator)
import Random exposing (Generator)
import Http
import Http exposing (..)
import Json.Decode exposing (Decoder, field)
import Json.Decode
import Json.Encode as Encode
import Browser.Navigation
import Debug
import Html.Attributes
import Array exposing (Array)



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  ,randomString : String
  , responseString : String
  , rooms : List MyObject
  , avilability : Bool
  }

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


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key url "" "" [] False, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  |Rolled String
  | HTTPRequest
  | GotData (Result Http.Error MyResults)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    Rolled newValue ->
      ( { model | randomString = newValue }, Cmd.none)

    HTTPRequest ->
        ( model, getData)    

    GotData (Ok response) ->
        let
            newModel =
                { model | rooms = response.results, avilability = checkAvilability model MyResults }
        in
        (newModel, Cmd.batch [ Random.generate Rolled tenLetterEnglishWord, createNewRoom model ] )


    GotData (Err _) ->
        ( { model | responseString = "Error" }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Terminplaner"
  , body =
      [div [] [ button [onClick HTTPRequest] [ text "Create New Room" ] ]
          , div [] [text(model.randomString)]
          , div [] [text(model.responseString)]
          , div [] [text(model.rooms |> List.map .roomName |> String.join ", ")]
          , div [] [text (Debug.toString model.avilability)]
          ]
  }


viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]



-- FUNCTIONS

tenLetterEnglishWord : Generator String
tenLetterEnglishWord =
     Rstring.string 10 Random.Char.english

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

sendData : Model -> Cmd Msg
sendData model =
    let
        payload : Encode.Value
        payload =
            Encode.object
                [ ( "roomName", Encode.string model.randomString )
                , ( "specifiedDates",  Encode.list Encode.string ["11.04.2001", "12.04.2001"] )
                , ( "users", Encode.list Encode.string ["test1", "test2"] )
                ]
    in
    Http.request
        {method = "POST"
        , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
        , url = "https://parseapi.back4app.com/classes/RoomEntry"
        , body = Http.jsonBody payload
        , expect = Http.expectWhatever (\_ -> GotData (Ok {results = []}))
        , timeout = Nothing
        , tracker = Nothing
        }

checkAvilability : Model -> (List MyObject -> MyResults) -> Bool
checkAvilability arg1 arg2 =
   not <| List.member arg1.randomString (arg2 arg1.rooms |> .results |> List.map .roomName)

createNewRoom : Model -> Cmd Msg
createNewRoom model =
      if model.avilability == True then
        sendData model
    else
        getData

myObjectDecoder : Decoder MyObject
myObjectDecoder =
    Json.Decode.map7 MyObject
        (field "objectId" Json.Decode.string)
        (field "specifiedDates" (Json.Decode.list Json.Decode.string))
        (field "users" (Json.Decode.list Json.Decode.string))
        (field "acceptedDates" (Json.Decode.list Json.Decode.string))
        (field "roomName" Json.Decode.string)
        (field "createdAt" Json.Decode.string)
        (field "updatedAt" Json.Decode.string)

myResultsDecoder : Decoder MyResults
myResultsDecoder =
    Json.Decode.map MyResults
        (field "results" (Json.Decode.list myObjectDecoder))
