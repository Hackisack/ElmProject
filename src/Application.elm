module Application exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Random exposing (Generator)
import Random.Char
import Http
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
import Debug
import Json.Decode exposing (null)



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
  , formFieldsDate : List String
  , newFieldDate : String
  , roomCreated : String
  , roomCreatedString : String
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

type alias MyCreation =
    { objectId : String
    , createdAt : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  ( Model key url "" "" [] False [] "" "" "", Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  |Rolled String
  | HTTPRequest
  | GotData (Result Http.Error MyResults)
  | AddField
  | ResetList
  | UpdateNewField String
  | RoomCreation (Result Http.Error MyCreation)


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
      ( { model | randomString = newValue},Cmd.none)

    HTTPRequest ->
        ( model,Cmd.batch[getData , Random.generate Rolled tenLetterEnglishWord])    

    GotData (Ok response) -> 
        ( { model | rooms = response.results}, createRoom model)

    GotData (Err _) ->
        ( { model | responseString = "Error" }, Cmd.none )

    RoomCreation (Ok response) -> --TODO extract real path

        ( { model | roomCreated = model.url.host ++ ":" ++  Maybe.withDefault "" (Maybe.map String.fromInt model.url.port_)++ String.dropRight 16 model.url.path ++ "Room.html?roomID=" ++ model.randomString, roomCreatedString = "Room was created. Visit this Link to join: " }, Cmd.none )

    RoomCreation (Err _) ->
        ( { model | roomCreated = "Error, please try again" }, Cmd.none )

    AddField ->
        ({ model | formFieldsDate = model.formFieldsDate ++ [ model.newFieldDate ], newFieldDate = "" }, Cmd.none)

    UpdateNewField newField ->
        ({ model | newFieldDate = newField }, Cmd.none)

    ResetList ->
        ({ model | formFieldsDate = [] }, Cmd.none)




        




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Terminplaner"
  , body =
      [    div [class "containerList"] [Html.ul [class "listUL"] (List.map (\field -> Html.li [class "listLI"] [Html.text field]) model.formFieldsDate)] --show all existing fields
          ,div [class "container2"] [ input [ type_ "text", value model.newFieldDate, onInput UpdateNewField, placeholder "YOUR CHOICE" ] []] --show new fields
          ,div [class "container2"] [button [class "btn2" , onClick AddField ] [ text "Add Date/Event" ]]
          ,div [class "container2"] [button [class "btn2" , onClick ResetList ] [ text "Reset List" ]]
          ,div [class "container"] [ button [class "btn" ,onClick HTTPRequest] [ text "Create New Room" ] ]
          ,div [] [p [][text model.roomCreatedString]  ,a [href model.roomCreated, target "_blank"] [text model.roomCreated]]
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

sendData : Model -> Bool -> Cmd Msg
sendData model bool =
  if bool then
    let
        payload : Encode.Value
        payload =
            Encode.object
                [ ( "roomName", Encode.string model.randomString )
                , ( "specifiedDates",  Encode.list Encode.string model.formFieldsDate )
                , ( "users", Encode.list Encode.string [] )
                , ( "acceptedDates", Encode.list Encode.string [] )
                ]
    in
    Http.request
        {method = "POST"
        , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
        , url = "https://parseapi.back4app.com/classes/RoomEntry"
        , body = Http.jsonBody payload
        , expect = Http.expectJson RoomCreation roomCreationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }
  else
    Cmd.none

checkAvilability : Model -> (List MyObject -> MyResults) -> Bool
checkAvilability arg1 arg2 =
   not <| List.member arg1.randomString (arg2 arg1.rooms |> .results |> List.map .roomName)

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

formFieldView : String -> Html Msg
formFieldView field =
    div [] [ input [ type_ "text", value field ] [] ]

roomCreationDecoder : Decoder MyCreation
roomCreationDecoder =
    Json.Decode.map2 MyCreation
        (field "objectId" Json.Decode.string)
        (field "createdAt" Json.Decode.string)

createRoom : Model -> Cmd Msg
createRoom model =
    let
        avilable =
            checkAvilability model MyResults

        sendDataCmd =
            sendData model avilable
    in
    sendDataCmd

-- Response if rrom is created sucessfully 
-- {"objectId":"sUFyQvrR8w","createdAt":"2023-04-04T14:05:28.530Z"}
