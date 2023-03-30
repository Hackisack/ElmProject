module Room exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Url
import Url.Parser exposing (Parser)
import Url
import Url.Parser as Parser
import Url.Parser.Query as Query
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode exposing (field)



main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , property : String
    , roomID : String
    , room : MyObject
    , error : String
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
    { results :List MyObject }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "modelInitialValue" ""  { objectId = "", specifiedDates = [""], users = [""], acceptedDates = [""], roomName = "", createdAt ="", updatedAt = ""} "", Cmd.none )


type Msg
    = Msg1
    | Msg2
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | RetrieveUrlID
    | GotData (Result Http.Error MyResults)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        RetrieveUrlID ->
            ( {model | roomID =Maybe.withDefault "Error" (List.head (List.reverse (String.split "=" (Url.toString model.url))))}, getData model)

        GotData (Ok response) ->
           ( { model | room =(List.head (response.results))}, Cmd.none )
            
        GotData (Err _) ->
            ( { model | error = "Error" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Room"
    , body =
        [ div [] [ button [onClick RetrieveUrlID] [ text "Join this Room" ] ]
          ,div [] [ text (model.roomID) ]
          ,div [] [ text (Debug.toString model.room) ]
        ]
    }

-- Placeholders

defaultResults : MyResults
defaultResults =
    { results = [defaultObject ] }

defaultObject : MyObject
defaultObject =
    { objectId = ""
    , specifiedDates = [""]
    , users = [""]
    , acceptedDates = [""]
    , roomName = ""
    , createdAt =""
    , updatedAt = ""
    }

-- Functions

getData :Model -> Cmd Msg
getData model=
    Http.request
    {method = "GET"
    , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
    , url = "https://parseapi.back4app.com/classes/RoomEntry"
    , body = Http.stringBody ("where={\"roomID\":\""           ++ model.roomID ++             "\"}") "application/json"
    , expect = Http.expectJson GotData myResultsDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

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
