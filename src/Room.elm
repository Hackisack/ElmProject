module Room exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Url
import Url
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode exposing (field)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Attributes exposing (disabled)

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
init _ url key =
    ( Model key url "modelInitialValue" "default"  { objectId = "", specifiedDates = [""], users = [""], acceptedDates = [""], roomName = "", createdAt ="", updatedAt = ""} "", Cmd.none )


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
            ( {model | roomID =Maybe.withDefault "Error" (List.head (List.reverse (String.split "=" (Url.toString model.url))))}, getData)

        GotData (Ok response) ->
           ( { model | room =(Maybe.withDefault defaultObject (findRightRoom model.roomID response))}, Cmd.none )
            
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
          ,div [] [ viewTable model.room ]
        ]
    }

-- Placeholder

defaultObject : MyObject
defaultObject =
    { objectId = "Default"
    , specifiedDates = ["Default"]
    , users = ["Default"]
    , acceptedDates = ["Default"]
    , roomName = "Default"
    , createdAt ="Default"
    , updatedAt = "Default"
    }

-- Functions



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

findRightRoom : String -> MyResults -> Maybe MyObject
findRightRoom roomID myResults =
    List.filter (\obj -> obj.roomName == roomID) myResults.results
        |> List.head


-- TEST SPACE

viewTable : MyObject -> Html Msg
viewTable room =
    let
        dates = room.specifiedDates
        users = room.users
        rows =
            List.map (\user -> tr [] (td [] [ text user ] :: List.map (\date -> td [] [ checkbox date user room.acceptedDates ]) dates)) users
    in
    table []
        (tr [] (th [] [ text "Users" ] :: List.map (\date -> th [] [ text date ]) dates)
        :: rows)

checkbox : String -> String -> List String -> Html Msg
checkbox date user acceptedDates =
    let
        isChecked =
            List.member user acceptedDates && List.member date acceptedDates
    in
    input
        [ type_ "checkbox"
        , checked isChecked
        , disabled False
        ]
        []






