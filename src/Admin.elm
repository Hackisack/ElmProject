module Admin exposing (..)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Url
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode exposing (field)
import Html.Attributes exposing (type_)
import Html.Attributes exposing (checked)
import Html.Attributes exposing (disabled)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Json.Encode as Encode
import Application exposing (MyResults)
import Browser.Dom exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onCheck)
import Browser.Navigation
import Application exposing (Msg(..))
import Html.Attributes exposing (class)

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
    , allResultsToUse : List MyObject
    , bool : Bool
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
    ( Model key url "modelInitialValue" [{ objectId = "", specifiedDates = [""], users = [""], acceptedDates = [""], roomName = "", createdAt ="", updatedAt = ""}] True, Cmd.none )



type Msg
    = Msg1
    | Msg2
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GetAndShowAll
    | GotData (Result Http.Error MyResults)
    | Delete String
    | EntryDeleted (Result Http.Error String)


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

        GetAndShowAll ->
            ( model, getData )

        GotData (Ok response) ->
            ( { model | allResultsToUse = response.results, bool = False }, Cmd.none)

        GotData (Err _) ->
            ( model, Cmd.none )

        Delete objectId ->
            ( model, deleteEntry objectId )

        EntryDeleted (Ok response) ->
            ( model, Browser.Navigation.reload)

        EntryDeleted (Err _) ->
            ( model, Cmd.none )

        


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [if model.bool then div [] [ button [class "btn2", onClick GetAndShowAll ] [ text "View All Rooms" ] ]
          else
          div [] [ viewTable model.allResultsToUse ]
          ]
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

viewTable : List MyObject -> Html Msg
viewTable objects =
    let
        tableHeader =
            tr []
                [ th [] [ text "Room Name" ]
                , th [] [ text "Users" ]
                , th [] [ text "Object ID" ]
                , th [] [ text "Specified Dates" ]
                , th [] [ text "Created At" ]
                , th [] [ text "Delete" ]
                ]
    in
    table []
        [ tableHeader
        , tbody [] (List.map viewTableRow objects)
        ]

viewTableRow : MyObject -> Html Msg
viewTableRow object =
    let
       objectId = object.objectId
       roomName = object.roomName
       users = String.join ", " object.users
       specifiedDates = String.join ", " object.specifiedDates
       createdAt = object.createdAt
    in
    tr []
        [ td [] [ text roomName ]
        , td [] [ text users ]
        , td [] [ text objectId ]
        , td [] [ text specifiedDates ]
        , td [] [ text createdAt ]
        , td [] [ button [ onClick (Delete objectId) ] [ text "Delete" ] ]
        ]
   

deleteEntry : String -> Cmd Msg
deleteEntry objectID =
    Http.request
        {method = "DELETE"
        , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
        , url = "https://parseapi.back4app.com/classes/RoomEntry/" ++ objectID
        , body = Http.emptyBody
        , expect = Http.expectString EntryDeleted
        , timeout = Nothing
        , tracker = Nothing
        }