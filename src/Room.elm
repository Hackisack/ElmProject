module Room exposing (..)

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
    , allResultsToUse : List MyObject
    , bool : Bool
    , checkboxes : List String
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
    ( Model key url "modelInitialValue" "default"  { objectId = "", specifiedDates = [""], users = [""], acceptedDates = [""], roomName = "", createdAt ="", updatedAt = ""} "" [] True [], Cmd.none )


type Msg
    = Msg1
    | Msg2
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | RetrieveUrlID
    | GotData (Result Http.Error MyResults)
    | NamePushed (Result Http.Error String)
    | UserUpdated String
    | CheckboxChecked Int String
    | CreateList
    | PushData

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
           let
            newResult =
                Maybe.withDefault defaultObject (findRightRoom model.roomID response)
            in
            let
                newResults =
                    newResult :: model.allResultsToUse
            in
            ( { model | room = newResult, allResultsToUse = newResults, bool = False, checkboxes = createEmptyCheckbox model }
            , Cmd.none
            )
            
        GotData (Err _) ->
            ( { model | error = "Error" }, Cmd.none )

        UserUpdated value ->
            ( { model | property = value }, Cmd.none )

        NamePushed (Ok response) ->
            ( model, Browser.Navigation.reload )

        NamePushed (Err _) ->
            ( model, Cmd.none )

        CheckboxChecked int string ->
            ( { model | checkboxes =  createCheckbox model int string } , Cmd.none )

        CreateList ->
            ( model, Cmd.none )

        PushData ->
            ( model,pushData model model.allResultsToUse)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Room"
    , body =
        [ if model.bool then div [] [ button [ onClick RetrieveUrlID ] [ text "View this Room" ] ]
          else
              
          --,div [] [ form [] [ input [type_ "text", placeholder "Your Name", onInput FieldUpdated] [] ] ]
          div [] [ viewTable model.room, button [ onClick PushData ] [ text "Send your choice" ] ]
          
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

viewTable : MyObject -> Html Msg
viewTable room =
    let
        dates = room.specifiedDates
        users = room.users
        rows =
            List.map (\user -> tr [] (td [] [ text user ] :: List.map (\date -> td [] [ checkbox date user room.acceptedDates dates users ]) dates)) users
        newRow =
            tr []
                (td [] [ input [ type_ "text", placeholder "Your Name", onInput UserUpdated ][] ]
                :: List.indexedMap (\index date -> td [] [ input[ type_ "checkbox", disabled False, onCheck (\isChecked -> if isChecked then CheckboxChecked index date else CheckboxChecked index date)][]]) dates)


    in
    table []
        (tr [] (th [] [ text "Users" ] :: List.map (\date -> th [] [ text date ]) dates)
        :: rows ++ [ newRow ])




checkbox : String -> String -> List String -> List String -> List String -> Html Msg
checkbox date user acceptedDates dates users =
    let
        -- Calculate the index of the user in the list of users
        userIndex = List.indexedMap Tuple.pair users |> List.filterMap (\(i, u) -> if u == user then Just i else Nothing) |> List.head |> Maybe.withDefault 0

        -- Calculate the start and end indices of the slice of accepted dates that belong to the current user
        sliceStart = userIndex * List.length dates
        sliceEnd = (userIndex + 1) * List.length dates

        -- Extract the slice of accepted dates that belong to the current user
        acceptedDatesForUser = List.take (sliceEnd - sliceStart) (List.drop sliceStart acceptedDates)

        -- Check if the current date is in the slice of accepted dates for the current user
        isChecked = List.member date acceptedDatesForUser
    in
    input
        [ type_ "checkbox"
        , checked isChecked
        , disabled True
        ]
        []

pushData : Model -> List MyObject -> Cmd Msg
pushData model myObjects =
    let
        matchingObjectId =
            findMatchingObjectId model myObjects

        updatedObjects =
            case matchingObjectId of
                objectId ->
                    List.map
                        (\obj ->
                            if obj.objectId == objectId then
                                {obj | users = obj.users ++ [model.property], acceptedDates = obj.acceptedDates ++ model.checkboxes }
                            else
                                obj
                        )
                        myObjects
  in
  let
    payload : Encode.Value
    payload =
        Encode.object
            [ ( "users", Encode.list Encode.string (List.concatMap .users updatedObjects) )
            , ( "acceptedDates", Encode.list Encode.string (List.concatMap .acceptedDates updatedObjects) )
            ]
  in
    Http.request
        {method = "PUT"
        , headers = [Http.header "X-Parse-Application-Id" "58G7kMmJiXqTEW6MCENwiLb6H8ebaiCJX3ahL91c", Http.header "X-Parse-REST-API-Key" "elB9iy4qqTAHzWxdQtFTqRsm84tTRctjyAmMyIBO"]
        , url = "https://parseapi.back4app.com/classes/RoomEntry/" ++ matchingObjectId
        , body = Http.jsonBody payload
        , expect = Http.expectString NamePushed
        , timeout = Nothing
        , tracker = Nothing
        }

findMatchingObjectId : Model -> List MyObject -> String
findMatchingObjectId model myObjects =
    let
        matchingObjects =
            List.filter (\obj -> obj.roomName == model.roomID) myObjects
    in
    case matchingObjects of
        (matchingObject :: _) ->
             matchingObject.objectId
        _ ->
            Debug.toString myObjects

createCheckbox : Model -> Int -> String -> List String
createCheckbox model int string =
    let
        specifiedDates = model.room.specifiedDates
        checkboxes = model.checkboxes
        (before, after) = split int checkboxes
        (newCheckbox, rest) =
            case after of
                hd :: tl ->
                    if hd == string then
                        ("0", tl)
                    else
                        (string, tl)
                [] ->
                    (string, [])
    in
    before ++ [newCheckbox] ++ rest ++ List.repeat (List.length specifiedDates - List.length (before ++ [newCheckbox] ++ rest)) "0"

createEmptyCheckbox : Model -> List String
createEmptyCheckbox model =
    let
        specifiedDates = model.room.specifiedDates
    in
    List.repeat (List.length specifiedDates) "0"




split : Int -> List a -> (List a, List a)
split i xs =
    (List.take i xs, List.drop i xs) 







