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
    }


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "modelInitialValue" "", Cmd.none )


type Msg
    = Msg1
    | Msg2
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Test

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

        Test ->
            ( {model | roomID =Maybe.withDefault "Error" (List.head (List.reverse (String.split "=" (Url.toString model.url))))}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Room"
    , body =
        [ div [] [ button [onClick Test] [ text "Create New Room" ] ]
         ,div [] [ text (model.roomID) ]
        ]
    }


