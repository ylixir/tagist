module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Navigation
import Set exposing (Set)
import Http
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt, hardcoded)
import Json.Decode exposing (Decoder, list, string, field, dict, nullable, maybe, decodeString)
import Dict exposing (Dict)
import Markdown


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL
--these are the models for the url/filters


type Filter
    = User String
    | Tag String


type FileType
    = PlainText
    | UnknownType


type FilterTree
    = FilterTree
        { orFilters : List Filter
        , andFilters : FilterTree
        }
    | EmptyFilterTree



--these are the data structures for the gists themselves


type ComputerLanguage
    = Markdown
    | Text
    | UnknownLanguage


type FileContents
    = Unloaded
    | Loading
    | Loaded String
    | Error String


type alias FileData =
    { name : String
    , rawUrl : String
    , fileType : FileType
    , language : ComputerLanguage
    , contents : FileContents
    }


type alias GistSummary =
    { id : String
    , owner : Maybe String
    , description : Maybe String
    , files : List FileData
    }


type alias Model =
    { error : Maybe String
    , gistResponses :
        List (Http.Response String)
        --the responses are more or less just for debugging, they shouldn't be needed really
    , gistInfo : List GistSummary
    , filters : FilterTree
    }


fileTypeDecoder : String -> Decoder FileType
fileTypeDecoder fileType =
    case fileType of
        "text/plain" ->
            decode PlainText

        _ ->
            decode UnknownType


languageDecoder : String -> Decoder ComputerLanguage
languageDecoder language =
    case language of
        "Markdown" ->
            decode Markdown

        "Text" ->
            decode Text

        _ ->
            decode UnknownLanguage


fileInfoDecoder : Decoder FileData
fileInfoDecoder =
    decode FileData
        |> required "filename" string
        |> required "raw_url" string
        |> required "type" (string |> Json.Decode.andThen fileTypeDecoder)
        |> optional "language" (string |> Json.Decode.andThen languageDecoder) UnknownLanguage
        |> hardcoded Unloaded


fileDecoder : Decoder (List FileData)
fileDecoder =
    (dict <| fileInfoDecoder) |> Json.Decode.andThen (\dict -> decode (Dict.values dict))


infoDecoder : Decoder GistSummary
infoDecoder =
    decode GistSummary
        |> required "id" string
        |> optionalAt [ "owner", "login" ] (maybe string) Nothing
        |> required "description" (nullable string)
        |> required "files" fileDecoder


categorizeFilters : String -> Maybe Filter
categorizeFilters filter =
    case String.uncons filter of
        Just ( '#', "" ) ->
            Nothing

        Just ( '#', tail ) ->
            Just <| User tail

        Just ( head, tail ) ->
            Just <| Tag filter

        Nothing ->
            Nothing


parseOrValues : String -> List Filter
parseOrValues filter =
    List.filterMap categorizeFilters (String.split "," filter)


treeMaker : String -> FilterTree -> FilterTree
treeMaker filter tree =
    FilterTree { orFilters = (parseOrValues filter), andFilters = tree }


parseAndValues : String -> FilterTree
parseAndValues filter =
    List.foldr treeMaker EmptyFilterTree (String.split "/" filter)


extractUsers : FilterTree -> List String
extractUsers filterTree =
    case filterTree of
        EmptyFilterTree ->
            []

        FilterTree tree ->
            Set.toList <|
                (List.foldl
                    (\filter users ->
                        case filter of
                            User user ->
                                Set.insert user users

                            Tag tag ->
                                users
                    )
                    (Set.fromList (extractUsers tree.andFilters))
                    tree.orFilters
                )



{-
   Including empty "or" conditions has the effect of matching nothing all of the time.
   This is almost certainly unwanted, and if a user doesn't want to specify a user it is
   hard to avoid. We will just helpfully filter out this condition.
-}


removeEmptyLinks : FilterTree -> FilterTree
removeEmptyLinks filterTree =
    case filterTree of
        FilterTree tree ->
            let
                cleanAndFilters : FilterTree
                cleanAndFilters =
                    removeEmptyLinks tree.andFilters
            in
                case tree.orFilters of
                    [] ->
                        cleanAndFilters

                    _ ->
                        FilterTree { tree | andFilters = cleanAndFilters }

        EmptyFilterTree ->
            EmptyFilterTree


modelFromLocation : Navigation.Location -> Model
modelFromLocation location =
    location.hash
        |> parseAndValues
        |> removeEmptyLinks
        |> Model Nothing [] []


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        newModel =
            modelFromLocation location
    in
        ( newModel
        , requestGists newModel
        )



-- UPDATE


type alias FileCoordinates =
    { gistId : String
    , fileName : String
    }


type Msg
    = UrlChange Navigation.Location
    | AppendGists (Result Http.Error (Http.Response String))
    | RequestFileContents FileCoordinates String
    | ReceiveFileContents FileCoordinates (Result Http.Error String)
    | RemoveFileContents FileCoordinates


getGists : String -> Http.Request (Http.Response String)
getGists url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.github.v3+json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\response -> Ok response)
        , timeout = Nothing
        , withCredentials = False
        }


requestGistsForUser : String -> Cmd Msg
requestGistsForUser user =
    Http.send AppendGists (getGists ("https://api.github.com/users/" ++ user ++ "/gists"))


requestGistsForAll : Cmd Msg
requestGistsForAll =
    Http.send AppendGists (getGists "https://api.github.com/gists")


requestGists : Model -> Cmd Msg
requestGists model =
    let
        users =
            extractUsers model.filters
    in
        case users of
            [] ->
                requestGistsForAll

            _ ->
                users
                    |> List.map requestGistsForUser
                    |> Cmd.batch


updateFileWithData : FileCoordinates -> FileContents -> FileData -> FileData
updateFileWithData fileCoords data file =
    if fileCoords.fileName == file.name then
        { file | contents = data }
    else
        file


updateGistWithFileData : FileCoordinates -> FileContents -> GistSummary -> GistSummary
updateGistWithFileData fileCoords data gist =
    if fileCoords.gistId == gist.id then
        { gist | files = List.map (updateFileWithData fileCoords data) gist.files }
    else
        gist


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newModel =
                    modelFromLocation location
            in
                ( newModel
                , requestGists newModel
                )

        AppendGists (Err error) ->
            { model | error = Just (toString error) } ! []

        AppendGists (Ok gist) ->
            { model
                | gistInfo =
                    model.gistInfo ++ Result.withDefault [] (decodeString (list infoDecoder) gist.body)
                , gistResponses = gist :: model.gistResponses
            }
                ! []

        ReceiveFileContents fileCoords (Err error) ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords (Error (toString error))) model.gistInfo } ! []

        ReceiveFileContents fileCoords (Ok data) ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords (Loaded data)) model.gistInfo } ! []

        RequestFileContents fileCoords url ->
            ( model, Http.send (ReceiveFileContents fileCoords) (Http.getString url) )

        RemoveFileContents fileCoords ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords Unloaded) model.gistInfo } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        (case model.error of
            Just error ->
                [ h1 [] [ text "Errors" ]
                , div [] [ text error ]
                ]

            Nothing ->
                []
        )
            ++ [ h1 [] [ text "Gists" ]
               , viewGists model.gistInfo
               ]


fileLink : FileData -> Html msg
fileLink f =
    a [ href f.rawUrl ] [ text f.name ]


viewFile : String -> FileData -> Html Msg
viewFile gistId file =
    let
        knownLanguage action bullet =
            case file.language of
                UnknownLanguage ->
                    span [] []

                _ ->
                    span [ action ] [ text bullet ]

        getContents =
            file.rawUrl
                |> RequestFileContents (FileCoordinates gistId file.name)
                |> onClick

        removeContents =
            FileCoordinates gistId file.name
                |> RemoveFileContents
                |> onClick
    in
        div [] <|
            case ( file.contents, file.language ) of
                ( Unloaded, UnknownLanguage ) ->
                    [ span [] []
                    , fileLink file
                    ]

                ( Unloaded, _ ) ->
                    [ knownLanguage getContents "+"
                    , fileLink file
                    ]

                ( Loading, _ ) ->
                    [ knownLanguage getContents "−"
                    , fileLink file
                    , div [] [ text "Loading..." ]
                    ]

                ( Loaded data, Markdown ) ->
                    [ knownLanguage removeContents "−"
                    , fileLink file
                    , div [] [ Markdown.toHtml [] data ]
                    ]

                ( Loaded data, Text ) ->
                    [ knownLanguage removeContents "−"
                    , fileLink file
                    , div [] [ pre [] [ code [] [ text data ] ] ]
                    ]

                ( Loaded data, _ ) ->
                    [ knownLanguage removeContents "−"
                    , fileLink file
                    , div [] [ text data ]
                    ]

                ( Error message, _ ) ->
                    [ knownLanguage getContents "↻"
                    , fileLink file
                    , div [] [ text message ]
                    ]


viewGist : GistSummary -> Html Msg
viewGist gist =
    div []
        [ h2 [] [ text <| Maybe.withDefault "Untitled" gist.description ]
        , h3 [] [ text <| Maybe.withDefault "Anonymous" gist.owner ]
        , div [] <| List.map (viewFile gist.id) gist.files
        ]


viewGists : List GistSummary -> Html Msg
viewGists gists =
    div [] <| List.map viewGist gists
