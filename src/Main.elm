module Main exposing (..)

{-| This is the entrypoint for the Tagist program
@docs ComputerLanguage, FileContents, FileCoordinates, FileData, FileType
@docs Filter, FilterTree, GistSummary, Model, Msg, categorizeFilters, extractUsers, fileDecoder, fileInfoDecoder, fileLink, fileTypeDecoder, filterGistByTags, filterGistByTree, getGists, infoDecoder, init, languageDecoder, main, modelFromLocation, parseAndValues, parseOrValues, removeEmptyLinks, requestGists, requestGistsForAll, requestGistsForUser, tagFilters, treeMaker, update, updateFileWithData, updateGistWithFileData, view, viewFile, viewGist, viewGists
-}

import Html exposing (..)
import Html.Attributes exposing (href, class, title)
import Html.Events exposing (onClick)
import Navigation
import Set exposing (Set)
import Http
import Json.Decode.Pipeline exposing (decode, required, optional, optionalAt, hardcoded)
import Json.Decode exposing (Decoder, list, string, field, dict, nullable, maybe, decodeString)
import Dict exposing (Dict)
import Markdown


{-| The entry point for tagist
    elm-reactor Tagist.elm
-}
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


{-| blerg
-}
type Filter
    = User String
    | Tag String


{-| blerg
-}
type FileType
    = PlainText
    | UnknownType


{-| blerg
-}
type FilterTree
    = FilterTree
        { orFilters : List Filter
        , andFilters : FilterTree
        }
    | EmptyFilterTree



--these are the data structures for the gists themselves


{-| blerg
-}
type ComputerLanguage
    = Markdown
    | Text
    | UnknownLanguage


{-| blerg
-}
type FileContents
    = Unloaded
    | Loading
    | Loaded String
    | Error String


{-| blerg
-}
type alias FileData =
    { name : String
    , rawUrl : String
    , fileType : FileType
    , language : ComputerLanguage
    , contents : FileContents
    }


{-| blerg
-}
type alias GistSummary =
    { id : String
    , owner : Maybe String
    , description : Maybe String
    , files : List FileData
    }


{-| blerg
-}
type alias Model =
    { error : Maybe String
    , gistResponses :
        List (Http.Response String)
        --the responses are more or less just for debugging, they shouldn't be needed really
    , gistInfo : List GistSummary
    , filters : FilterTree
    }


{-| blerg
-}
fileTypeDecoder : String -> Decoder FileType
fileTypeDecoder fileType =
    case fileType of
        "text/plain" ->
            decode PlainText

        _ ->
            decode UnknownType


{-| blerg
-}
languageDecoder : String -> Decoder ComputerLanguage
languageDecoder language =
    case language of
        "Markdown" ->
            decode Markdown

        "Text" ->
            decode Text

        _ ->
            decode UnknownLanguage


{-| blerg
-}
fileInfoDecoder : Decoder FileData
fileInfoDecoder =
    decode FileData
        |> required "filename" string
        |> required "raw_url" string
        |> required "type" (string |> Json.Decode.andThen fileTypeDecoder)
        |> optional "language" (string |> Json.Decode.andThen languageDecoder) UnknownLanguage
        |> hardcoded Unloaded


{-| blerg
-}
fileDecoder : Decoder (List FileData)
fileDecoder =
    (dict <| fileInfoDecoder) |> Json.Decode.andThen (\dict -> decode (Dict.values dict))


{-| blerg
-}
infoDecoder : Decoder GistSummary
infoDecoder =
    decode GistSummary
        |> required "id" string
        |> optionalAt [ "owner", "login" ] (maybe string) Nothing
        |> required "description" (nullable string)
        |> required "files" fileDecoder


{-| blerg
-}
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


{-| blerg
-}
parseOrValues : String -> List Filter
parseOrValues filter =
    List.filterMap categorizeFilters (String.split "," filter)


{-| blerg
-}
treeMaker : String -> FilterTree -> FilterTree
treeMaker filter tree =
    FilterTree { orFilters = (parseOrValues filter), andFilters = tree }


{-| blerg
-}
parseAndValues : String -> FilterTree
parseAndValues filter =
    List.foldr treeMaker EmptyFilterTree (String.split "/" filter)


{-| blerg
-}
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


{-| blerg
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


{-| blerg
-}
tagFilters : List Filter -> List String
tagFilters =
    List.foldl
        (\f a ->
            case f of
                User s ->
                    a

                Tag s ->
                    s :: a
        )
        []


{-| blerg
-}
filterGistByTags : List Filter -> Maybe GistSummary -> Maybe GistSummary
filterGistByTags orFilters gist =
    case ( tagFilters orFilters, gist ) of
        ( [], _ ) ->
            --empty filters match all the things
            gist

        ( filters, Nothing ) ->
            Nothing

        ( filters, Just g ) ->
            case
                List.foldl
                    (\f a ->
                        a
                            || (case g.description of
                                    Nothing ->
                                        False

                                    Just d ->
                                        String.contains (String.toLower f) (String.toLower d)
                               )
                    )
                    False
                    filters
            of
                True ->
                    gist

                False ->
                    Nothing


{-| blerg
-}
filterGistByTree : FilterTree -> GistSummary -> Maybe GistSummary
filterGistByTree tree gist =
    case tree of
        EmptyFilterTree ->
            Just gist

        FilterTree tree ->
            filterGistByTags tree.orFilters (filterGistByTree tree.andFilters gist)


{-| blerg
-}
modelFromLocation : Navigation.Location -> Model
modelFromLocation location =
    location.hash
        |> parseAndValues
        |> removeEmptyLinks
        |> Model Nothing [] []


{-| blerg
-}
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


{-| blerg
-}
type alias FileCoordinates =
    { gistId : String
    , fileName : String
    }


{-| blerg
-}
type Msg
    = UrlChange Navigation.Location
    | AppendGists (Result Http.Error (Http.Response String))
    | RequestFileContents FileCoordinates String
    | ReceiveFileContents FileCoordinates (Result Http.Error String)
    | RemoveFileContents FileCoordinates


{-| blerg
-}
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


{-| blerg
-}
requestGistsForUser : String -> Cmd Msg
requestGistsForUser user =
    Http.send AppendGists (getGists ("https://api.github.com/users/" ++ user ++ "/gists"))


{-| blerg
-}
requestGistsForAll : Cmd Msg
requestGistsForAll =
    Http.send AppendGists (getGists "https://api.github.com/gists")


{-| blerg
-}
requestGists : Model -> Cmd Msg
requestGists model =
    case (extractUsers model.filters) of
        [] ->
            requestGistsForAll

        users ->
            users
                |> List.map requestGistsForUser
                |> Cmd.batch


{-| blerg
-}
updateFileWithData : FileCoordinates -> FileContents -> FileData -> FileData
updateFileWithData fileCoords data file =
    if fileCoords.fileName == file.name then
        { file | contents = data }
    else
        file


{-| blerg
-}
updateGistWithFileData : FileCoordinates -> FileContents -> GistSummary -> GistSummary
updateGistWithFileData fileCoords data gist =
    if fileCoords.gistId == gist.id then
        { gist | files = List.map (updateFileWithData fileCoords data) gist.files }
    else
        gist


{-| blerg
-}
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
                    model.gistInfo ++ (List.filterMap (filterGistByTree model.filters) <| Result.withDefault [] (decodeString (list infoDecoder) gist.body))
                , gistResponses = gist :: model.gistResponses
            }
                ! []

        ReceiveFileContents fileCoords (Err error) ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords (Error (toString error))) model.gistInfo } ! []

        ReceiveFileContents fileCoords (Ok data) ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords (Loaded data)) model.gistInfo } ! []

        RequestFileContents fileCoords url ->
            ( { model | gistInfo = List.map (updateGistWithFileData fileCoords Loading) model.gistInfo }, Http.send (ReceiveFileContents fileCoords) (Http.getString url) )

        RemoveFileContents fileCoords ->
            { model | gistInfo = List.map (updateGistWithFileData fileCoords Unloaded) model.gistInfo } ! []



-- VIEW


{-| blerg
-}
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


{-| blerg
-}
fileLink : String -> FileData -> Html msg
fileLink t f =
    a [ href f.rawUrl, title f.name ] [ text t ]


{-| blerg
-}
viewFile : String -> FileData -> Html Msg
viewFile gistId file =
    let
        expand action bullet =
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

        rawLink =
            fileLink "raw"
    in
        div [] <|
            case ( file.contents, file.language ) of
                ( Unloaded, UnknownLanguage ) ->
                    [ span [] [ text file.name ]
                    , rawLink file
                    ]

                ( Unloaded, _ ) ->
                    [ expand getContents <| "[ + " ++ file.name ++ " ]"
                    , rawLink file
                    ]

                ( Loading, _ ) ->
                    [ expand getContents <| "[ + " ++ file.name ++ " Loading ]"
                    , rawLink file
                    ]

                ( Loaded data, Markdown ) ->
                    [ expand removeContents <| "[ − " ++ file.name ++ " ]"
                    , rawLink file
                    , div [] [ Markdown.toHtml [] data ]
                    ]

                ( Loaded data, Text ) ->
                    [ expand removeContents <| "[ − " ++ file.name ++ " ]"
                    , rawLink file
                    , div [] [ pre [] [ code [] [ text data ] ] ]
                    ]

                ( Loaded data, _ ) ->
                    [ expand removeContents <| "[ − " ++ file.name ++ " ]"
                    , rawLink file
                    , div [] [ text data ]
                    ]

                ( Error message, _ ) ->
                    [ expand getContents <| "[ ↻ " ++ file.name ++ " ]"
                    , rawLink file
                    , div [] [ text message ]
                    ]


{-| blerg
-}
viewGist : GistSummary -> Html Msg
viewGist gist =
    div [ class "comic--border" ]
        [ h2 [] [ text <| Maybe.withDefault "Untitled" gist.description ]
        , h3 [] [ text <| "—" ++ Maybe.withDefault "Anonymous" gist.owner ]
        , div [] <| List.map (viewFile gist.id) gist.files
        ]


{-| blerg
-}
viewGists : List GistSummary -> Html Msg
viewGists gists =
    div [] <| List.map viewGist gists
