module Main exposing (..)

import Html exposing (..)
import Navigation
import Set exposing (Set)
import Http


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL


type Filter
    = User String
    | Tag String


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


type FilterTree
    = FilterTree
        { orFilters : List Filter
        , andFilters : FilterTree
        }
    | EmptyFilterTree


parseOrValues : String -> List Filter
parseOrValues filter =
    List.filterMap categorizeFilters (String.split "," filter)


treeMaker : String -> FilterTree -> FilterTree
treeMaker filter tree =
    FilterTree { orFilters = (parseOrValues filter), andFilters = tree }


parseAndValues : String -> FilterTree
parseAndValues filter =
    List.foldr treeMaker EmptyFilterTree (String.split "/" filter)


extractUsers : FilterTree -> Set String
extractUsers filterTree =
    case filterTree of
        EmptyFilterTree ->
            Set.empty

        FilterTree tree ->
            List.foldl
                (\filter users ->
                    case filter of
                        User user ->
                            Set.insert user users

                        Tag tag ->
                            users
                )
                (extractUsers tree.andFilters)
                tree.orFilters



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
        |> Model "" []


type alias Model =
    { error : String
    , gists : List String
    , filters : FilterTree
    }


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


type Msg
    = UrlChange Navigation.Location
    | AppendGists (Result Http.Error String)


requestGistsForUser : String -> Cmd Msg
requestGistsForUser user =
    Http.send AppendGists (Http.getString ("https://api.github.com/users/" ++ user ++ "/gists"))


requestGists : Model -> Cmd Msg
requestGists model =
    Cmd.batch
        (model.filters
            |> extractUsers
            |> Set.toList
            |> List.map requestGistsForUser
        )


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

        AppendGists result ->
            case result of
                Err error ->
                    { model | error = toString error } ! []

                Ok gist ->
                    { model | gists = gist :: model.gists } ! []



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ h1 [] [ text "Filter Tree" ]
        , viewTree model.filters
        , viewUsers <| extractUsers model.filters
        , h1 [] [ text "Errors" ]
        , div [] [ text model.error ]
        , h1 [] [ text "Gists" ]
        , viewGists model.gists
        ]


viewGists : List String -> Html msg
viewGists gists =
    div [] <| List.map (\gist -> div [] [ text gist ]) gists


viewUsers : Set String -> Html msg
viewUsers users =
    div []
        [ h1 [] [ text "Users" ]
        , ul [] <|
            (users
                |> Set.toList
                |> List.map (\user -> li [] [ text user ])
            )
        ]


viewFilterItem : Filter -> Html msg
viewFilterItem item =
    case item of
        User string ->
            li [] [ text <| "User: " ++ string ]

        Tag string ->
            li [] [ text <| "Tag: " ++ string ]


viewTree : FilterTree -> Html msg
viewTree filterTree =
    case filterTree of
        FilterTree tree ->
            div []
                [ h2 [] [ text "Or" ]
                , ul []
                    ((li [] [ h2 [] [ text "And" ], ul [] [ viewTree tree.andFilters ] ])
                        :: (List.map viewFilterItem tree.orFilters)
                    )
                ]

        EmptyFilterTree ->
            div [] []
