module Main exposing (main)

import Html exposing (Html, Attribute, table, tbody, tr, td, text, div, span)
import Html.Attributes exposing (style)
import Matrix exposing (Matrix)
import Matrix.Extra
import Array
import Random
import AnimationFrame


gridSize : Int
gridSize =
    100


neighborCountForBirth : Int
neighborCountForBirth =
    3


cycleLength : Int
cycleLength =
    2000


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { grid : Grid
    , step : Int
    , generateNewCells : Bool
    }


type alias Grid =
    Matrix Bool


type Msg
    = SetGrid Grid
    | Step
    | NewCell ( Int, Int )


gridGenerator : Random.Generator Grid
gridGenerator =
    Random.list gridSize (Random.list gridSize Random.bool)
        |> Random.map (Matrix.fromList >> Maybe.withDefault (Matrix.repeat gridSize gridSize False))


init : ( Model, Cmd Msg )
init =
    let
        model =
            { grid = Matrix.repeat gridSize gridSize False
            , step = 1
            , generateNewCells = True
            }
    in
        ( model, Random.generate SetGrid gridGenerator )



{- UPDATE -}


step grid x y isAlive =
    let
        neighborCount =
            Matrix.Extra.neighbours x y grid
                |> List.filter identity
                |> List.length
    in
        if (not isAlive) && neighborCount == neighborCountForBirth then
            True
        else if isAlive && (neighborCount == 2 || neighborCount == 3) then
            True
        else
            False


randomPair : Random.Generator ( Int, Int )
randomPair =
    Random.pair (Random.int 0 gridSize) (Random.int 0 gridSize)


createLiveCells grid newCell x y isAlive =
    let
        ( xx, yy ) =
            newCell
    in
        if abs x == xx && abs y == yy then
            True
        else
            isAlive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGrid grid_ ->
            ( { model | grid = grid_ }, Cmd.none )

        NewCell ( a, b ) ->
            let
                grid1 =
                    model.grid

                generateNewCells =
                    if model.step % cycleLength == 0 then
                        not model.generateNewCells
                    else
                        model.generateNewCells

                grid2 =
                    if generateNewCells then
                        Matrix.indexedMap (createLiveCells grid1 ( a, b )) grid1
                    else
                        grid1
            in
                ( { model | grid = grid2, generateNewCells = generateNewCells }, Cmd.none )

        Step ->
            let
                grid1 =
                    model.grid

                grid2 =
                    Matrix.indexedMap (step grid1) grid1
            in
                ( { model | grid = grid2, step = model.step + 1 }, Random.generate NewCell randomPair )



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs (always Step)



{- VIEW FUNCTION -}


countLiveCells : Grid -> Int
countLiveCells grid =
    grid |> Matrix.filter identity |> Array.length


stepLabel : Model -> String
stepLabel model =
    "Step = " ++ (toString model.step)


populationLabel : Model -> String
populationLabel model =
    "Population = " ++ (toString (countLiveCells model.grid))


densityLabel : Model -> String
densityLabel model =
    let
        population =
            (toFloat (countLiveCells model.grid))

        nCells =
            (toFloat (gridSize * gridSize))

        density =
            population / nCells
    in
        "Density = " ++ (String.padRight 7 '0' (toString density))


randomBirthLabel model =
    let
        cycleStage =
            model.step % cycleLength

        cycleStageString =
            (toString cycleStage) ++ "/" ++ (toString cycleLength)
    in
        if model.generateNewCells then
            "Birth: ON (" ++ cycleStageString ++ ")"
        else
            "Birth: OFF (" ++ cycleStageString ++ ")"


legend : Model -> String
legend model =
    [ stepLabel model
    , populationLabel model
    , densityLabel model
    , randomBirthLabel model
    ]
        |> String.join (", ")


view : Model -> Html Msg
view model =
    div []
        [ table [ style [ ( "border-collapse", "collapse" ) ] ]
            [ tbody [] (List.map row (matrixToList model.grid))
            ]
        , span [ style [ ( "font-family", "Courier" ) ] ] [ text (legend model) ]
        ]


matrixToList : Matrix a -> List (List a)
matrixToList matrix =
    List.range 0 (Matrix.height matrix)
        |> List.map (flip Matrix.getRow matrix >> Maybe.withDefault Array.empty >> Array.toList)


row : List Bool -> Html Msg
row =
    List.map cell >> (tr [])


cell : Bool -> Html Msg
cell isAlive =
    td [ cellStyle isAlive ] []


cellStyle : Bool -> Attribute Msg
cellStyle isAlive =
    let
        backgroundColor =
            if isAlive then
                "#ee0"
            else
                "#050"
    in
        style
            [ ( "background-color", backgroundColor )
            , ( "width", "4px" )
            , ( "height", "4px" )
            , ( "border", "0.5px solid black" )
            ]
