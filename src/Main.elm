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


{-| Determine whether the grid cell at position i j
is alive or dead at the the next step. Here
isAlive is assumed to be the current state of the cell i j.
-}
step : Grid -> Int -> Int -> Bool -> Bool
step grid i j isAlive =
    let
        neighborCount =
            Matrix.Extra.neighbours i j grid
                |> List.filter identity
                |> List.length
    in
        if (not isAlive) && neighborCount == neighborCountForBirth then
            True
        else if isAlive && (neighborCount == 2 || neighborCount == 3) then
            True
        else
            False



{- Helpers for generating a ner random live cell -}


randomPair : Random.Generator ( Int, Int )
randomPair =
    Random.pair (Random.int 0 gridSize) (Random.int 0 gridSize)


createLiveCells : Grid -> ( Int, Int ) -> Int -> Int -> Bool -> Bool
createLiveCells grid newCell x y isAlive =
    let
        ( xx, yy ) =
            newCell
    in
        if abs x == xx && abs y == yy then
            True
        else
            isAlive


{-| Return the negation of the model.generatateNewCells
boolean at the start of each cycle. A cycle has lenfth
cycleLength.
-}
generateNewCells : Model -> Bool
generateNewCells model =
    if model.step % cycleLength == 0 then
        not model.generateNewCells
    else
        model.generateNewCells



{- Components of the update function -}


{-| Use the argument `grid` to set model.grid.
-}
setGrid : Model -> Grid -> ( Model, Cmd Msg )
setGrid model grid =
    ( { model | grid = grid }, Cmd.none )


{-| Make a new live cell at position (i, j)
-}
makeNewLiveCell : Model -> ( Int, Int ) -> ( Model, Cmd Msg )
makeNewLiveCell model ( i, j ) =
    let
        genCells =
            generateNewCells model

        newGrid =
            if genCells then
                Matrix.indexedMap (createLiveCells model.grid ( i, j )) model.grid
            else
                model.grid
    in
        ( { model | grid = newGrid, generateNewCells = genCells }, Cmd.none )


{-| Take one step forward: compute a new grid from the old one using
the rules in the partially applied function `step model.grid` using

indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
-- Apply a function of every element in the matrix. Notice that

step : Grid -> Int -> Int -> Bool -> Bool

so that

step model.grid : Int -> Int -> Bool -> Bool

is of the right type for `Matrix.indexedMap`

Also, issue a command to generate a new random pair (i,j) for
consumption by the NewCell message handler the update function.

-}
takeStep : Model -> ( Model, Cmd Msg )
takeStep model =
    let
        newGrid =
            Matrix.indexedMap (step model.grid) model.grid
    in
        ( { model | grid = newGrid, step = model.step + 1 }, Random.generate NewCell randomPair )


{-| UPDATE FUNCTION:

1.  On initialization, set the grid to a matrix of random values (True, False)
2.  When NewCell (i, j) message is received, create a live cell at at location (i, j)
3.  When a Step message is received, compute a new grid from the old one and issue
    a command to generate a tuple of two random numbers and issue a NewCell randomPair message.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGrid grid ->
            setGrid model grid

        NewCell ( i, j ) ->
            makeNewLiveCell model ( i, j )

        Step ->
            takeStep model



{- SUBSCRIPTIONS -}


{-| Send the step message at each animation frame req4ust of
the javascript rendering engine.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs (always Step)



{- VIEW FUMCTION -}


{-| Display the cell grid with the data legend below it
-}
view : Model -> Html Msg
view model =
    div []
        [ table [ style [ ( "border-collapse", "collapse" ) ] ]
            [ tbody [] (List.map row (matrixToList model.grid))
            ]
        , span [ style [ ( "font-family", "Courier" ) ] ] [ text (legend model) ]
        ]



{- VIEW FUMCTION: CELL GRID -}


{-| If the matrix has N rows, construct the list [0, 1, ..., N]
using List.range 0 (Matrix.height matrix). Feed this list to
a function

flip Matrix.getRow matrix

that gets the kth row of the matrix. Here we use

getRow : Int -> Matrix a -> Maybe (Array a)

Thus a Maybe Array is returned. Feed this result to the
composition

Maybe.withDefault Array.empty >> Array.toList

which turns it into a list.

-}
matrixToList : Matrix a -> List (List a)
matrixToList matrix =
    List.range 0 (Matrix.height matrix)
        |> List.map (flip Matrix.getRow matrix >> Maybe.withDefault Array.empty >> Array.toList)


{-| row takes an array of booleans and
returns a table row cells corresponding
to the array. A cell for False is displayed
as a dark green square, and a cell for True
is displayed as a yellow square.
-}
row : List Bool -> Html Msg
row =
    List.map cell >> (tr [])


{-| Display a cell, with the "look"
depending on wther it is "alive" or not.
-}
cell : Bool -> Html Msg
cell isAlive =
    td [ cellStyle isAlive ] []


{-| Compute style: a yellow square if the cell is alive,
dark green if not.
-}
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



{- VIEW FUNCTION: LABELS
   These compute a string as a function of the model and
   are used in constructing the legend belo the grid --
   information about population, density, etc.
-}


countLiveCells : Grid -> Int
countLiveCells grid =
    grid |> Matrix.filter identity |> Array.length


stepLabel : Model -> String
stepLabel model =
    "Step: " ++ (toString model.step)


populationLabel : Model -> String
populationLabel model =
    "Population: " ++ (toString (countLiveCells model.grid))


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
        "Density: " ++ (String.padRight 7 '0' (toString density))


randomBirthLabel : Model -> String
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
