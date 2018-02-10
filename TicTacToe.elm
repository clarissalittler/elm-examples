import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Array exposing (..)

main = Html.beginnerProgram { model = model, view = view, update = update}

type Piece = X | O

showPiece : Maybe Piece -> String
showPiece p = case p of
                  (Just X) -> "X"
                  (Just O) -> "O"
                  Nothing  -> "_"

-- The board is in row, column form
              
type alias Model = {xWins : Int,
                    oWins : Int,
                    board : Array (Array (Maybe Piece)),
                    currentPlayer : Piece}

model = {xWins=0,
         oWins=0,
         board=blankBoard,
         currentPlayer = X}
    
type Msg = Place Int Int
         | Reset

getPiece : Int -> Int -> Array (Array (Maybe Piece)) -> Maybe Piece
getPiece i j a = case get i a of
                     Nothing -> Nothing
                     Just r -> case get j r of
                                   Nothing -> Nothing
                                   Just p -> p
--I don't know why I'm getting myself turned around on this one!
--we need to replace the /column/ inside the /row/
setPiece i j a p = case get i a of
                       Nothing -> a
                       Just row -> set i (set j (Just p) row) a

blankBoard = initialize 3 (\x -> initialize 3 (\y -> Nothing))
                                   
nextPlayer p = case p of
                   X -> O
                   O -> X
-- this function is the most annoying part!
-- we need a total of 8 checks to see if someone won!

testMaybe : Maybe a -> Maybe a -> Bool
testMaybe m1 m2 = case m1 of
                      Nothing -> False
                      Just x -> case m2 of
                                    Nothing -> False
                                    Just y -> x == y

checkRow m n = (testMaybe (getPiece n 0 m) (getPiece n 1 m)) && (testMaybe (getPiece n 1 m) (getPiece n 2 m))

checkCol m n = let zeroth = getPiece 0 n m
                   first = getPiece 1 n m
                   second = getPiece 2 n m
               in (testMaybe zeroth first) && (testMaybe first second)
                               
findWinner : Array (Array (Maybe Piece)) -> Bool
findWinner a = let rowWins = checkRow a 0 || checkRow a 1 || checkRow a 2
                   colWins = checkCol a 0 || checkCol a 1 || checkCol a 2
                   diagOne = (testMaybe (getPiece 0 0 a) (getPiece 1 1 a)) && (testMaybe (getPiece 1 1 a) (getPiece 2 2 a))
                   diagTwo = (testMaybe (getPiece 2 0 a) (getPiece 1 1 a)) && (testMaybe (getPiece 1 1 a) (getPiece 0 2 a))
               in rowWins || colWins || diagOne || diagTwo
                        
placePiece i j m = let newBoard = {m | board = (setPiece i j (m.board) m.currentPlayer)}
                       winner = findWinner newBoard.board
                   in if winner
                        then case newBoard.currentPlayer of
                                 X -> {newBoard | currentPlayer = X, board = blankBoard, xWins=newBoard.xWins+1}
                                 O -> {newBoard | currentPlayer = X, board = blankBoard, oWins=newBoard.oWins+1}
                        else {newBoard | currentPlayer = nextPlayer m.currentPlayer}
                                   
update msg model = case msg of
                       Place i j -> placePiece i j model
                       Reset -> {model | board = blankBoard, xWins=0, oWins=0}

collapseMaybe : Maybe (Maybe a) -> Maybe a
collapseMaybe j = case j of
                      Just (Just x) -> Just x
                      Just Nothing -> Nothing
                      Nothing -> Nothing
                                 
boardSquare model i j = div
                          [cellStyle,onClick (Place i j)]
                          [text (showPiece (getPiece i j model.board))]

cellStyle = style [("width","20px"),("height","20px"),("margin","10px")]
                              
boardRow model i = div
                   [style [("display","flex"),("flex-direction","row")]]
                   (toList (indexedMap (\j x -> boardSquare model i j) model.board))

boardHtml m = (toList (indexedMap (\i x -> boardRow m i) m.board))
resetButton = button [onClick Reset] [text "Reset the game"]
winnersTotal m = div [] [text ("X Wins "++(toString m.xWins)),
                         text (" O Wins "++(toString m.oWins))]
                   
view : Model -> Html Msg
view m = div [] (boardHtml m ++ [resetButton, winnersTotal m] )
