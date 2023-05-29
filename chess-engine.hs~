import Data.Char (ord, chr)
import Data.List (find)
import Data.Maybe (fromJust, isJust)

-- Initial Chess Board
--     a    b    c    d    e    f    g    h
-- 8 | RB | NB | BB | QB | KB | BB | NB | RB |
-- 7 | PB | PB | PB | PB | PB | PB | PB | PB |
-- 6 |    |    |    |    |    |    |    |    |
-- 5 |    |    |    |    |    |    |    |    |
-- 4 |    |    |    |    |    |    |    |    |
-- 3 |    |    |    |    |    |    |    |    |
-- 2 | PW | PW | PW | PW | PW | PW | PW | PW |
-- 1 | RW | NW | BW | QW | KW | BW | NW | RW |





-- Data Types

-- location of pieces 
-- e.g: ('a', 7) is a black Pawn 
type Location = (Char, Int)

-- type of player, either white or black
data Player = White 
            | Black 
            deriving (Show, Eq)

-- P: Pawn
-- N: Knight
-- K: King
-- Q: Queen
-- R: Rook
-- B: Bishop
data Piece = P Location 
           | N Location 
           | K Location 
           | Q Location 
           | R Location
           | B Location 
           deriving (Show, Eq)
           
-- current player, first list is white, second is black   
type Board = (Player, [Piece], [Piece])










-- Engine Functions

-- sets the initial board configuration
setBoard :: Board
setBoard = (White, whitePieces, blackPieces)
  where
    whitePieces = [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1),
                   Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1),
                   P ('b', 2), P ('c', 2), P ('d', 2), P ('e', 2), 
                   P ('f', 2), P ('a', 2), P ('g', 2), P ('h', 2)]
                   
    blackPieces = [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), 
                   Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),
                   P ('b', 7), P ('c', 7), P ('d', 7), P ('e', 7),
                   P ('f', 7), P ('a', 7), P ('g', 7), P ('h', 7)]


visualizeBoard :: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = 
    let
        -- Define the rows and columns of the chess board
        rows = [8,7..1]
        columns = ['a'..'h']
        
        -- Combine the white and black pieces into a single list
        pieces = whitePieces ++ blackPieces
        
        -- Define a function to find the piece at a given location on the board
        pieceAt loc = find (\p -> location p == loc) pieces
        
        -- Define a function to convert a piece to its string representation
        showPiece (Just (P _)) = "P"
        showPiece (Just (N _)) = "N"
        showPiece (Just (K _)) = "K"
        showPiece (Just (Q _)) = "Q"
        showPiece (Just (R _)) = "R"
        showPiece (Just (B _)) = "B"
        showPiece Nothing = " "
        
        -- Define a function to convert a player to its string representation
        showPlayer p | p `elem` whitePieces = 'W'
                     | otherwise = 'B'
        
        -- Define a function to convert a cell on the board to its string representation
        showCell loc | isJust piece = showPiece piece ++ [showPlayer $ fromJust piece]
                     | otherwise = "  "
            where piece = pieceAt loc
        
        -- Generate the string representation of the entire board
        boardString = unlines $ map (\r -> show r ++ " |" ++ concatMap (\c -> ' ' : showCell (c,r) ++ " |") columns) rows
    in
    -- Return the final string representation of the board with row and column labels and player turn indicator
    "    a    b    c    d    e    f    g    h\n" ++ boardString ++ "\nTurn: " ++ show player


location :: Piece -> Location
location (P loc) = loc
location (N loc) = loc
location (K loc) = loc
location (Q loc) = loc
location (R loc) = loc
location (B loc) = loc

main = do
    let board = setBoard
    let boardString = visualizeBoard board
    putStrLn boardString

-- takes as input a piece and a board and outputs a list of possible legal moves
suggestMove:: Piece -> Board -> [Location]

-- check all possible moves for a Bishop (diagonals)
suggestMove (B location) board = d1 ++ d2
  where 
    d1 = mainDiagonal location board (cellType location board) 8
    d2 = antiDiagonal location board  (cellType location board) 8
    
-- check all possible moves for a King (verical + horizontal but once)
suggestMove (K location) board = vertical ++ horizontal ++ d1 ++ d2
  where
    d1 = mainDiagonal location board (cellType location board) 1
    d2 = antiDiagonal location board (cellType location board) 1
    vertical = verticalMove location board (cellType location board) 1
    horizontal = horizontalMove location board (cellType location board) 1
   

-- check all possible moves for a Queen (Bishop + Rook)
suggestMove (Q location) board = axialMove ++ diagonalMove 
  where
    axialMove = suggestMove (R location) board
    diagonalMove = suggestMove (B location) board
    

-- check all possible moves for a Rook (vertical + horizontal)
suggestMove (R location) board = vertical ++ horizontal
  where
    vertical = verticalMove location board (cellType location board) 8
    horizontal = horizontalMove location board (cellType location board) 8
   
-- check all possible moves for a Knight (L shape)
-- p stands for point
suggestMove (N (char , int) ) board = p1 ++ p2 ++ p3 ++ p4 ++ p5 ++ p6 ++ p7 ++ p8
  where 
    sourceColor = cellType (char , int) board
    p1 = helperMove ((changeColumn char 2), int + 1) board 0 0 sourceColor 1
    p2 = helperMove ((changeColumn char 2), int - 1) board 0 0 sourceColor 1
    p3 = helperMove ((changeColumn char (-2)), int + 1) board 0 0 sourceColor 1
    p4 = helperMove ((changeColumn char (-2)), int - 1) board 0 0 sourceColor 1
    p5 = helperMove ((changeColumn char 1), int + 2) board 0 0 sourceColor 1
    p6 = helperMove ((changeColumn char (-1)), int + 2) board 0 0 sourceColor 1
    p7 = helperMove ((changeColumn char 1), int - 2) board 0 0 sourceColor 1
    p8 = helperMove ((changeColumn char (-1)), int - 2) board 0 0 sourceColor 1

-- check all possible moves for a pawn
-- clear romves any piece of the enemy type in the forward move
-- since the method move will consider it a ligall move but it is not
-- in case of black all directions are reversed due to the view and the logic of the helper methods
suggestMove (P location) board = forward ++ forwardright ++ forwardleft
  where 
    forward | (cellType location board) == 1 && secondRow location = clear 2 (moveUp location board 1 2) board
      | (cellType location board) == 1  = clear 2 (moveUp location board 1 1) board
      | seventhRow location = clear 1 (moveDown location board 2  2) board
      | otherwise = clear 1 (moveDown location board 2  1) board
    forwardright | (cellType location board) == 1 &&  length (upRight location board 1 1) == 1 && cellType ( (upRight location board 1 1) !! 0) board /= 0 = upRight location board 1 1
       | (cellType location board) == 2 && length (downLeft location board 2 1) == 1 && cellType ((downLeft location board 2 1) !! 0) board  /= 0 = downLeft location board 2 1
       |otherwise = []
    forwardleft | (cellType location board) == 1 &&  length (upLeft location board 1 1) /= 0  && cellType ( ( upLeft location board 1 1) !! 0) board /= 0 = upLeft location board 1 1
       |(cellType location board) == 2 &&  length (downRight location board 2 1) /= 0 && cellType (( downRight location board 2 1) !! 0) board /= 0 = downRight location board 2 1
       | otherwise = []


-- takes a piece, board and a location 
-- returns whether it is legal to move the piece to that location
isLegal :: Piece -> Board -> Location -> Bool

-- checks whether that location is included in the suggested moves for the piece
isLegal piece board location = elem location (suggestMove piece board)

-- takes a piece, location and oldBoard
-- returns newBoard
move :: Piece -> Location -> Board -> Board
move piece location (player , white , black)
    | cellType (getLocation piece) (player , white , black) == 2 && player == White
      = error "This is White player’s turn, Black can’t move."
    | cellType (getLocation piece) (player , white , black) == 1 && player == Black
      = error "This is Black player’s turn, White can’t move."
    | not (isLegal piece (player,white,black) location)
      = error ("Illegal move for piece " ++ (show piece))
    | otherwise = (otherPlayer,l1,l2)
      where
        otherPlayer = oppositePlayer player
        l1 = updateList piece location white
        l2 = updateList piece location black









-- General helper functions
   
-- takes a locations and checks if it is in bounds
isValidLocation :: Location -> Bool
isValidLocation (char, int) = int >= 1 && int <= 8 && (validChar char)

-- checks if a char is between 'a' and 'h' inclusive
validChar :: Char -> Bool
validChar char = code >= 97 && code <= 104
   where code = ord char


-- takes location returns 0 if cell at given location is empty, 
-- 1 if a white piece exists in it and 2 if a black piece exists in it
cellType :: Location -> Board -> Int 
cellType location (player, white, black) 
    | contains white location = 1
    | contains black location = 2
    | otherwise = 0
                                         
-- takes in a list of pieces and a location
-- returns if the list contains a piece in the given location
contains :: [Piece] -> Location -> Bool
contains [] _ = False
contains (piece : rest) location  
    | getLocation piece == location = True
    | otherwise = contains rest location


-- given a piece it returns its location
getLocation :: Piece -> Location
getLocation (P location) = location
getLocation (N location) = location
getLocation (K location) = location
getLocation (Q location) = location
getLocation (R location) = location
getLocation (B location) = location


-- takes the piece and the newLocation
-- returns a piece with the same type but with the newLocation
setLocation :: Piece -> Location -> Piece
setLocation (P oldLocation) newLocation = P newLocation
setLocation (N oldLocation) newLocation = N newLocation
setLocation (K oldLocation) newLocation = K newLocation
setLocation (Q oldLocation) newLocation = Q newLocation
setLocation (R oldLocation) newLocation = R newLocation
setLocation (B oldLocation) newLocation = B newLocation


-- adds delta to current column
changeColumn :: Char -> Int -> Char
changeColumn column delta = chr ((ord column) + delta)

-- takes a player
-- returns the other player
oppositePlayer :: Player -> Player
oppositePlayer Black = White
oppositePlayer White = Black


-- takes a piece, a location and a list of pieces 
-- returns the list after updating it
updateList :: Piece -> Location -> [Piece] -> [Piece]
updateList piece location [] = []
updateList piece location (firstPiece : rest)
  | piece == firstPiece = (setLocation piece location) : rest
  | getLocation firstPiece == location = rest
  | otherwise = firstPiece : updateList piece location rest


    
    
    
    


-- Helper functions for suggestMove

-- generic move
helperMove :: Location -> Board -> Int -> Int -> Int -> Int -> [Location]
helperMove (column, row) board dx dy sourceColor moves
    | not (isValidLocation (column, row)) || moves == 0 || (cellType (column, row) board) == sourceColor  = []
    | let color = cellType (column, row) board in color > 0 && color /= sourceColor = [(column, row)]
    | otherwise = [(column, row)] ++ helperMove ((changeColumn column dx), row + dy) board dx dy sourceColor (moves - 1)
    


-- vertical moves
verticalMove :: Location -> Board -> Int -> Int -> [Location]
verticalMove location board sourceColor moves = up ++ down
  where
    up = moveUp location board sourceColor moves
    down = moveDown location board sourceColor moves

moveUp :: Location -> Board -> Int -> Int -> [Location]
moveUp (column, row) board sourceColor moves =
    helperMove (column, row + 1) board 0 1 sourceColor moves

moveDown :: Location -> Board -> Int -> Int -> [Location]
moveDown (column, row) board sourceColor moves = 
    helperMove (column, row - 1) board 0 (-1) sourceColor moves



-- horizontal moves
horizontalMove :: Location -> Board -> Int -> Int -> [Location]
horizontalMove location board sourceColor moves = left ++ right
  where 
    left = moveLeft location board sourceColor moves
    right = moveRight location board sourceColor moves

moveRight :: Location -> Board -> Int -> Int -> [Location]
moveRight (column, row) board sourceColor moves =
    helperMove ((changeColumn column  1), row) board 1 0 sourceColor moves

moveLeft :: Location -> Board -> Int -> Int -> [Location]
moveLeft (column, row) board sourceColor moves = 
    helperMove ((changeColumn column (-1)), row) board (-1) 0 sourceColor moves
    
    
    
-- diagonal moves
mainDiagonal :: Location -> Board -> Int -> Int -> [Location]
mainDiagonal location board sourceColor moves = ur ++ dl
  where 
    ur = upRight location board sourceColor moves
    dl = downLeft location board sourceColor moves

antiDiagonal :: Location -> Board -> Int -> Int -> [Location]
antiDiagonal location board sourceColor moves = ul ++ dr
  where 
    ul = upLeft location board sourceColor moves
    dr = downRight location board sourceColor moves

upRight :: Location -> Board -> Int -> Int -> [Location]
upRight (column, row) board sourceColor moves =
    helperMove ((changeColumn column 1), row + 1) board 1 1 sourceColor moves

upLeft :: Location -> Board -> Int -> Int -> [Location]
upLeft (column, row) board sourceColor moves =
    helperMove ((changeColumn column (-1)), row + 1) board (-1) 1 sourceColor moves

downRight :: Location -> Board -> Int -> Int -> [Location]
downRight (column, row) board sourceColor moves =
    helperMove ((changeColumn column  1), row - 1) board 1 (-1) sourceColor moves

downLeft :: Location -> Board -> Int -> Int -> [Location]
downLeft (column, row) board sourceColor moves =
    helperMove ((changeColumn column (-1)), row - 1) board (-1) (-1) sourceColor moves

-- takes a loctaion and checks whether it is in the second row
secondRow (_ , 2) = True
secondRow _ = False

-- takes a loctaion and checks whether it is in  row 7
seventhRow (_ , 7) = True
seventhRow _ = False



-- this function removes locations from a list that holds a specific piece

clear t list board = [x | x <- list , cellType x board  /= t] 



--- different boards to test the code with

board1 = (White, whitePieces, blackPieces)
  where
    whitePieces = [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1),
                   Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1),
                   P ('b', 2), P ('c', 2), P ('d', 2), P ('e', 2), 
                   P ('f', 2), P ('a', 2), P ('g', 2), P ('h', 2)]
                   
    blackPieces = [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), 
                   Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),
                   P ('b', 7), P ('c', 7), P ('d', 7), P ('e', 7),
                   P ('f', 7), P ('a', 7), P ('g', 7), P ('h', 7)]



board2 = (White, whitePieces, blackPieces)
  where
    whitePieces = [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1),
                   Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1),
                   P ('b', 2), P ('c', 2), P ('d', 2), P ('e', 2), 
                   P ('f', 2), P ('a', 2), P ('g', 2), P ('h', 2)]
                   
    blackPieces = [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), 
                   Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),
                   P ('b', 7), P ('c', 7), P ('d', 7), P ('e', 7),
                   P ('f', 7), P ('a', 7), P ('g', 7), P ('h', 7)]

board3 = (White, whitePieces, blackPieces)
  where
    whitePieces = [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1),
                   Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1),
                   P ('b', 2), P ('c', 4), P ('d', 4), P ('e', 4), 
                   P ('f', 2), P ('a', 2), P ('g', 2), P ('h', 4)]
                   
    blackPieces = [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), 
                   Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8),
                   P ('b', 7), P ('c', 5), P ('d', 5), P ('e', 5),
                   P ('f', 7), P ('a', 7), P ('g', 5), P ('h', 5)]






