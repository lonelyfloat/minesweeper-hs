{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import qualified Data.Vector              as V
import qualified Data.Vector.Mutable      as MV
import           System.Console.Haskeline
import           Text.Printf

data ObjectType = Bomb | Empty deriving (Eq, Ord, Enum, Show)
data Cell = Cell {
    cellType  :: ObjectType,
    isHidden  :: Bool,
    isFlagged :: Bool,
    pos       :: (Int, Int)
    } deriving (Eq, Show)
type Board = V.Vector (V.Vector Cell)

initCell :: (Int, Int) -> Int -> IO Cell
initCell pos density = do
    x <- utctDayTime <$> getCurrentTime
    let r = floor (x * 0x29362842803917) `div` 0o12023 -- NOTE: Both of these numbers are completely arbitrary
    let t = r `mod` 100
    return (Cell {cellType = if t < density then Bomb else Empty, isHidden=True, isFlagged=False, pos=pos})

testBoard :: (Int, Int) -> Int -> IO Board
testBoard (w,h) density =
    V.fromList . map V.fromList <$>
    sequence [sequence [ initCell (x,y) density | x <- [0..w-1]] | y <- [0..h-1]]

getTypeChar :: Board -> Cell -> Char
getTypeChar board ot = case ot of
    Cell _ True flagged _    -> if flagged
        then 'F'
        else '-'
    Cell t False _ pos -> case t of
        Bomb  -> '*'
        Empty ->
            if bombCount /= 0 then head (show bombCount) else ' '
            where bombCount = getNeighborData board pos ((==Bomb) . cellType)

inBounds' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
inBounds' (w,h) (x,y) (kx,ky) = y + ky >= 0 && y + ky < h && x + kx >= 0 && x + kx < w && not (kx == 0 && ky == 0)

inBounds :: Board -> (Int, Int) -> (Int, Int) -> Bool
inBounds board (x,y) (kx,ky) = inBounds' (length (board V.! y), length board) (x,y) (kx,ky)

getNeighborData :: Board -> (Int, Int) -> (Cell -> Bool) -> Int
getNeighborData objs (x,y) f = length . filter f
    . map (\t -> ind (x + fst t, y + snd t))
    . filter (inBounds objs (x,y))
    $ concat [[(xi,yi) | xi <- [-1..1]] | yi <- [-1..1]]
  where ind (x,y) = objs V.! y V.! x

genCanvas :: Board -> V.Vector (V.Vector Char)
genCanvas board = V.map (V.map (getTypeChar board)) board

data GameState = GameState {
    board     :: Board,
    playerPos :: (Int, Int),
    endGame   :: Bool,
    turns     :: Int
    }
type Input = Char

floodFiller :: Board -> [(Int, Int)] -> MV.MVector s (V.Vector Cell) -> ST s ()
floodFiller _ [] mv = return ()
floodFiller gameBoard (p@(x,y):xs) mv = do
    when bound (MV.write mv y (V.modify (\ml -> MV.read ml x >>= \t -> MV.write ml x t{isHidden=False}) (gameBoard V.! y)))
    b <- V.freeze mv
    floodFiller b (if bound then left:right:up:down:xs else xs) mv
  where left  = (x-1, y)
        right = (x+1, y)
        up    = (x, y-1)
        down  = (x, y+1)
        bound = inBounds gameBoard (x - 1, y) (1,0) && isHidden (gameBoard V.! y V.! x) && getNeighborData gameBoard p ((==Bomb) . cellType) == 0

dig :: GameState -> GameState
dig state@(GameState gameBoard playerPos@(playerX, playerY) _ _)
  | cellType cell == Bomb = s{endGame=True}
  | getNeighborData gameBoard playerPos ((==Bomb) . cellType) == 0 = s{board=V.modify (floodFiller gameBoard [playerPos]) gameBoard} -- Flood fill here
  | otherwise = s
  where cell = gameBoard V.! playerY V.! playerX
        s = state{board=modified}
        modifier mv = MV.read mv playerX >>= \t -> MV.write mv playerX t{isHidden = False}
        modified = V.modify (\x -> MV.write x playerY (V.modify modifier (gameBoard V.! playerY))) gameBoard

flag :: GameState -> GameState
flag state@(GameState gameBoard (playerX,playerY) _ _) = s
  where s = state{board=modified}
        flagged = isFlagged (gameBoard V.! playerY V.! playerX)
        modifier mv = MV.read mv playerX >>= \t -> MV.write mv playerX t{isFlagged = not flagged}
        modified = V.modify (\x -> MV.write x playerY (V.modify modifier (gameBoard V.! playerY))) gameBoard


update :: GameState -> Input -> GameState
update state@(GameState gameBoard playerPos@(playerX, playerY) _ _) input = case input of
    'w' -> state{playerPos = wrap (playerX, playerY - 1)}
    'a' -> state{playerPos = wrap (playerX - 1, playerY)}
    's' -> state{playerPos = wrap (playerX, playerY + 1)}
    'd' -> state{playerPos = wrap (playerX + 1, playerY)}
    ' ' -> dig state
    'f' -> flag state
    _   -> state
  where wrap (x,y) = (x `mod` (length . (gameBoard V.!) . (y `mod`) . length $ gameBoard), y `mod` length gameBoard)

draw :: GameState -> IO ()
draw state@(GameState gameBoard (playerX, playerY) _ _) =
    let chs = genCanvas . board $ state in
    let vcan = V.modify (\x -> MV.write x playerY (V.modify (\mv -> MV.write mv playerX 'X') (chs V.! playerY) ) ) chs in
    let canvas = intercalate "\n" . V.toList $ V.map V.toList vcan in
    putStrLn canvas

removeLines :: Int -> IO ()
removeLines x = replicateM_ x $ putStr "\x1b[1A\x1b[2K"

loop :: GameState -> InputT IO GameState
loop state@(GameState gameBoard (playerX, playerY) end turns) = if not end then getInputChar "" >>= (\case
        Just 'Q'  -> return state
        Just inp  ->
            let newst = update state inp in
                liftIO (removeLines . (+1) . length . board $ state)
             >> liftIO (draw newst)
             >> loop newst{turns = turns + 1}
        Nothing -> loop state
        ) else return state

revealBombs :: Board -> Board
revealBombs = V.map $ V.map adjustCell
  where adjustCell c = if cellType c == Bomb then c{isHidden=False} else c

main :: IO ()
main = do
    b <- testBoard (20,10) 15
    let is = GameState b (0,0) False 0
    draw is
    f <- runInputT defaultSettings $ loop is
    removeLines (length . board $ f)
    let revealed = f{board=revealBombs . board $ f}
    putStrLn "Game Over"
    printf "You survived %d turns\n" (turns f)
    draw revealed
-- test comment
