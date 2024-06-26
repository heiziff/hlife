module Graphics.Life (initGame, encodeCellState, GameState, tick, gameHeight, gameWidth) where

import Data.ByteString.Builder (word8)
import qualified Data.ByteString.Builder as B
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import System.Random
import System.Random.Stateful (Uniform (uniformM), applyIOGen, newIOGenM)

-- | Type to represent the state of a game
type GameState = V.Vector CellState

data CellState = Dead | Alive
    deriving (Show, Enum, Bounded)

instance Uniform CellState where
    uniformM s = do
        b <- uniformM s
        if b then pure Alive else pure Dead

-- | Encodes a cell in a Bytestring Builder for displaying it on the screen
encodeCellState :: CellState -> B.Builder
encodeCellState Dead = word8 0xFF
encodeCellState Alive = word8 0x0

type Position = (Int, Int)

-- | The height of the game grid
gameHeight :: Int
gameHeight = 100

-- | The width of the game grid
gameWidth :: Int
gameWidth = 100

{- |
  Initiates a game of life by giving each cell a random value
-}
initGame :: IO GameState
initGame = V.generateM (gameHeight * gameWidth) (const genVal)
  where
    genVal = do
        rand <- initStdGen
        rr <- newIOGenM rand
        applyIOGen uniform rr

{- |
  Simulates one step in the supplied game
-}
tick :: GameState -> GameState
tick game = V.imap (updateCell game) game

updateCell :: GameState -> Int -> CellState -> CellState
updateCell game idx idxState = case idxState of
    Dead -> if liveNeighborCount == 3 then Alive else Dead
    Alive ->
        if liveNeighborCount < 2
            then Dead
            else if liveNeighborCount <= 3 then Alive else Dead
  where
    liveNeighborCount = sum . map fromEnum $ getNeighbors game idx

getNeighbors :: GameState -> Int -> [CellState]
getNeighbors game idx = mapMaybe (\a -> game V.!? positionToIdx a) neighborIndices
  where
    (x, y) = idxToPosition idx
    neighborIndices = filter isValidIndex [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]
    isValidIndex (xx, yy) = xx >= 0 && xx < gameWidth && yy >= 0 && yy < gameHeight

positionToIdx :: Position -> Int
positionToIdx (x, y) = y * gameWidth + x

idxToPosition :: Int -> Position
idxToPosition idx = (idx `mod` gameWidth, idx `div` gameWidth)
