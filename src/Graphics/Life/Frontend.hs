{-# LANGUAGE OverloadedStrings #-}

module Graphics.Life.Frontend (runGame) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Vector as V
import Graphics.Life
import SDL
import System.Random
import System.Random.Stateful (applyIOGen, newIOGenM)

runGame :: IO ()
runGame = do
    initializeAll
    window <-
        createWindow
            "hlife"
            (WindowConfig True False False Windowed NoGraphicsContext Wherever True (V2 (fromIntegral gameWidth + 20) (fromIntegral gameHeight + 20)) True)
    renderer <- createRenderer window (-1) (RendererConfig AcceleratedVSyncRenderer True)
    game <- V.generateM (gameHeight * gameWidth) (const genVal)
    texture <- createTexture renderer RGB332 TextureAccessStreaming (V2 (fromIntegral gameWidth) (fromIntegral gameHeight))
    appLoop texture game renderer
    destroyWindow window
  where
    genVal = do
        rand <- initStdGen
        rr <- newIOGenM rand
        applyIOGen uniform rr

appLoop :: Texture -> GameState -> Renderer -> IO ()
appLoop texture game renderer = do
    events <- pollEvents
    let isEscapePressed event =
            case eventPayload event of
                KeyboardEvent keyboardEvent ->
                    keyboardEventKeyMotion keyboardEvent == Pressed
                        && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeEscape
                _ -> False
        qPressed = any isEscapePressed events
    let pxData = toLazyByteString $ encodeGameState game
    updateTexture texture Nothing (toStrict pxData) (fromIntegral gameWidth)
    copy renderer texture Nothing Nothing
    present renderer
    threadDelay 100000 -- slow down game speed
    unless qPressed $ appLoop texture (tick game) renderer

encodeGameState :: V.Vector CellState -> B.Builder
encodeGameState = V.foldr (\a acc -> encodeCellState a <> acc) (B.byteString "")
