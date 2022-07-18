{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MagicarpBarbaraFRP (main) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Termbox.Banana as TB

import Control.Concurrent (forkIO, threadDelay)
import GHC.Float.RealFracMethods (roundDoubleInt)
import System.Random (initStdGen, uniformR)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- (\as -> if null as then ["magicarp"] else as) <$> getArgs
  TB.run $ program $ unwords args

program ::
    String ->
    Event TB.TermboxEvent ->
    Behavior (Int, Int) ->
    MomentIO (Behavior (TB.Cells, TB.Cursor), Event ())
program text tbEvent bSize = do
    tickEvent <- tickLoop
    let bText = pure text
        bTextLength = length <$> bText
    (xPos, yPos, eBounce) <- makeSimulationBehavior tickEvent bSize bTextLength
    bColor <- makeRandomColor eBounce
    let eClose = keyEvent 'q'
        theText =
            label <$> bText
                  <*> bColor
                  <*> (pure mempty)
                  <*> (roundDoubleInt <$> xPos)
                  <*> (roundDoubleInt <$> yPos)

    return $ ((,TB.NoCursor) <$> theText, eClose)
  where
    keyEvent :: Char -> Event ()
    keyEvent c' =
        ()
            <$ filterE
                (\case TB.EventKey (TB.KeyChar c) -> c == c'; _ -> False)
                tbEvent

makeRandomColor :: Event a -> MomentIO (Behavior TB.Attr)
makeRandomColor ev = do
        let colors = [TB.red
                     ,TB.cyan
                     ,TB.green
                     ,TB.magenta
                     ,TB.yellow
                     ,TB.blue
                     ,TB.black
                     ,TB.white]
        gen <- (liftIO initStdGen)
        (eNewColor, _bRNG) <-
            mapAccum gen $
                ( \g ->
                    let (i, g') = uniformR (0, length colors - 1) g
                     in (colors !! i, g')
                ) <$ ev
        stepper mempty eNewColor

tickLoop :: MomentIO (Event ())
tickLoop = do
    (tickEvent, fireTickEvent) <- newEvent
    let ticker = do
            fireTickEvent ()
            threadDelay $ roundDoubleInt $ 10 ^ 6 / 30
            ticker
    _ <- liftIO $ forkIO ticker
    return tickEvent

type XPositionBehavior = Behavior Double
type YPositionBehavior = Behavior Double
type BounceEvent = Event ()

makeSimulationBehavior :: Event () -> Behavior (Int,Int) -> Behavior Int -> MomentIO (XPositionBehavior, YPositionBehavior, BounceEvent)
makeSimulationBehavior tickEvent bSize bObjectWidth = mdo
        let speed = 0.25 :: Double
        xVel :: Behavior Double <-
            accumB speed $
                unions
                    [ (const speed) <$ eBounceLeft
                    , (const $ - speed) <$ eBounceRight
                    ]
        yVel :: Behavior Double <-
            accumB speed $
                unions
                    [ (const speed) <$ eBounceTop
                    , (const $ - speed) <$ eBounceBottom
                    ]
        xPos <- accumB 0 $ (+) <$> (xVel <@ tickEvent)
        yPos <- accumB 0 $ (+) <$> (yVel <@ tickEvent)
        let eBounceLeft =
                whenE ((< 0) <$> xVel) $ -- moving to the left
                    whenE ((< 0) <$> xPos) tickEvent

            eBounceTop =
                whenE ((< 0) <$> yVel) $
                    whenE ((< 0) <$> yPos) tickEvent
            eBounceBottom =
                whenE ((> 0) <$> yVel) $
                    whenE
                        ( (>=)
                            <$> yPos
                            <*> ((subtract 1 . fromIntegral . snd) <$> bSize)
                        )
                        tickEvent

            eBounceRight =
                whenE ((> 0) <$> xVel) $ -- moving to the right
                    whenE
                        ( (>=)
                            <$> xPos
                            <*> ( (\(x, _) width -> fromIntegral $ x - width)
                                    <$> bSize
                                    <*> bObjectWidth
                                )
                        )
                        tickEvent
        let anyBounce =
                ()
                    <$ merge
                        (merge eBounceLeft eBounceRight)
                        (merge eBounceTop eBounceBottom)
        return (xPos, yPos, anyBounce)

type PosX = Int
type PosY = Int
label :: String -> TB.Attr -> TB.Attr -> PosX -> PosY -> TB.Cells
label str fg bg posX posY =
    mconcat
        [ TB.set deltaPosX posY (TB.Cell c fg bg)
        | (c, deltaPosX) <- zip str [posX ..]
        ]
