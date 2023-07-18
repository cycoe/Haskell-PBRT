module Libs.ProgressBar
  ( RenderBar
  , newRenderBar
  , incRenderProgress) where

import qualified System.ProgressBar as P
import qualified Data.Text.Lazy as T
import Data.Time (diffUTCTime, nominalDiffTimeToSeconds)
import Numeric (showFFloat)

type RenderBar s = P.ProgressBar s

_loopForSPPStyle :: P.Style s
_loopForSPPStyle = P.defStyle
  { P.stylePrefix = P.msg $ T.pack "Rendering"
  , P.stylePostfix = _loopForSPPLabel
  }

_updateInterval :: Double
_updateInterval = 10

_makeProgress :: Int -> P.Progress ()
_makeProgress n = P.Progress 0 n ()

_remainTimeLabel :: P.Label s
_remainTimeLabel = P.remainingTime P.renderDuration $ T.pack "..."

_sampleRateLabel :: P.Label s
_sampleRateLabel = P.Label render where
  render (P.Progress done _ _) (P.Timing start current)
    | done <= 0 || diff < 1 = defaultMsg
    | diff > sample = T.pack $ showFFloat (Just 1) (diff / sample) "s/spp"
    | otherwise     = T.pack $ showFFloat (Just 1) (sample / diff) "spp/s"
    where
      defaultMsg = T.pack "..."
      diff = realToFrac . nominalDiffTimeToSeconds $ diffUTCTime current start
      sample = fromIntegral done :: Float

_loopForSPPLabel :: P.Label s
_loopForSPPLabel = P.Label render where
  render p t = T.concat [ P.runLabel _remainTimeLabel p t
                        , T.pack " ("
                        , P.runLabel _sampleRateLabel p t
                        , T.pack ")"
                        ]

newRenderBar :: Int -> IO (RenderBar ())
newRenderBar n = P.newProgressBar _loopForSPPStyle _updateInterval $ _makeProgress n

incRenderProgress :: RenderBar s -> IO ()
incRenderProgress = flip P.incProgress 1
