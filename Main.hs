module Main (main) where

import Options.Applicative
import Libs.Configs (Configs(..))

configP :: Parser Configs
configP = Configs
  <$> switch ( long "quick-check"
            <> short 'q'
            <> help "Quick check rendering result with low resolutions and spp"
            <> showDefault
             )

main :: IO ()
main = render =<< execParser opts where
  opts = info (configP <**> helper)
    ( fullDesc
   <> header "Physical-based ray tracing, but with Haskell."
    )

render :: Configs -> IO ()
render configs = print configs
