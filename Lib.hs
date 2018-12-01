-- | Contains argument parsing and other convenience functions
module Lib where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Control.Monad                  ( unless )
import           Control.Monad.Fail
import           Control.Monad.IO.Class

data StandardArgs = StdArgs
  { inputFile :: FilePath
  , part      :: Int
  }

-- | Define a parser for standard arguments
getParser :: Parser StandardArgs
getParser =
  StdArgs
    <$> strOption
          (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
    <*> option
          auto
          (  long "part"
          <> help "Which part of the solution (1|2)"
          <> showDefault
          <> value 1
          <> metavar "INT"
          )

-- | Validates standard arguments
validate :: MonadFail m => StandardArgs -> m StandardArgs
validate arg@(StdArgs _ p) = do
  unless (p == 1 || p == 2) $ error "part argument must be either 1 or 2"
  pure arg

-- | Parse arguments given to the application and validates them
parseArgs :: (MonadIO m, MonadFail m) => m StandardArgs
parseArgs = validate =<< (liftIO . execParser $ info getParser fullDesc)
