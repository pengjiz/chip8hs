module Run
  ( CommonOpts (..)
  , RunOpts (..)
  , DisasmOpts
  , run
  , disasm
  ) where

import           Brick              (customMain)
import           Brick.BChan        (newBChan, writeBChan)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (forever, void)
import qualified Data.ByteString    as BS
import           Data.Word          (Word16)
import           Disassemble        (disassemble)
import qualified Graphics.Vty       as T
import           Machine            (initMachine, keyPress, loadRom)
import           System.Exit        (exitFailure)
import qualified System.Random      as R
import           UI                 (Event (..), app)

-- | Common arguments for all commands.
data CommonOpts = CommonOpts
  { -- | ROM file to use.
    file      :: FilePath
    -- | Starting address of program.
  , startAddr :: Word16
  }

-- | Arguments specifically for running a ROM.
data RunOpts = RunOpts
  { -- | Number of steps executed per second.
    speed      :: Int
    -- | Number of screen redraws per second.
  , redrawRate :: Int
    -- | Time before clearing a key press in milliseconds.
  , keyTimeout :: Int
    -- | Initial random seed.
  , seed       :: Maybe Int
  } deriving (Show)

-- | Run a ROM.
run :: CommonOpts -> RunOpts -> IO ()
run copts ropts = do
  -- Set up events
  chan <- newBChan 100
  void . forkIO . forever $ do
    threadDelay $ 1000000 `div` speed ropts
    writeBChan chan Execute
  void . forkIO . forever $ do
    threadDelay $ 1000000 `div` redrawRate ropts
    writeBChan chan Redraw
  void . forkIO . forever $ do
    threadDelay $ 1000000 `div` 60
    writeBChan chan DecayTimers
  void . forkIO . forever $ do
    threadDelay $ (keyTimeout ropts * 1000) `div` keyPress
    writeBChan chan DecayKeypad

  -- Prepare machine
  rom <- BS.unpack <$> BS.readFile (file copts)
  g <- maybe R.newStdGen (pure . R.mkStdGen) $ seed ropts
  let s = loadRom (startAddr copts) rom $ initMachine g

  -- Run
  let bVty = T.mkVty T.defaultConfig
  vty <- bVty
  r <- customMain vty bVty (Just chan) app s

  -- Check result
  case r of
    (Right _) -> return ()
    (Left e)  -> print e >> exitFailure

-- | Options specifically for disassembling a ROM.
type DisasmOpts = ()

-- | Disassemble a ROM.
disasm :: CommonOpts -> DisasmOpts -> IO ()
disasm opts _ = BS.readFile (file opts) >>=
  putStr . disassemble (startAddr opts) . BS.unpack
