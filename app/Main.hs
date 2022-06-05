{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}

module Main (main) where

import           Options.Applicative
import           Run

data Cmd = CmdRun CommonOpts RunOpts
         | CmdDisasm CommonOpts DisasmOpts

commonOpts :: Parser CommonOpts
commonOpts = do
  f <- argument str
       $ metavar "FILE"
       <> help "ROM file to use"
  s <- option auto
       $ metavar "ADDR"
       <> help "Starting address of program"
       <> long "start-addr"
       <> value 0x200
       <> showDefault
       <> hidden
  pure $ CommonOpts { file = f, startAddr = s }

runOpts :: Parser RunOpts
runOpts = do
  s <- option auto
       $ metavar "SPEED"
       <> help "Number of steps executed per second"
       <> long "speed"
       <> value 1200
       <> showDefault
       <> hidden
  r <- option auto
       $ metavar "RATE"
       <> help "Number of screen redraws per second"
       <> long "redraw-rate"
       <> value 24
       <> showDefault
       <> hidden
  k <- option auto
       $ metavar "TIME"
       <> help "Time before clearing a key press in milliseconds"
       <> long "key-timeout"
       <> value 100
       <> showDefault
       <> hidden
  g <- optional . option auto
       $ metavar "SEED"
       <> help "Initial random seed"
       <> long "seed"
       <> hidden
  pure $ RunOpts { speed = s
                 , redrawRate = r
                 , keyTimeout = k
                 , seed = g
                 }

cmdRun :: Parser Cmd
cmdRun = CmdRun <$> commonOpts <*> runOpts

cmdDisasm :: Parser Cmd
cmdDisasm = flip CmdDisasm () <$> commonOpts

cmd :: ParserInfo Cmd
cmd = info (p <**> helper)
  $ fullDesc
  <> progDesc "Run or disassemble CHIP-8 ROM"
  <> header "chip8hs - CHIP-8 emulator and disassembler"
  where r = command "run" $ info (cmdRun <**> helper) (progDesc "Run ROM")
        d = command "disasm"
            $ info (cmdDisasm <**> helper) (progDesc "Disassemble ROM")
        p = subparser (r <> d)

main :: IO ()
main = execParser cmd >>= \case
  CmdRun xs ys    -> run xs ys
  CmdDisasm xs ys -> disasm xs ys
