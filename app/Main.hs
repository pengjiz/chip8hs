{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase    #-}

module Main (main) where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Run

data Cmd = CmdRun CommonOpts RunOpts
         | CmdDisasm CommonOpts DisasmOpts

commonOpts :: Parser CommonOpts
commonOpts = do
  f <- argument str
       $ metavar "FILE"
       <> help "ROM file path"
  s <- option auto
       $ metavar "N"
       <> help "Starting address of program"
       <> short 's'
       <> value 0x200
       <> showDefault
       <> hidden
  pure $ CommonOpts { file = f, startAddr = s }

runOpts :: Parser RunOpts
runOpts = do
  cf <- option auto
        $ metavar "N"
        <> help "CPU frequency in Hz"
        <> long "cpu-freq"
        <> value 1000
        <> showDefault
        <> hidden
  sf <- option auto
        $ metavar "N"
        <> help "Screen refresh frequency in Hz"
        <> long "screen-freq"
        <> value 30
        <> showDefault
        <> hidden
  kt <- option auto
        $ metavar "N"
        <> help "Key press timeout in milliseconds"
        <> long "key-timeout"
        <> value 100
        <> showDefault
        <> hidden
  r <- optional
       $ option auto
       $ metavar "N"
       <> help "Random seed"
       <> long "seed"
       <> hidden
  pure $ RunOpts { cpuFreq = cf
                 , screenFreq = sf
                 , keyTimeout = kt
                 , seed = r
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
