module MachineSpec (spec) where

import           Control.Monad       (foldM)
import           Data.Bits           ((.&.))
import           Data.Either         (fromRight)
import           Data.Word           (Word8)
import           Lens.Micro.Platform (each, ix, over, set, view)
import           Machine
import qualified System.Random       as R
import           Test.Hspec          (Spec, describe, it, shouldBe)

mkMachine :: [Word8] -> Machine
mkMachine bs = fromRight err m
  where m = loadRom 0x200 bs . initMachine $ R.mkStdGen 0
        err = error "Failed to make test machine"

stepN :: Int -> Machine -> Either Error Machine
stepN n m = foldM (flip ($)) m $ replicate n step

spec :: Spec
spec = do
  describe "step" $ do
    it "can clear frame buffer" $ do
      let m = set (fb . pixels . ix 0) True
              . set (fb . dirty) False
              $ mkMachine [0x00, 0xe0]
      let r = over (fb . pixels . each) (const False)
              . set (fb . dirty) True
              . set pc 0x202 $ m
      step m `shouldBe` Right r

    it "can jump to a memory address" $ do
      let m = mkMachine [0x12, 0x34]
      let r = set pc 0x234 m
      step m `shouldBe` Right r

    it "can jump to a position starting from register 0" $ do
      let m = set (drs . ix 0) 0x1 $ mkMachine [0xb1, 0x23]
      let r = set pc 0x124 m
      step m `shouldBe` Right r

    it "can call a subroutine" $ do
      let m = mkMachine [0x22, 0x34]
      let r = set pc 0x234
              . set sp 1
              . set (stack . ix 0) 0x200 $ m
      step m `shouldBe` Right r

    it "can return from a subroutine" $ do
      let m = mkMachine [0x22, 0x04, 0x00, 0xe0, 0x00, 0xee]
      let r = set pc 0x202
              . set (stack . ix 0) 0x200 $ m
      stepN 2 m `shouldBe` Right r

    it "can handle nested subroutine calls" $ do
      let m = mkMachine [ 0x22, 0x04
                        , 0x00, 0xe0
                        , 0x22, 0x08
                        , 0x00, 0xee
                        , 0x00, 0xee
                        ]
      let r = set pc 0x202
              . set (stack . ix 0) 0x200
              . set (stack . ix 1) 0x204 $ m
      stepN 4 m `shouldBe` Right r

    it "can skip instruction if a value and a register equal" $ do
      let m = set (drs . ix 0) 0x11 $ mkMachine [0x30, 0x11]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if a value and a register do not equal" $ do
      let m = set (drs . ix 0) 0x10 $ mkMachine [0x30, 0x11]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can skip instruction if a value and a register do not equal" $ do
      let m = set (drs . ix 0) 0x10 $ mkMachine [0x40, 0x11]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if a value and a register equal" $ do
      let m = set (drs . ix 0) 0x11 $ mkMachine [0x40, 0x11]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can skip instruction if two registers equal" $ do
      let m = set (drs . ix 0) 0x10
              . set (drs . ix 1) 0x10
              $ mkMachine [0x50, 0x10]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if two registers do not equal" $ do
      let m = set (drs . ix 0) 0x10
              . set (drs . ix 1) 0x11
              $ mkMachine [0x50, 0x10]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can skip instruction if two registers do not equal" $ do
      let m = set (drs . ix 0xa) 0x11
              . set (drs . ix 0xb) 0x10
              $ mkMachine [0x9a, 0xb0]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if two registers equal" $ do
      let m = set (drs . ix 0xa) 0x11
              . set (drs . ix 0xb) 0x11
              $ mkMachine [0x9a, 0xb0]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can skip instruction if a key is pressed" $ do
      let m = set (keypad . ix 0x9) keyPress
              . set (drs . ix 0x0) 0x9
              $ mkMachine [0xe0, 0x9e]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if a key is not pressed" $ do
      let m = set (drs . ix 0x0) 0x9 $ mkMachine [0xe0, 0x9e]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can skip instruction if a key is not pressed" $ do
      let m = set (drs . ix 0x0) 0x9 $ mkMachine [0xe0, 0xa1]
      let r = set pc 0x204 m
      step m `shouldBe` Right r

    it "can stay on instruction if a key is pressed" $ do
      let m = set (keypad . ix 0x9) keyPress
              . set (drs . ix 0x0) 0x9
              $ mkMachine [0xe0, 0xa1]
      let r = set pc 0x202 m
      step m `shouldBe` Right r

    it "can store value to a register" $ do
      let m = mkMachine [0x6a, 0x11]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x11 $ m
      step m `shouldBe` Right r

    it "can copy a register to another" $ do
      let m = set (drs . ix 0xb) 0x11 $ mkMachine [0x8a, 0xb0]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x11 $ m
      step m `shouldBe` Right r

    it "can store a memory address" $ do
      let m = mkMachine [0xa1, 0x23]
      let r = set pc 0x202 . set ir 0x123 $ m
      step m `shouldBe` Right r

    it "can store delay timer value to a register" $ do
      let m = set dt 0xf7 $ mkMachine [0xfa, 0x07]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xf7 $ m
      step m `shouldBe` Right r

    it "can store pressed key code to a register" $ do
      let m = set (keypad . ix 0x9) keyPress $ mkMachine [0xfa, 0x0a]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x9 $ m
      step m `shouldBe` Right r

    it "can store the address of sprite for a character" $ do
      let m = set (drs . ix 0x1) 0xa $ mkMachine [0xf1, 0x29]
      let r = set pc 0x202 . set ir 50 $ m
      step m `shouldBe` Right r

    it "can store BCD of a number to memory" $ do
      let m = set ir 0x204
              . set (drs . ix 0x1) 123
              $ mkMachine [0xf1, 0x33]
      let r = set pc 0x202
              . set (mem . ix 0x204) 1
              . set (mem . ix 0x205) 2
              . set (mem . ix 0x206) 3 $ m
      step m `shouldBe` Right r

    it "can set delay timer" $ do
      let m = set (drs . ix 0xa) 0x10 $ mkMachine [0xfa, 0x15]
      let r = set pc 0x202 . set dt 0x10 $ m
      step m `shouldBe` Right r

    it "can set sound timer" $ do
      let m = set (drs . ix 0xa) 0x10 $ mkMachine [0xfa, 0x18]
      let r = set pc 0x202 . set st 0x10 $ m
      step m `shouldBe` Right r

    it "can add value to a register" $ do
      let m = mkMachine [0x7a, 0x11]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x11 $ m
      step m `shouldBe` Right r

    it "can take or on two registers" $ do
      let m = set (drs . ix 0xa) 0xf0
              . set (drs . ix 0xb) 0x0f
              $ mkMachine [0x8a, 0xb1]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xff $ m
      step m `shouldBe` Right r

    it "can take and on two registers" $ do
      let m = set (drs . ix 0xa) 0xf0
              . set (drs . ix 0xb) 0x0f
              $ mkMachine [0x8a, 0xb2]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x00 $ m
      step m `shouldBe` Right r

    it "can take xor on two registers" $ do
      let m = set (drs . ix 0xa) 0xf0
              . set (drs . ix 0xb) 0x00
              $ mkMachine [0x8a, 0xb3]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xf0 $ m
      step m `shouldBe` Right r

    it "can add a register to another with carry" $ do
      let m = set (drs . ix 0xa) 0xff
            . set (drs . ix 0xb) 0x01
            $ mkMachine [0x8a, 0xb4]
      let r = set pc 0x202
              . set (drs . ix 0xa) 0
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can add a register to another without carry" $ do
      let m = set (drs . ix 0xa) 0xf0
            . set (drs . ix 0xb) 0x01
            $ mkMachine [0x8a, 0xb4]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xf1 $ m
      step m `shouldBe` Right r

    it "can subtract a register from another with borrow" $ do
      let m = set (drs . ix 0xa) 0x1
              . set (drs . ix 0xb) 0x2
              $ mkMachine [0x8a, 0xb5]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xff $ m
      step m `shouldBe` Right r

    it "can subtract a register from another without borrow" $ do
      let m = set (drs . ix 0xa) 0x2
              . set (drs . ix 0xb) 0x1
              $ mkMachine [0x8a, 0xb5]
      let r = set pc 0x202
              . set (drs . ix 0xa) 0x1
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can right shift a register with non-zero LSB" $ do
      let m = set (drs . ix 0xa) 0xff $ mkMachine [0x8a, 0xb6]
      let r = set pc 0x202
              . set (drs . ix 0xa) 0x7f
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can right shift a register with zero LSB" $ do
      let m = set (drs . ix 0xa) 0xf0 $ mkMachine [0x8a, 0xb6]
      let r = set pc 0x202 . set (drs . ix 0xa) 0x78 $ m
      step m `shouldBe` Right r

    it "can reversely subtract a register from another with borrow" $ do
      let m = set (drs . ix 0xa) 0x2
              . set (drs . ix 0xb) 0x1
              $ mkMachine [0x8a, 0xb7]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xff $ m
      step m `shouldBe` Right r

    it "can reversely subtract a register from another without borrow" $ do
      let m = set (drs . ix 0xa) 0x1
              . set (drs . ix 0xb) 0x2
              $ mkMachine [0x8a, 0xb7]
      let r = set pc 0x202
              . set (drs . ix 0xa) 0x1
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can left shift a register with non-zero MSB" $ do
      let m = set (drs . ix 0xa) 0xff $ mkMachine [0x8a, 0xbe]
      let r = set pc 0x202
              . set (drs . ix 0xa) 0xfe
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can left shift a register with zero MSB" $ do
      let m = set (drs . ix 0xa) 0x7f $ mkMachine [0x8a, 0xbe]
      let r = set pc 0x202 . set (drs . ix 0xa) 0xfe $ m
      step m `shouldBe` Right r

    it "can add a register to I with carry" $ do
      let m = set ir 0xfff
              . set (drs . ix 0xa) 0x1
              $ mkMachine [0xfa, 0x1e]
      let r = set pc 0x202
              . set ir 0x0
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can add a register to I without carry" $ do
      let m = set ir 0xffe
              . set (drs . ix 0xa) 0x1
              $ mkMachine [0xfa, 0x1e]
      let r = set pc 0x202 . set ir 0xfff $ m
      step m `shouldBe` Right r

    it "can sample a random number" $ do
      let m = mkMachine [0xca, 0xf7]
      let (n, g) = R.random $ view rng m
      let r = set pc 0x202
              . set (drs . ix 0xa) (n .&. 0xf7)
              . set rng g $ m
      step m `shouldBe` Right r

    it "can draw a sprite with collision" $ do
      let m = set ir 0x204
              . set (mem . ix 0x204) 0x10
              . set (mem . ix 0x205) 0x10
              . set (fb . dirty) False
              . set (fb . pixels . ix 3) True
              $ mkMachine [0xd0, 0x12]
      let r = set pc 0x202
              . set (fb . dirty) True
              . set (fb . pixels . ix 3) False
              . set (fb . pixels . ix 67) True
              . set (drs . ix 0xf) 1 $ m
      step m `shouldBe` Right r

    it "can draw a sprite without collision" $ do
      let m = set ir 0x204
              . set (mem . ix 0x204) 0x10
              . set (mem . ix 0x205) 0x80
              . set (fb . dirty) False
              $ mkMachine [0xd0, 0x12]
      let r = set pc 0x202
              . set (fb . dirty) True
              . set (fb . pixels . ix 3) True
              . set (fb . pixels . ix 64) True $ m
      step m `shouldBe` Right r

    it "can draw to a dirty frame buffer" $ do
      let m = set ir 0x204
              . set (mem . ix 0x204) 0x10
              $ mkMachine [0xd0, 0x11]
      let r = set pc 0x202
              . set (fb . dirty) True
              . set (fb . pixels . ix 3) True $ m
      step m `shouldBe` Right r

    it "can wait on key press" $ do
      let m = mkMachine [0xfa, 0x0a]
      let r = set pc 0x200 . set paused True $ m
      step m `shouldBe` Right r

    it "can dump registers to memory" $ do
      let m = set (drs . ix 0) 1
              . set (drs . ix 1) 2
              . set (drs . ix 2) 3
              . set ir 0x204
              $ mkMachine [0xf2, 0x55]
      let r = set pc 0x202
              . set (mem . ix 0x204) 1
              . set (mem . ix 0x205) 2
              . set (mem . ix 0x206) 3 $ m
      step m `shouldBe` Right r

    it "can load registers from memory" $ do
      let m = set (mem . ix 0x204) 1
              . set (mem . ix 0x205) 2
              . set (mem . ix 0x206) 3
              . set ir 0x204
              $ mkMachine [0xf2, 0x65]
      let r = set pc 0x202
              . set (drs . ix 0) 1
              . set (drs . ix 1) 2
              . set (drs . ix 2) 3 $ m
      step m `shouldBe` Right r

    it "fails on invalid opcode" $ do
      let m = mkMachine [0xff, 0xff]
      let r = Error { kind = UnknownOpcode, machine = m }
      step m `shouldBe` Left r

    it "fails on too many subroutine calls" $ do
      let m = mkMachine [0x22, 0x00]
      let r = Error { kind = StackOverflow
                    , machine = set sp 12 . set (stack . each) 0x200 $ m
                    }
      stepN 20 m `shouldBe` Left r

    it "fails on too many returns" $ do
      let m = mkMachine [0x22, 0x04, 0x00, 0xee, 0x00, 0xee]
      let r = Error { kind = InvalidSP
                    , machine = set pc 0x202
                                . set sp 0
                                . set (stack . ix 0) 0x200 $ m
                    }
      stepN 10 m `shouldBe` Left r

  describe "decayTimers" $ do
    it "decrements ST and DT by 1 if both are positive" $ do
      let m = set dt 0xff . set st 0xaa $ mkMachine []
      let r = set dt 0xfe . set st 0xa9 $ m
      decayTimers m `shouldBe` r

    it "does nothing on a timer if it is 0" $ do
      let m = mkMachine []
      decayTimers m `shouldBe` m

  describe "pressKey" $ do
    it "sets a key to pressed" $ do
      let m = mkMachine []
      let r = set (keypad . ix 0xa) keyPress m
      pressKey 0xa m `shouldBe` Right r

    it "fails on invalid key codes" $ do
      let ks = [0x10 .. 0xff]
      let m = mkMachine []
      let r = Error { kind = InvalidKey, machine = m }
      map (`pressKey` m) ks `shouldBe` map (const $ Left r) ks

  describe "decayKeypad" $
    it "decrements all positive key values by 1" $ do
      let m = set (keypad . ix 0xa) keyPress $ mkMachine []
      let r = set (keypad . ix 0xa) (keyPress - 1) m
      decayKeypad m `shouldBe` r

  describe "clearKeypad" $
    it "sets all keys to unpressed" $ do
      let m = set (keypad . ix 0xa) keyPress $ mkMachine []
      let r = set (keypad . each) 0 m
      clearKeypad m `shouldBe` r

-- Local Variables:
-- dante-target: "chip8hs:test:spec"
-- End:
