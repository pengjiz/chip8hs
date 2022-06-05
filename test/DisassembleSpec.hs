module DisassembleSpec (spec) where

import           Control.Monad (forM_)
import           Disassemble
import           Test.Hspec    (Spec, describe, it, shouldBe)
import           Text.Printf   (printf)

spec :: Spec
spec = do
  describe "decode" $ do
    let cases =
          [ ("00e0", "CLS", 0x00e0, Cls)
          , ("00ee", "RET", 0x00ee, Ret)
          , ("0nnn", "SYS nnn", 0x0123, Sys (Mem 0x123))
          , ("1nnn", "JP nnn", 0x1234, Jp (Val 0x0) (Mem 0x234))
          , ("2nnn", "CALL nnn", 0x2345, Call (Mem 0x345))
          , ("3xnn", "SE Vx, nn", 0x3011, Se (DR 0x0) (Val 0x11))
          , ("4xnn", "SNE Vx, nn", 0x4011, Sne (DR 0x0) (Val 0x11))
          , ("5xy0", "SE Vx, Vy", 0x5010, Se (DR 0x0) (DR 0x1))
          , ("6xnn", "LD Vx, nn", 0x6a11, Ld (DR 0xa) (Val 0x11))
          , ("7xnn", "ADD Vx, nn", 0x7a11, Add (DR 0xa) (Val 0x11))
          , ("8xy0", "LD Vx, Vy", 0x8ab0, Ld (DR 0xa) (DR 0xb))
          , ("8xy1", "OR Vx, Vy", 0x8ab1, Or (DR 0xa) (DR 0xb))
          , ("8xy2", "AND Vx, Vy", 0x8ab2, And (DR 0xa) (DR 0xb))
          , ("8xy3", "XOR Vx, Vy", 0x8ab3, Xor (DR 0xa) (DR 0xb))
          , ("8xy4", "ADD Vx, Vy", 0x8ab4, Add (DR 0xa) (DR 0xb))
          , ("8xy5", "SUB Vx, Vy", 0x8ab5, Sub (DR 0xa) (DR 0xb))
          , ("8xy6", "SHR Vx, Vy", 0x8ab6, Shr (DR 0xa) (DR 0xb))
          , ("8xy7", "SUBN Vx, Vy", 0x8ab7, Subn (DR 0xa) (DR 0xb))
          , ("8xye", "SHL Vx, Vy", 0x8abe, Shl (DR 0xa) (DR 0xb))
          , ("9xy0", "SNE Vx, Vy", 0x9bc0, Sne (DR 0xb) (DR 0xc))
          , ("annn", "LD I, nnn", 0xa123, Ld IR (Mem 0x123))
          , ("bnnn", "JP V0, nnn", 0xb123, Jp (DR 0) (Mem 0x123))
          , ("cxnn", "RND Vx, nn", 0xce33, Rnd (DR 0xe) (Val 0x33))
          , ("dxyn", "DRW Vx, Vy, n", 0xd001, Drw (DR 0x0) (DR 0x0) (Val 0x1))
          , ("ex9e", "SKP Vx", 0xea9e, Skp (DR 0xa))
          , ("exa1", "SKNP Vx", 0xeaa1, Sknp (DR 0xa))
          , ("fx07", "LD Vx, DT", 0xfa07, Ld (DR 0xa) DT)
          , ("fx0a", "LD Vx, K", 0xfa0a, Ld (DR 0xa) Key)
          , ("fx15", "LD DT, Vx", 0xfa15, Ld DT (DR 0xa))
          , ("fx18", "LD ST, Vx", 0xfa18, Ld ST (DR 0xa))
          , ("fx1e", "ADD I, Vx", 0xfa1e, Add IR (DR 0xa))
          , ("fx29", "LD F, Vx", 0xfa29, Ld Font (DR 0xa))
          , ("fx33", "LD B, Vx", 0xfa33, Ld Bcd (DR 0xa))
          , ("fx55", "LD [I], Vx", 0xfa55, Ld IR (DR 0xa))
          , ("fx65", "LD Vx, [I]", 0xfa65, Ld (DR 0xa) IR)
          ]

    forM_ cases $ \(sc, si, c, r) ->
      it (printf "decodes 0x%s to %s" sc si) $ decode c `shouldBe` Just r

    it "fails on unknown opcodes" $
      let cs = [0x8aba, 0xfa56, 0xfa77, 0xffff]
      in  map decode cs `shouldBe` map (const Nothing) cs

  describe "disassemble" $ do
    it "shows the assembly of one full opcode" $
      disassemble 0x200 [0x00, 0xe0] `shouldBe` unlines ["[0x200] 0x00e0 : CLS"]

    it "appends 0x00 to one dangling byte" $
      disassemble 0x200 [0x1] `shouldBe` unlines ["[0x200] 0x0100 : SYS 0x100"]

    it "shows the assembly of multiple opcodes" $
      disassemble 0x200 [ 0x6a, 0x11
                        , 0x8a, 0xb0
                        , 0xfa, 0x65
                        , 0x30, 0x01
                        , 0x50, 0x10
                        , 0xea, 0x9e
                        ] `shouldBe`
        unlines [ "[0x200] 0x6a11 : LD Va, 0x11"
                , "[0x202] 0x8ab0 : LD Va, Vb"
                , "[0x204] 0xfa65 : LD Va, [I]"
                , "[0x206] 0x3001 : SE V0, 0x01"
                , "[0x208] 0x5010 : SE V0, V1"
                , "[0x20a] 0xea9e : SKP Va"
                ]

    it "shows unknown opcodes" $
      disassemble 0x200 [0x8a, 0xba, 0xfa, 0x56, 0xfa] `shouldBe`
        unlines [ "[0x200] 0x8aba : -"
                , "[0x202] 0xfa56 : -"
                , "[0x204] 0xfa00 : -"
                ]

-- Local Variables:
-- dante-target: "chip8hs:test:spec"
-- End:
