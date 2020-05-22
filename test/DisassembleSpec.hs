module DisassembleSpec (spec) where

import           Disassemble
import           Test.Hspec  (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "decode" $
    it "decode an opcode into an operation" $ do
      decode 0x00e0 `shouldBe` Just Cls
      decode 0x00ee `shouldBe` Just Ret
      decode 0x0123 `shouldBe` Just (Sys (Mem 0x123))
      decode 0x1234 `shouldBe` Just (Jp (Val 0x0) (Mem 0x234))
      decode 0x2345 `shouldBe` Just (Call (Mem 0x345))
      decode 0x3011 `shouldBe` Just (Se (DR 0x0) (Val 0x11))
      decode 0x4011 `shouldBe` Just (Sne (DR 0x0) (Val 0x11))
      decode 0x5010 `shouldBe` Just (Se (DR 0x0) (DR 0x1))
      decode 0x6a11 `shouldBe` Just (Ld (DR 0xa) (Val 0x11))
      decode 0x7a11 `shouldBe` Just (Add (DR 0xa) (Val 0x11))
      decode 0x8ab0 `shouldBe` Just (Ld (DR 0xa) (DR 0xb))
      decode 0x8ab1 `shouldBe` Just (Or (DR 0xa) (DR 0xb))
      decode 0x8ab2 `shouldBe` Just (And (DR 0xa) (DR 0xb))
      decode 0x8ab3 `shouldBe` Just (Xor (DR 0xa) (DR 0xb))
      decode 0x8ab4 `shouldBe` Just (Add (DR 0xa) (DR 0xb))
      decode 0x8ab5 `shouldBe` Just (Sub (DR 0xa) (DR 0xb))
      decode 0x8ab6 `shouldBe` Just (Shr (DR 0xa) (DR 0xb))
      decode 0x8ab7 `shouldBe` Just (Subn (DR 0xa) (DR 0xb))
      decode 0x8abe `shouldBe` Just (Shl (DR 0xa) (DR 0xb))
      decode 0x9bc0 `shouldBe` Just (Sne (DR 0xb) (DR 0xc))
      decode 0xa123 `shouldBe` Just (Ld IR (Mem 0x123))
      decode 0xb123 `shouldBe` Just (Jp (DR 0) (Mem 0x123))
      decode 0xce33 `shouldBe` Just (Rnd (DR 0xe) (Val 0x33))
      decode 0xd001 `shouldBe` Just (Drw (DR 0x0) (DR 0x0) (Val 0x1))
      decode 0xea9e `shouldBe` Just (Skp (DR 0xa))
      decode 0xeaa1 `shouldBe` Just (Sknp (DR 0xa))
      decode 0xfa07 `shouldBe` Just (Ld (DR 0xa) DT)
      decode 0xfa0a `shouldBe` Just (Ld (DR 0xa) Key)
      decode 0xfa15 `shouldBe` Just (Ld DT (DR 0xa))
      decode 0xfa18 `shouldBe` Just (Ld ST (DR 0xa))
      decode 0xfa1e `shouldBe` Just (Add IR (DR 0xa))
      decode 0xfa29 `shouldBe` Just (Ld Font (DR 0xa))
      decode 0xfa33 `shouldBe` Just (Ld Bcd (DR 0xa))
      decode 0xfa55 `shouldBe` Just (Ld IR (DR 0xa))
      decode 0xfa65 `shouldBe` Just (Ld (DR 0xa) IR)
      decode 0x8aba `shouldBe` Nothing
      decode 0xffff `shouldBe` Nothing

  describe "disassemble" $
    it "show the assembly of byte codes" $ do
      disassemble [0x00, 0xe0, 0x00, 0xee, 0x01, 0x11] `shouldBe`
        unlines [ "0x00e0 : CLS"
                , "0x00ee : RET"
                , "0x0111 : SYS 0x111"
                ]
      disassemble [0x6a, 0x11, 0x8a, 0xb0, 0xfa, 0x65] `shouldBe`
        unlines [ "0x6a11 : LD Va, 0x11"
                , "0x8ab0 : LD Va, Vb"
                , "0xfa65 : LD Va, [I]"
                ]
      disassemble [0x30, 0x01, 0x50, 0x10, 0xea, 0x9e] `shouldBe`
        unlines [ "0x3001 : SE V0, 0x01"
                , "0x5010 : SE V0, V1"
                , "0xea9e : SKP Va"
                ]
      disassemble [0x8a, 0xba, 0xfa, 0x56, 0xfa, 0x34, 0x1] `shouldBe`
        unlines [ "0x8aba : -"
                , "0xfa56 : -"
                , "0xfa34 : -"
                , "0x0100 : SYS 0x100"
                ]

-- Local Variables:
-- dante-target: "chip8hs:test:spec"
-- End:
