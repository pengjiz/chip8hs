module Disassemble
  ( Operand (..)
  , Operation (..)
  , decode
  , disassemble
  ) where

import           Data.Bits   ((.&.))
import           Data.Word   (Word16, Word8)
import           Text.Printf (printf)
import           Util        (chunksOf, mergeOpcode, splitOpcode)

-- | CHIP-8 operand.
data Operand =
  -- | Literal value.
  Val Word8
  -- | Memory address.
  | Mem Word16
  -- | Data register.
  | DR Word8
  -- | Address register I.
  | IR
  -- | Delay timer.
  | DT
  -- | Sound timer.
  | ST
  -- | Key press.
  | Key
  -- | BCD representation.
  | Bcd
  -- | Font.
  | Font
  deriving (Eq)

instance Show Operand where
  show (Val x) = printf "0x%02x" x
  show (Mem x) = printf "0x%03x" x
  show (DR x)  = printf "V%x" x
  show IR      = "I"
  show DT      = "DT"
  show ST      = "ST"
  show Key     = "K"
  show Bcd     = "B"
  show Font    = "F"

-- | CHIP-8 operation.
data Operation =
  -- | Jump to a machine code routine (ignored).
  Sys Operand
  -- | Clear screen.
  | Cls
  -- | Call a subroutine.
  | Call Operand
  -- | Return from a subroutine.
  | Ret
  -- | Jump to a new location.
  | Jp Operand Operand
  -- | Skip next operation if equal.
  | Se Operand Operand
  -- | Skip next operation if not equal.
  | Sne Operand Operand
  -- | Set value.
  | Ld Operand Operand
  -- | Add values.
  | Add Operand Operand
  -- | Bit-wise or.
  | Or Operand Operand
  -- | Bit-wise and.
  | And Operand Operand
  -- | Bit-wise xor.
  | Xor Operand Operand
  -- | Subtract value.
  | Sub Operand Operand
  -- | Reverse subtract.
  | Subn Operand Operand
  -- | Left shift.
  | Shl Operand Operand
  -- | Right shift.
  | Shr Operand Operand
  -- | Generate a random number.
  | Rnd Operand Operand
  -- | Draw a sprite.
  | Drw Operand Operand Operand
  -- | Skip next operation if key is pressed.
  | Skp Operand
  -- | Skip next operation if key is not pressed.
  | Sknp Operand
  deriving (Eq)

instance Show Operation where
  show (Sys x)     = "SYS " ++ show x
  show Cls         = "CLS"
  show (Call x)    = "CALL " ++ show x
  show Ret         = "RET"
  show (Se x y)    = printf "SE %s, %s" (show x) (show y)
  show (Sne x y)   = printf "SNE %s, %s" (show x) (show y)
  show (Add x y)   = printf "ADD %s, %s" (show x) (show y)
  show (Or x y)    = printf "OR %s, %s" (show x) (show y)
  show (And x y)   = printf "AND %s, %s" (show x) (show y)
  show (Xor x y)   = printf "XOR %s, %s" (show x) (show y)
  show (Sub x y)   = printf "SUB %s, %s" (show x) (show y)
  show (Subn x y)  = printf "SUBN %s, %s" (show x) (show y)
  show (Shl x y)   = printf "SHL %s, %s" (show x) (show y)
  show (Shr x y)   = printf "SHR %s, %s" (show x) (show y)
  show (Rnd x y)   = printf "RND %s, %s" (show x) (show y)
  show (Drw x y z) = printf "DRW %s, %s, %s" (show x) (show y) (show z)
  show (Skp x)     = printf "SKP %s" (show x)
  show (Sknp x)    = printf "SKNP %s" (show x)
  show (Jp x y)    = case x of
                       (Val 0x0) -> "JP " ++ show y
                       _         -> printf "JP %s, %s" (show x) (show y)
  show (Ld x y)    = case (x, y) of
                       (IR, DR _) -> printf "LD [%s], %s" (show x) (show y)
                       (DR _, IR) -> printf "LD %s, [%s]" (show x) (show y)
                       _          -> printf "LD %s, %s" (show x) (show y)

-- | Decode opcode.
decode :: Word16 -> Maybe Operation
decode c = case splitOpcode c of
  (0x0, 0x0, 0xe, 0x0) -> Just Cls
  (0x0, 0x0, 0xe, 0xe) -> Just Ret
  (0x0, _, _, _)       -> Just $ Sys (Mem m)
  (0x1, _, _, _)       -> Just $ Jp (Val 0x0) (Mem m)
  (0x2, _, _, _)       -> Just $ Call (Mem m)
  (0x3, x, _, _)       -> Just $ Se (DR x) (Val n)
  (0x4, x, _, _)       -> Just $ Sne (DR x) (Val n)
  (0x5, x, y, 0x0)     -> Just $ Se (DR x) (DR y)
  (0x6, x, _, _)       -> Just $ Ld (DR x) (Val n)
  (0x7, x, _, _)       -> Just $ Add (DR x) (Val n)
  (0x8, x, y, 0x0)     -> Just $ Ld (DR x) (DR y)
  (0x8, x, y, 0x1)     -> Just $ Or (DR x) (DR y)
  (0x8, x, y, 0x2)     -> Just $ And (DR x) (DR y)
  (0x8, x, y, 0x3)     -> Just $ Xor (DR x) (DR y)
  (0x8, x, y, 0x4)     -> Just $ Add (DR x) (DR y)
  (0x8, x, y, 0x5)     -> Just $ Sub (DR x) (DR y)
  (0x8, x, y, 0x6)     -> Just $ Shr (DR x) (DR y)
  (0x8, x, y, 0x7)     -> Just $ Subn (DR x) (DR y)
  (0x8, x, y, 0xe)     -> Just $ Shl (DR x) (DR y)
  (0x9, x, y, 0x0)     -> Just $ Sne (DR x) (DR y)
  (0xa, _, _, _)       -> Just $ Ld IR (Mem m)
  (0xb, _, _, _)       -> Just $ Jp (DR 0) (Mem m)
  (0xc, x, _, _)       -> Just $ Rnd (DR x) (Val n)
  (0xd, x, y, s)       -> Just $ Drw (DR x) (DR y) (Val s)
  (0xe, x, 0x9, 0xe)   -> Just $ Skp (DR x)
  (0xe, x, 0xa, 0x1)   -> Just $ Sknp (DR x)
  (0xf, x, 0x0, 0x7)   -> Just $ Ld (DR x) DT
  (0xf, x, 0x0, 0xa)   -> Just $ Ld (DR x) Key
  (0xf, x, 0x1, 0x5)   -> Just $ Ld DT (DR x)
  (0xf, x, 0x1, 0x8)   -> Just $ Ld ST (DR x)
  (0xf, x, 0x1, 0xe)   -> Just $ Add IR (DR x)
  (0xf, x, 0x2, 0x9)   -> Just $ Ld Font (DR x)
  (0xf, x, 0x3, 0x3)   -> Just $ Ld Bcd (DR x)
  (0xf, x, 0x5, 0x5)   -> Just $ Ld IR (DR x)
  (0xf, x, 0x6, 0x5)   -> Just $ Ld (DR x) IR
  _                    -> Nothing
  where m = c .&. 0x0fff
        n = fromIntegral (c .&. 0x00ff) :: Word8

-- | Disassemble a sequence of bytes starting from a given address.
disassemble :: Word16 -> [Word8] -> String
disassemble addr = unlines
                   . zipWith showLine [addr, addr + 2 ..]
                   . map mergeOpcode . chunksOf 2
  where showLine a x = let d = maybe "-" show (decode x)
                       in printf "[0x%03x] 0x%04x : %s" a x d
