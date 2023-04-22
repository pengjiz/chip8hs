{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Machine
  ( Machine
  , FrameBuffer
  , ErrorKind (..)
  , Error (..)
  , initMachine
  , loadRom
  , step
  , decayTimers
  , pressKey
  , decayKeypad
  , clearKeypad
  , pc
  , ir
  , drs
  , sp
  , stack
  , mem
  , st
  , dt
  , keypad
  , fb
  , rng
  , paused
  , dirty
  , pixels
  , screenWidth
  , screenHeight
  , keyPress
  ) where

import           Control.Monad       ((<=<))
import           Data.Bits           (shiftL, shiftR, testBit, xor, (.&.),
                                      (.|.))
import           Data.Function       (on)
import           Data.List           (intercalate)
import qualified Data.Vector         as V
import           Data.Word           (Word16, Word8)
import           Disassemble         (Operand (..), Operation (..), decode)
import           Lens.Micro.Platform (each, ix, makeLenses, over, preview, set,
                                      toListOf, traverseOf, view)
import qualified System.Random       as R
import           Text.Printf         (printf)
import           Util                (bcdOf, mergeOpcode, splitSprite)

-- | Width of the screen.
screenWidth :: Int
screenWidth = 64

-- | Height of the screen.
screenHeight :: Int
screenHeight = 32

-- | Initial value of a pressed key.
keyPress :: Int
keyPress = 20

-- | Model for frame buffer.
data FrameBuffer = FrameBuffer
  { -- | Pixels of frame buffer.
    _pixels :: V.Vector Bool
    -- | Whether frame buffer has been drawn on the screen.
  , _dirty  :: Bool
  } deriving (Eq)

instance Show FrameBuffer where
  show fb = printf "%s, [%s]" d ps
    where d = if _dirty fb then "dirty" else "clean"
          ps = map (\x -> if x then '*' else '-') (V.toList . _pixels $ fb)

makeLenses ''FrameBuffer

-- | An empty frame buffer.
emptyFB :: FrameBuffer
emptyFB = FrameBuffer
  { _pixels = V.replicate (screenWidth * screenHeight) False
  , _dirty = True
  }

-- | Model for CHIP-8 machine.
data Machine = Machine
  { -- | Program counter.
    _pc     :: Word16
    -- | Address register I.
  , _ir     :: Word16
    -- | Data registers.
  , _drs    :: V.Vector Word8
    -- | Stack pointer.
  , _sp     :: Word8
    -- | Stack.
  , _stack  :: V.Vector Word16
    -- | Main memory.
  , _mem    :: V.Vector Word8
    -- | Sound timer.
  , _st     :: Word8
    -- | Delay timer.
  , _dt     :: Word8
    -- | Keypad.
  , _keypad :: V.Vector Int
    -- | Frame buffer.
  , _fb     :: FrameBuffer
    -- | Random number generator.
  , _rng    :: R.StdGen
    -- | Whether to stay on the current instruction.
  , _paused :: Bool
  }

instance Eq Machine where
  (==) a b = f _pc
             && f _ir
             && f _drs
             && f _sp
             && f _stack
             && f _mem
             && f _st
             && f _dt
             && f _keypad
             && f _fb
             && f (show . _rng)
             && f _paused
    where f g = on (==) g a b

instance Show Machine where
  show m = unlines
    [ "Machine"
    , " PC    : " ++ showAddr (_pc m)
    , " I     : " ++ showAddr (_ir m)
    , " Vx    : " ++ showVec showVal (_drs m)
    , " SP    : " ++ showVal (_sp m)
    , " Stack : " ++ showVec showAddr (_stack m)
    , " Mem   : " ++ showMem (_mem m)
    , " ST    : " ++ show (_st m)
    , " DT    : " ++ show (_dt m)
    , " Keypad: " ++ showVec (\x -> if x > 0 then "D" else "U") (_keypad m)
    , " FB    : " ++ show (_fb m)
    , " RNG   : " ++ show (_rng m)
    , " State : " ++ if _paused m then "Paused" else "Running"
    ]
    where showAddr = printf "0x%03x"
          showVal = printf "0x%02x"
          showVec f = printf "[%s]" . intercalate ", " . map f . V.toList
          showMem = showVec (\(n, x) -> printf "(0x%03x,%s)" n (showVal x))
                    . V.filter ((/= 0) . snd) . V.imap (,)

makeLenses ''Machine

-- | CHIP-8 font set.
fontSet :: V.Vector Word8
fontSet = V.fromList
  [ 0xf0, 0x90, 0x90, 0x90, 0xf0 -- 0
  , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
  , 0xf0, 0x10, 0xf0, 0x80, 0xf0 -- 2
  , 0xf0, 0x10, 0xf0, 0x10, 0xf0 -- 3
  , 0x90, 0x90, 0xf0, 0x10, 0x10 -- 4
  , 0xf0, 0x80, 0xf0, 0x10, 0xf0 -- 5
  , 0xf0, 0x80, 0xf0, 0x90, 0xf0 -- 6
  , 0xf0, 0x10, 0x20, 0x40, 0x40 -- 7
  , 0xf0, 0x90, 0xf0, 0x90, 0xf0 -- 8
  , 0xf0, 0x90, 0xf0, 0x10, 0xf0 -- 9
  , 0xf0, 0x90, 0xf0, 0x90, 0x90 -- A
  , 0xe0, 0x90, 0xe0, 0x90, 0xe0 -- B
  , 0xf0, 0x80, 0x80, 0x80, 0xf0 -- C
  , 0xe0, 0x90, 0x90, 0x90, 0xe0 -- D
  , 0xf0, 0x80, 0xf0, 0x80, 0xf0 -- E
  , 0xf0, 0x80, 0xf0, 0x80, 0x80 -- F
  ]

-- | Initialize a new machine.
initMachine :: R.StdGen -> Machine
initMachine r = Machine
  { _pc = 0
  , _ir = 0
  , _drs = V.replicate 16 0
  , _sp = 0
  , _stack = V.replicate 12 0
  , _mem = fontSet V.++ V.replicate (4096 - V.length fontSet) 0
  , _st = 0
  , _dt = 0
  , _keypad = V.replicate 16 0
  , _fb = emptyFB
  , _rng = r
  , _paused = False
  }

-- | Type of error.
data ErrorKind =
  -- | Invalid opcode.
  UnknownOpcode
  -- | Stack full when trying to push data.
  | StackOverflow
  -- | Stack pointer out of range.
  | InvalidSP
  -- | Memory address out of range.
  | InvalidAddress
  -- | Data register number out of range.
  | InvalidDR
  -- | Key code out of range.
  | InvalidKey
  -- | No font for the character.
  | NoFont
  -- | Other general error.
  | Other
  deriving (Eq)

instance Show ErrorKind where
  show UnknownOpcode  = "Unknown opcode"
  show StackOverflow  = "Stack overflow"
  show InvalidSP      = "Invalid stack pointer"
  show InvalidAddress = "Invalid memory address"
  show InvalidDR      = "Invalid data register number"
  show InvalidKey     = "Invalid key code"
  show NoFont         = "No font available for the character"
  show Other          = "General error"

-- | Machine error.
data Error = Error
  { -- | Type of error.
    kind    :: ErrorKind
    -- | Machine state when error occurs.
  , machine :: Machine
  } deriving (Eq)

instance Show Error where
  show e = printf "error: %s (opcode %s)" k c
    where k = show . kind $ e
          c = either (const "-") (printf "0x%04x") (fetchOpcode . machine $ e)

-- | Throw a machine error.
throwError :: ErrorKind -> Machine -> Either Error a
throwError k m = Left $ Error { kind = k, machine = m }

-- | CHIP-8 instruction.
type Instruction = Machine -> Either Error Machine

-- | Get the instruction of the given opcode.
getInstruction :: Word16 -> Maybe Instruction
getInstruction c = decode c >>= \case
  Cls                       -> move clearFB
  Ret                       -> move retSubr
  Jp (Val _) (Mem m)        -> stay $ jump m
  Jp (DR 0) (Mem m)         -> stay $ jumpFrom m
  Call (Mem m)              -> stay $ callSubr m
  Se (DR x) (Val n)         -> move $ skipIf (testVal (==) x n)
  Se (DR x) (DR y)          -> move $ skipIf (testDR (==) x y)
  Sne (DR x) (Val n)        -> move $ skipIf (testVal (/=) x n)
  Sne (DR x) (DR y)         -> move $ skipIf (testDR (/=) x y)
  Skp (DR x)                -> move $ skipIf (isKeyPressed x)
  Sknp (DR x)               -> move $ skipIf (fmap not . isKeyPressed x)
  Ld (DR x) (Val n)         -> move $ writeDR x n
  Ld (DR x) (DR y)          -> move $ storeDR x y
  Ld (DR x) DT              -> move $ storeDT x
  Ld IR (Mem m)             -> move $ storeAddr m
  Ld DT (DR x)              -> move $ writeTimer (flip $ set dt) x
  Ld ST (DR x)              -> move $ writeTimer (flip $ set st) x
  Ld (DR x) Key             -> move $ storeKey x
  Ld Font (DR x)            -> move $ storeFont x
  Ld Bcd (DR x)             -> move $ storeBcd x
  Ld IR (DR x)              -> move $ dumpDR x
  Ld (DR x) IR              -> move $ loadDR x
  Add (DR x) (Val n)        -> move $ addVal x n
  Add (DR x) (DR y)         -> move $ addDR x y
  Add IR (DR x)             -> move $ addMem x
  Or (DR x) (DR y)          -> move $ orDR x y
  And (DR x) (DR y)         -> move $ andDR x y
  Xor (DR x) (DR y)         -> move $ xorDR x y
  Sub (DR x) (DR y)         -> move $ subDR x y
  Subn (DR x) (DR y)        -> move $ subnDR x y
  Shr (DR x) (DR y)         -> move $ shrDR x y
  Shl (DR x) (DR y)         -> move $ shlDR x y
  Rnd (DR x) (Val n)        -> move $ rand x n
  Drw (DR x) (DR y) (Val n) -> move $ draw x y n
  _                         -> Nothing
  where move = Just . (nextPC <=<)
        stay = Just

-- | Fetch the current opcode for the machine.
fetchOpcode :: Machine -> Either Error Word16
fetchOpcode m = mergeOpcode <$> readMem (view pc m) 2 m

-- | Load ROM into memory and jump to the start of ROM.
loadRom :: Word16 -> [Word8] -> Machine -> Either Error Machine
loadRom addr = (jump addr <=<) . writeMem addr

-- | Execute one step for the machine.
step :: Machine -> Either Error Machine
step m = do c <- fetchOpcode m
            f <- maybe err pure (getInstruction c)
            f m
  where err = throwError UnknownOpcode m

-- | Decrement delay and sound timers by 1 if positive.
decayTimers :: Machine -> Machine
decayTimers = over dt g . over st g
  where g x = if x > 0 then x - 1 else 0

-- | Set a key to pressed.
pressKey :: Word8 -> Machine -> Either Error Machine
pressKey k m = if nk >= V.length (view keypad m)
               then throwError InvalidKey m
               else pure $ set (keypad . ix nk) keyPress m
  where nk = fromIntegral k

-- | Decrement all positive key values by 1.
decayKeypad :: Machine -> Machine
decayKeypad = over (keypad . each) g
  where g x = if x > 0 then x - 1 else 0

-- | Set all keys to unpressed.
clearKeypad :: Machine -> Machine
clearKeypad = set (keypad . each) 0

-- | Read value from a data register.
readDR :: Word8 -> Machine -> Either Error Word8
readDR x m = maybe err pure vx
  where vx = preview (drs . ix (fromIntegral x)) m
        err = throwError InvalidDR m

-- | Write value to a data register.
writeDR :: Word8 -> Word8 -> Instruction
writeDR x n m = if nx >= 0 && nx < size
                then pure $ set (drs . ix (fromIntegral x)) n m
                else throwError InvalidDR m
  where nx = fromIntegral x
        size = V.length (view drs m)

-- | Read the given number of bytes starting at a given address from memory.
readMem :: Word16 -> Int -> Machine -> Either Error [Word8]
readMem addr n m = maybe err pure ws
  where as = take n . iterate (1+) $ fromIntegral addr
        ws = traverseOf each (\a -> preview (mem . ix a) m) as
        err = throwError InvalidAddress m

-- | Write bytes to memory starting at the given address.
writeMem :: Word16 -> [Word8] -> Instruction
writeMem addr bs m = if a + n - 1 >= size || a < 0
                     then throwError InvalidAddress m
                     else pure $ over mem g m
  where n = length bs
        a = fromIntegral addr
        g v = V.update_ v (V.enumFromN a n) (V.fromList bs)
        size = V.length (view mem m)

-- | Push value to stack.
pushStack :: Word16 -> Instruction
pushStack v m = if p >= size
                then throwError StackOverflow m
                else pure . over sp (1+) . set (stack . ix p) v $ m
  where size = V.length $ view stack m
        p = fromIntegral $ view sp m

-- | Pop value from stack.
popStack :: Machine -> Either Error (Word16, Machine)
popStack m = maybe err r v
  where p = fromIntegral $ view sp m
        v = preview (stack . ix (p - 1)) m
        r x = pure (x, over sp (subtract 1) m)
        err = throwError InvalidSP m

-- | Move program counter to the next instruction.
nextPC :: Instruction
nextPC m = let m' = if view paused m
                    then m
                    else over pc (2+) m
           in pure m'

-- | Jump to a given address.
jump :: Word16 -> Instruction
jump addr = pure . set pc addr

-- | Jump to a address relative to V0.
jumpFrom :: Word16 -> Instruction
jumpFrom addr m = readDR 0 m >>= \x -> jump (fromIntegral x + addr) m

-- | Return from a subroutine.
retSubr :: Instruction
retSubr = uncurry jump <=< popStack

-- | Call a subroutine at the given address.
callSubr :: Word16 -> Instruction
callSubr addr m = pushStack (view pc m) m >>= jump addr

-- | Skip the next instruction if the predicate returns true.
skipIf :: (Machine -> Either Error Bool) -> Instruction
skipIf p m = p m >>= \r -> if r then nextPC m else pure m

-- | Test on a register and a value.
testVal :: (Word8 -> Word8 -> Bool)
        -> Word8 -> Word8
        -> Machine -> Either Error Bool
testVal p x v m = flip p v <$> readDR x m

-- | Test on two registers.
testDR :: (Word8 -> Word8 -> Bool)
       -> Word8 -> Word8
       -> Machine -> Either Error Bool
testDR p x y m = p <$> readDR x m <*> readDR y m

-- | Store memory address to address register.
storeAddr :: Word16 -> Instruction
storeAddr addr = pure . set ir addr

-- | Store delay timer.
storeDT :: Word8 -> Instruction
storeDT x m = writeDR x (view dt m) m

-- | Store value on a register to another.
storeDR :: Word8 -> Word8 -> Instruction
storeDR x y m = readDR y m >>= \n -> writeDR x n m

-- | Apply a binary operation on two data registers.
applyDR :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> Instruction
applyDR op x y m = op <$> readDR x m <*> readDR y m >>=
  \n -> writeDR x n m

-- | Apply a binary operation on two data registers and also set the flag.
applyDRWithF :: (Word8 -> Word8 -> Word8)
             -> (Word8 -> Word8 -> Bool)
             -> Word8 -> Word8 -> Instruction
applyDRWithF op f x y m = do
  vx <- readDR x m
  vy <- readDR y m
  m' <- applyDR op x y m
  return $ set (drs . ix 0xf) (if f vx vy then 1 else 0) m'

-- | Add value to a register.
addVal :: Word8 -> Word8 -> Instruction
addVal x v m = readDR x m >>= \n -> writeDR x (n + v) m

-- | Add value to address register.
addMem :: Word8 -> Instruction
addMem x m = readDR x m >>= \vx ->
  let c = fromIntegral (view ir m) + fromIntegral vx > (0xfff :: Int)
  in pure
     . set (drs . ix 0xf) (if c then 1 else 0)
     . over ir ((0xfff .&.) . (fromIntegral vx +)) $ m

-- | Add two data registers and set the carry flag.
addDR :: Word8 -> Word8 -> Instruction
addDR = applyDRWithF (+) f
  where f x y = fromIntegral x + fromIntegral y > (0xff :: Int)

-- | Bitwise or on two data registers.
orDR :: Word8 -> Word8 -> Instruction
orDR = applyDR (.|.)

-- | Bitwise and on two data registers.
andDR :: Word8 -> Word8 -> Instruction
andDR = applyDR (.&.)

-- | Bitwise xor on two data registers.
xorDR :: Word8 -> Word8 -> Instruction
xorDR = applyDR xor

-- | Subtract one data register from the other and set the borrow flag.
subDR :: Word8 -> Word8 -> Instruction
subDR = applyDRWithF (-) (>=)

-- | Reverse subtract on two registers.
subnDR :: Word8 -> Word8 -> Instruction
subnDR = applyDRWithF (flip (-)) (<)

-- | Right shift.
shrDR :: Word8 -> Word8 -> Instruction
shrDR = applyDRWithF op f
  where op x _ = shiftR x 1
        f x _ = testBit x 0

-- | Left shift.
shlDR :: Word8 -> Word8 -> Instruction
shlDR = applyDRWithF op f
  where op x _ = shiftL x 1
        f x _ = testBit x 7

-- | Sample a random number.
rand :: Word8 -> Word8 -> Instruction
rand x n m = let (r, g) = R.random $ view rng m
             in writeDR x (r .&. n) (set rng g m)

-- | Set delay timer or sound timer.
writeTimer :: (Machine -> Word8 -> Machine) -> Word8 -> Instruction
writeTimer t x m = t m <$> readDR x m

-- | Check if key in the given register is pressed.
isKeyPressed :: Word8 -> Machine -> Either Error Bool
isKeyPressed x m = readDR x m >>= \k ->
  let vk = (>0) <$> preview (keypad . ix (fromIntegral k)) m
      err = throwError InvalidKey m
  in maybe err pure vk

-- | Wait for a key press and store it.
storeKey :: Word8 -> Instruction
storeKey x m = case V.findIndex (>0) (view keypad m) of
  Just k  -> set paused False <$> writeDR x (fromIntegral k) m
  Nothing -> pure $ set paused True m

-- | Clear frame buffer.
clearFB :: Instruction
clearFB = pure . set fb emptyFB

-- | Write pixels to frame buffer and also set the flag.
writeFB :: V.Vector Bool -> Instruction
writeFB ps m = let c = V.any (\(x, y) -> not x && y)
                       . V.zip ps . view (fb . pixels) $ m
               in pure
                  . set (fb . dirty) True
                  . set (fb . pixels) ps
                  . set (drs . ix 0xf) (if c then 1 else 0) $ m

-- | Draw the given number of bytes to frame buffer.
draw :: Word8 -> Word8 -> Word8 -> Instruction
draw x y n m = do
  vx <- fromIntegral <$> readDR x m
  vy <- fromIntegral <$> readDR y m
  bs <- readMem (view ir m) (fromIntegral n) m
  let ps = drawSprite vx vy bs $ view (fb . pixels) m
  writeFB ps m

-- | Draw a pixel.
drawPixel :: (Int, Int, Bool) -> V.Vector Bool -> V.Vector Bool
drawPixel (x, y, c) = over (ix n) (xor c)
  where x' = x `mod` screenWidth
        y' = y `mod` screenHeight
        n = y' * screenWidth + x'

-- | Draw a sprite.
drawSprite :: Int -> Int -> [Word8] -> V.Vector Bool -> V.Vector Bool
drawSprite x y bs = flip (foldr drawPixel) ps
  where xs = cycle [x .. x + 7]
        ys = concatMap (replicate 8) [y .. y + length bs - 1]
        cs = splitSprite bs
        ps = zip3 xs ys cs

-- | Store the address of the sprite for a character.
storeFont :: Word8 -> Instruction
storeFont x m = readDR x m >>= \vx ->
  if vx > 0xf || vx < 0
  then throwError NoFont m
  else storeAddr (5 * fromIntegral vx) m

-- | Store the BCD representation of a register value.
storeBcd :: Word8 -> Instruction
storeBcd x m = readDR x m >>= \ds ->
  writeMem (view ir m) (toListOf each $ bcdOf ds) m

-- | Dump data registers to memory.
dumpDR :: Word8 -> Instruction
dumpDR x m = traverseOf each (`readDR` m) [0 .. x] >>=
  flip (writeMem (view ir m)) m

-- | Load data from memory to data registers.
loadDR :: Word8 -> Instruction
loadDR x m = if nx >= V.length (view drs m)
             then throwError InvalidDR m
             else g <$> readMem (view ir m) (nx + 1) m
  where g bs = over drs (\v -> V.update_ v nxs (V.fromList bs)) m
        nx = fromIntegral x
        nxs = V.enumFromN 0 (nx + 1)
