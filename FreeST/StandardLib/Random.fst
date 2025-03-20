module Random where

-- Squares RNG (By Bernard Widynski)

-- y = x = ctr * key; z = y + key;
-- x = x*x + y; x = (x>>32) | (x<<32); /* round 1 */
-- x = x*x + z; x = (x>>32) | (x<<32); /* round 2 */
-- x = x*x + y; x = (x>>32) | (x<<32); /* round 3 */
-- t = x = x*x + z; x = (x>>32) | (x<<32); /* round 4 */
-- return t ^ ((x*x + y) >> 32); /* round 5 */

-- Using Integer because theres no 64-bit unssigned int (u64Int), and clamping to 64-bit to imitate u64Int overflow behaviour
_clampTo64 : Integer -> Integer
_clampTo64 x = modI x (2i ^i 64i)

_squaresRNG : Int -> Int -> Int
_squaresRNG ctr key =
    let ctrI = intToInteger ctr in let keyI = intToInteger key in
    let x = _clampTo64 (ctrI *i keyI) in let y = x in let z = _clampTo64 (y +i keyI) in                           --x, y and z
    let x = _clampTo64 (_clampTo64 (x *i x) +i y) in let x = lorI (_clampTo64 (shiftLI x 32)) (shiftRI x 32) in  --round 1
    let x = _clampTo64 (_clampTo64 (x *i x) +i z) in let x = lorI (_clampTo64 (shiftLI x 32)) (shiftRI x 32) in  --round 2
    let x = _clampTo64 (_clampTo64 (x *i x) +i y) in let x = lorI (_clampTo64 (shiftLI x 32)) (shiftRI x 32) in  --round 3
    let t = _clampTo64 (_clampTo64 (x *i x) +i z) in let x = lorI (_clampTo64 (shiftLI x 32)) (shiftRI x 32) in  --round 4
    integerToInt $ lxorI t (_clampTo64 (shiftLI (_clampTo64 (_clampTo64 (x *i x) +i y)) 32))                --round 5


data RNGState = RNGState (Int, Int)

--Creates new rng state
newRNGState : () -> RNGState
newRNGState u = RNGState (1, getSystemTime u)

--Gets next Int
nextInt64 : RNGState -> (Int, RNGState)
nextInt64 (RNGState ctrKey) =
    let (ctr, key) = ctrKey in
    let n = _squaresRNG ctr key in
    (n, RNGState (n, key))

--Gets the next Int between min and max, inclusive, exclusive, respectively
nextInt :  Int -> Int -> RNGState -> (Int, RNGState)
nextInt min max rng =
    let (n, rng) = nextInt64 rng in
    (min + (mod n (max - min)), rng)

--Gets next 64 bits
next64Bits : RNGState -> (Integer, RNGState)
next64Bits rng =
    let (n, rng) = nextInt64 rng in
    let n = intToInteger n in
    if n <i 0i then
        (n +i (2i ^i 64i), rng)
    else
        (n, rng)

--Gets the next n*64 bits
nextN64Bits : Int -> RNGState -> (Integer, RNGState)
nextN64Bits n rng = 
    if n <= 1 then
        next64Bits rng
    else
        let (current, rng) = next64Bits rng in
        let current = shiftLI current (64 * (n - 1)) in
        let (next, rng) = nextN64Bits (n - 1) rng in
        (lorI current next, rng)
