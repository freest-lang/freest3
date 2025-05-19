module Chacha20 where

import SecureUtils
import List
import Random

data Nonce = Nonce Integer                  -- 96-bit
data NonceHalf = NonceHalf Integer          -- 95-bit
data ChachaState = ChachaState (Nonce, Int) -- Nonce + Counter
data Stream = Stream Integer                -- 512-bit
data Block = Block Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int -- 4x4 matrix of 32-bit values, This should be a mutable array.
type Newchacha20Exchange = !NonceHalf ; ?NonceHalf

_getNonce : Nonce -> Integer
_getNonce (Nonce nonceValue) = nonceValue

_getStream : Stream -> Integer
_getStream (Stream streamValue) = streamValue

_newNonceHalf : () -> NonceHalf
_newNonceHalf u = 
    let rng = newRNGState u in
    let (nonce, rng) = nextN64Bits 2 rng in
    NonceHalf $ modI nonce (2i ^i 95i)

_newNonce : NonceHalf -> NonceHalf -> Nonce
_newNonce (NonceHalf half1) (NonceHalf half2) = Nonce $ half1 +i half2

-- Round assisting functions

_clampTo32 : Int -> Int
_clampTo32 x = mod x (2 ^ 32)

_bitRotation32 : Int -> Int -> Int
_bitRotation32 value shift = lor (shiftL value shift) (shiftR value (32 - shift))

-- Chacha Rounds

_quarterRound : Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
_quarterRound a b c d =
    let a = _clampTo32 (a + b) in let d = lxor a d in let d = _clampTo32 (_bitRotation32 d 16) in
    let c = _clampTo32 (c + d) in let b = lxor b c in let b = _clampTo32 (_bitRotation32 b 12) in
    let a = _clampTo32 (a + b) in let d = lxor a d in let d = _clampTo32 (_bitRotation32 d 8) in
    let c = _clampTo32 (c + d) in let b = lxor b c in let b = _clampTo32 (_bitRotation32 b 7) in
    (a, b, c, d)

_columnRound : Block -> Block
_columnRound (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15) =
    -- Column 1: 0, 4, 8, 12
    let abcd = _quarterRound c0 c4 c8 c12 in
    let (c0, bcd) = abcd in let (c4, cd) = bcd in let (c8, c12) = cd in
    -- Column 2: 1, 5, 9, 13
    let abcd = _quarterRound c1 c5 c9 c13 in
    let (c1, bcd) = abcd in let (c5, cd) = bcd in let (c9, c13) = cd in
    -- Column 3: 2, 6, 10, 14
    let abcd = _quarterRound c2 c6 c10 c14 in
    let (c2, bcd) = abcd in let (c6, cd) = bcd in let (c10, c14) = cd in
    -- Column 4: 3, 7, 11, 15
    let abcd = _quarterRound c3 c7 c11 c15 in
    let (c3, bcd) = abcd in let (c7, cd) = bcd in let (c11, c15) = cd in
    -- Rebuild and return list
    (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15)

_diagonalRound : Block -> Block
_diagonalRound (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15) =
    -- Main diagonal: 0, 5, 10, 15
    let abcd = _quarterRound c0 c5 c10 c15 in
    let (c0, bcd) = abcd in let (c5, cd) = bcd in let (c10, c15) = cd in
    -- Diagonal 2: 1, 6, 11, 12
    let abcd = _quarterRound c1 c6 c11 c12 in
    let (c1, bcd) = abcd in let (c6, cd) = bcd in let (c11, c12) = cd in
    -- Diagonal 3: 2, 7, 8, 13
    let abcd = _quarterRound c2 c7 c8 c13 in
    let (c2, bcd) = abcd in let (c7, cd) = bcd in let (c8, c13) = cd in
    -- Diagonal 4: 3, 4, 9, 14
    let abcd = _quarterRound c3 c4 c9 c14 in
    let (c3, bcd) = abcd in let (c4, cd) = bcd in let (c9, c14) = cd in
    -- Rebuild and return list
    (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15)

_chachaN : Block -> Int -> Block
_chachaN block n =
    if n <= 0 then
        block
    else
        let block = _columnRound block in
        let block = _diagonalRound block in
        _chachaN block (n - 2)

-- ChaCha Block and Stream building assinting function

_splitTo32Bit : Integer -> Int -> [Int]
_splitTo32Bit value i =
    if i == 0 then
        []
    else
        let part = integerToInt $ modI value (2i ^i 32i) in
        (_splitTo32Bit (shiftRI value 32) (i-1)) ++ [part]

_popHead : [Int] -> (Int, [Int])
_popHead list = (head list, tail list)

_listToBlock : [Int] -> Block
_listToBlock list =
    let (c0, list) =  _popHead list in let (c1, list)  = _popHead list in let (c2, list)  = _popHead list in let (c3, list)  = _popHead list in
    let (c4, list) =  _popHead list in let (c5, list)  = _popHead list in let (c6, list)  = _popHead list in let (c7, list)  = _popHead list in
    let (c8, list) =  _popHead list in let (c9, list)  = _popHead list in let (c10, list) = _popHead list in let (c11, list) = _popHead list in
    let (c12, list) = _popHead list in let (c13, list) = _popHead list in let (c14, list) = _popHead list in let (c15, list) = _popHead list in
    (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15)

_blockToList : Block -> [Int]
_blockToList (Block c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15) = c0::c1::c2::c3::c4::c5::c6::c7::c8::c9::c10::c11::c12::c13::c14::[c15]

-- ChaCha Block and Stream building

_chacha20 : Key -> ChachaState -> (Stream, ChachaState)
_chacha20 (AsymmetricKey _ _) _ =
    error @(Stream, ChachaState) "Chacha20 does not use asymmetric keys."
_chacha20 (SessionKey keyValue) (ChachaState nonceCounter) =
    let (nonce, counter) = nonceCounter in
    let nonceValue = _getNonce nonce in

    -- Building inital block
    let list = [1634760805, 857760878, 2036477234, 1797285236] in   -- "expand 32-byte k" -> ["expa", "nd 3", "2-by", "te k"]
    let list = list ++ _splitTo32Bit keyValue 8 in                  -- Key in row 2 and 3
    let list = list ++ [counter] in                                 -- Counter first column last row
    let list = list ++ _splitTo32Bit nonceValue 3 in                -- Nonce last 3 values of last row
    let block = _listToBlock list in

    -- ChaCha Rounds (10 colum + 10 diagonal, 20 total)
    let block = _chachaN block 20 in

    -- Read final matrix from right to left, so the resulting right most bits are the first values of the matrix (as is supposed)
    let stream = foldr @Integer (\x : Int y : Integer -> (shiftLI y 32) +i (intToInteger x)) 0i (_blockToList block) in

    (Stream stream, ChachaState (Nonce nonceValue, counter + 1))

--Gives stream of size*512 bits
_multipleChacha20 : Key -> ChachaState -> Int -> (Stream, ChachaState)
_multipleChacha20 key chachaState size =
    if size <= 1 then
        _chacha20 key chachaState
    else
        let (streamL, chachaState) = _multipleChacha20 key chachaState (size - 1) in
        let streamLValue = _getStream streamL in
        let (streamR, chachaState) = _chacha20 key chachaState in
        let streamRValue = _getStream streamR in
        (Stream (lorI (shiftLI streamLValue 512) streamRValue), chachaState)


--Finds value's bit size in multiples of 512
_calculateSize : Integer -> Int
_calculateSize value =
    if value ==i 0i then
        0
    else
        (_calculateSize (shiftRI value 512)) + 1

encryptdecryptWithchacha20 : ChachaState -> EncryptionAlgorithm
encryptdecryptWithchacha20 chachaState msg key =
    --Calculate message size
    let size = _calculateSize msg in
    --Obtain stream
    let (stream, chachaState) = _multipleChacha20 key chachaState size in
    --Encrypt and send message
    let msg = lxorI msg (_getStream stream) in
    (msg, encryptdecryptWithchacha20 chachaState)

newchacha20A: forall a . Newchacha20Exchange ; a -> (EncryptionAlgorithm, a)
newchacha20A c =
    let half1 = _newNonceHalf () in
    let c = send half1 c in
    let (half2, c) = receive c in
    (encryptdecryptWithchacha20 $ ChachaState $ (_newNonce half1 half2, 0), c)

newchacha20B: forall a . dualof Newchacha20Exchange ; a -> (EncryptionAlgorithm, a)
newchacha20B c =
    let (half1, c) = receive c in
    let half2 = _newNonceHalf () in
    let c = send half2 c in
    (encryptdecryptWithchacha20 $ ChachaState $ (_newNonce half1 half2, 0), c)