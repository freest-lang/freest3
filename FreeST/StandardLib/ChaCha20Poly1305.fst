module ChaCha20Poly1305 where

import SecureUtils
import List
import Random

data ChaChaPolyState = ChaChaPolyState RNGState
data ChachaState = ChachaState (Nonce, Int) -- Nonce + Counter
data Nonce = Nonce Integer                  -- 96-bit
data KeyStream = KeyStream Integer          -- 512-bit
data Block = Block Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int -- 4x4 matrix of 32-bit values, This should be a mutable array.
type NewChaChaPolyExchange = !Integer       -- Sharing rng seed
data CryptMode = Encrypt | Decrypt          -- Work around due to not being able to call functions that were not declared yet

_getNonce : Nonce -> Integer
_getNonce (Nonce nonceValue) = nonceValue

_getKeyStream : KeyStream -> Integer
_getKeyStream (KeyStream keyStreamValue) = keyStreamValue

_newNonce : RNGState -> (Nonce, RNGState)
_newNonce rng =
    let (nonce, rng) = nextN64Bits 2 rng in
    (Nonce $ modI nonce (2i ^i 96i), rng)

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

-- ChaCha Block and KeyStream building assinting function

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

-- ChaCha Block and KeyStream building

_keyStreamStep : Key -> ChachaState -> (KeyStream, ChachaState)
_keyStreamStep (AsymmetricKey _ _) _ =
    error @(KeyStream, ChachaState) "ChaCha20 does not use asymmetric keys."
_keyStreamStep (SessionKey keyValue) (ChachaState nonceCounter) =
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
    let keyStream = foldr @Integer (\x : Int y : Integer -> (shiftLI y 32) +i (intToInteger x)) 0i (_blockToList block) in

    (KeyStream keyStream, ChachaState (Nonce nonceValue, counter + 1))

--Gives keyStream of size*512 bits
_generateKeyStream : Key -> ChachaState -> Int -> (KeyStream, ChachaState)
_generateKeyStream key chachaState size =
    if size <= 1 then
        _keyStreamStep key chachaState
    else
        let (keyStreamL, chachaState) = _generateKeyStream key chachaState (size - 1) in
        let keyStreamLValue = _getKeyStream keyStreamL in
        let (keyStreamR, chachaState) = _keyStreamStep key chachaState in
        let keyStreamRValue = _getKeyStream keyStreamR in
        (KeyStream (lorI (shiftLI keyStreamLValue 512) keyStreamRValue), chachaState)

_ChaCha20 : Integer -> Key -> Nonce -> Int -> Integer
_ChaCha20 value key nonce size =
    --Obtain keyStream
    let (keyStream, _) = _generateKeyStream key (ChachaState (nonce, 0)) size in
    --Encrypt/Decrypt and return value
    let newValue = lxorI value (_getKeyStream keyStream) in
    newValue

_generatePoly1305Key : Key -> Nonce -> (Integer, Integer)
_generatePoly1305Key key nonce =
    --Generate a 256-bit key from the ChaCha20 key and nonce
    let keyStream = _ChaCha20 0i key nonce 1 in
    --Get the first 128 bits
    let r = modI keyStream (2i ^i 128i) in
    --Get the second 128 bits
    let s = modI (shiftRI keyStream 128) (2i ^i 128i) in
    --Mask that clamps r, sets to 0 the 4 hightest bits from bytes 3,7,11,15 and the 2 lowest bits from bytes 4,8,12
    let mask = 340282347905251000583034798271797133071i in
    --Clamp r
    let r = landI r mask in
    (r, s)

_accumulatePoly1305 : Integer -> Integer -> Integer -> Integer -> Integer
_accumulatePoly1305 r p ciphertext acc =
    if ciphertext ==i 0i then
        acc
    else
        --Get the lowest 16 bytes (128 bits)
        let block = modI ciphertext (2i ^i 128i) in
        --Shift ciphertext to the right by 128 bits
        let ciphertext = shiftRI ciphertext 128 in
        --Add 1 bit to block
        let block = block +i (2i ^i 128i) in
        --Add to accumulator and multiply by r
        let acc = modI ((acc +i block) *i r) p in
        _accumulatePoly1305 r p ciphertext acc

_generatePoly1305Tag : Integer -> Key -> Nonce -> Integer
_generatePoly1305Tag ciphertext key nonce =
    --Get the Poly1305 key
    let (r, s) = _generatePoly1305Key key nonce in
    --Calculate modulu P
    let p = (2i ^i 130i) -i 5i in
    --Accumulate the ciphertext
    let acc = _accumulatePoly1305 r p ciphertext 0i in
    --Add s to accumulator
    let acc = acc +i s in
    --Return 16 byte (128 bit) tag
    modI acc (2i ^i 128i)
    
--Finds value's bit size in multiples of 512 (32*16)
_calculateSize : Integer -> Int
_calculateSize value =
    if value ==i 0i then
        0
    else
        (_calculateSize (shiftRI value 512)) + 1

--Separates ciphertext and tag
_separateChipherTag : Integer -> Int -> (Integer, Integer)
_separateChipherTag value size =
    let tag = shiftRI value (512 * size) in
    let ciphertext = modI value (2i ^i (intToInteger(512 * size))) in
    (ciphertext, tag)

chaCha20Poly1305 : CryptMode -> ChaChaPolyState -> NextCrypt

chaCha20Poly1305 Encrypt (ChaChaPolyState rng) msg key =
    --Calculate msg size
    let size = _calculateSize msg in
    --Obtain nonce and new RNG state
    let (nonce, rng) = _newNonce rng in
    --Encrypt
    let ciphertext = _ChaCha20 msg key nonce size in
    --Genrate Poly1305 tag
    let tag = _generatePoly1305Tag ciphertext key nonce in
    --Attach tag to ciphertext
    let ciphertextAndMac = lorI ciphertext (shiftLI tag (512 * size)) in
    --Return ciphertext+mac and next iterations
    let chaChaPolyState = ChaChaPolyState rng in
    (ciphertextAndMac, chaCha20Poly1305 Encrypt chaChaPolyState, chaCha20Poly1305 Decrypt chaChaPolyState)

chaCha20Poly1305 Decrypt (ChaChaPolyState rng) value key =
    --Calculate ciphertext size
    let size = (_calculateSize value) - 1 in
    --Separetate tag from ciphertext
    let (ciphertext, tag) = _separateChipherTag value size in
    --Obtain nonce and new RNG state
    let (nonce, rng) = _newNonce rng in
    --Check tag
    if tag ==i _generatePoly1305Tag ciphertext key nonce then
        --Decrypt
        let msg = _ChaCha20 ciphertext key nonce size in
        --Return decrypted message and next iterations
        let chaChaPolyState = ChaChaPolyState rng in
        (msg, chaCha20Poly1305 Encrypt chaChaPolyState, chaCha20Poly1305 Decrypt chaChaPolyState)
    else
        error @(Integer, NextEncrypt, NextDecrypt) "ChaCha20Poly1305: Invalid tag."

newChaChaPolyA: forall a . NewChaChaPolyExchange ; a -> ((NextEncrypt, NextDecrypt), a)
newChaChaPolyA c =
    let rng = newRNGState () in
    let seed = getRngSeed rng in
    let c = send seed c in
    let chaChaPolyState = ChaChaPolyState rng in
    ((chaCha20Poly1305 Encrypt chaChaPolyState, chaCha20Poly1305 Decrypt chaChaPolyState), c)

newChaChaPolyB: forall a . dualof NewChaChaPolyExchange ; a -> ((NextEncrypt, NextDecrypt), a)
newChaChaPolyB c =
    let (seed, c) = receive c in
    let rng = newRNGStateSetSeed seed in
    let chaChaPolyState = ChaChaPolyState rng in
    ((chaCha20Poly1305 Encrypt chaChaPolyState, chaCha20Poly1305 Decrypt chaChaPolyState), c)
