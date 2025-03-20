module Secure where

import Random
import List

----- Modular Exponentiation -----

_modExp : InfiniteInt -> InfiniteInt -> InfiniteInt -> InfiniteInt
_modExp b e m = 
    if e ==i 0i then 
        1i
    else if evenI e then
        _modExp (modI (b *i b) m) (e /i 2i) m
    else 
        modI (b *i _modExp b (e -i 1i) m) m




--  ██████  ██ ███████ ███████ ██ ███████       ██   ██ ███████ ██      ██      ███    ███  █████  ███    ██ 
--  ██   ██ ██ ██      ██      ██ ██            ██   ██ ██      ██      ██      ████  ████ ██   ██ ████   ██ 
--  ██   ██ ██ █████   █████   ██ █████   █████ ███████ █████   ██      ██      ██ ████ ██ ███████ ██ ██  ██ 
--  ██   ██ ██ ██      ██      ██ ██            ██   ██ ██      ██      ██      ██  ██  ██ ██   ██ ██  ██ ██ 
--  ██████  ██ ██      ██      ██ ███████       ██   ██ ███████ ███████ ███████ ██      ██ ██   ██ ██   ████ 


type DHProtocol = !InfiniteInt ; ?InfiniteInt

data Key = Key InfiniteInt  -- 256-bit

--8192-bit MODP group 18
_dhP : InfiniteInt
_dhP = 68171758476213495590643433084986531271759695309573263235881319185384385557674102439283279615552301110607010260567277060576543672042167353921249641611589827099759815373182646127543631281741314565457020738998902204967231262491310725795783783049784656429244984173561484961967249196407582242419973594946800739429994964183747405442971310357276197966718707884764158104700573267312586099453009222736430034928817707948384625935945930820319750986019930643497543625725884857580157971216801032457458333593944673574522525767607563684739648600967529279953296804207522649084724787612195175422263607098738741914645097619238328447106496924704452040775305936491843260905103268987718680085388156790453979273532133189443896052343062485984002920853239202462209596271775929553414982455918395215636680261863909033790451686705760043324045915933869183540411008141310762080019823231026185722122135090386984943357470309444794098585129592582456002575080219759641815274328607981268105257466267717528269137758126557117498820994251686491762873082385488730053187415605901056747044556560150308610986856182325459772201715941636488490479320970659359714988717520108307866816821009130332898895914296591024879475191128227138086669293956659118949975532739032108101571041195513818810483465657497025265512391425864443585891353148751079869810957758427910917581708563586301897105038951021059341956636980402390581605400836264440381028184599630585971267814830779433087194626769096715786560129644339884489089868936000805734334699917401287732627181269460305815405985087770871167883752365501932268283954605283352502039852443798406398221404956404606149293305776309154934233362524236421338611655298136881804242486186977076164290033373913707654134680188215768243140771030452255415899755865868664300363323597447629772697152280904844241955882490102366731357635413114450115342754879407377583554363024903414099182575081761602430505168713042908502824540598074881897103746444487643789756656866563996478864339776660187994830128180901836802609305337393169233940810850334954844734351233176485825993422195110091742991582981694723362766532450396825990637199710300836720425733904633221862633140406646325438770987356673794087934079714925391623657263106674731411202754711280639483814428913435548956637991855491756776477498248950567351547380031249267224462049204226698902680800602929639266448277227648036766258697622677257592622880796582843040065889927455946187061208465381880645326934704804407788209945307399546272072143428648959i

_dhG : InfiniteInt
_dhG = 2i

--8192-bit to 256-bit key, doing this to somewhat keep the "full output" of DH, rather then just mod by 256
_reduceKey : InfiniteInt -> Key
_reduceKey n =
    let n = xorBitI (shiftRI n 4096) (modI n (2i ^i 4096i)) in    --8192 -> 4096
    let n = xorBitI (shiftRI n 2048) (modI n (2i ^i 2048i)) in    --4096 -> 2048
    let n = xorBitI (shiftRI n 1024) (modI n (2i ^i 1024i)) in    --2048 -> 1024
    let n = xorBitI (shiftRI n 512)  (modI n (2i ^i 512i)) in     --1024 -> 512
      Key $ xorBitI (shiftRI n 256)  (modI n (2i ^i 256i))        --512 -> 256


dhA : forall a . DHProtocol ; a -> (a, Key)
dhA c0 =
    let (aSecret, rng) = nextN64Bits 4 (newRNGState ()) in
    let aShared = _modExp _dhG aSecret _dhP in
    let c1 = send aShared c0 in
    let (bShared, c2) = receive c1 in
    (c2, _reduceKey $ _modExp bShared aSecret _dhP)

dhB : forall a . dualof DHProtocol ; a -> (a, Key)
dhB c0 =
    let (bSecret, rng) = nextN64Bits 4 (newRNGState ()) in
    let (aShared, c1) = receive c0 in
    let bShared = (_modExp _dhG bSecret _dhP) in
    let c2 = send bShared c1 in
    (c2, _reduceKey $ _modExp aShared bSecret _dhP)




--   ██████ ██   ██  █████   ██████ ██   ██  █████  ██████   ██████  
--  ██      ██   ██ ██   ██ ██      ██   ██ ██   ██      ██ ██  ████ 
--  ██      ███████ ███████ ██      ███████ ███████  █████  ██ ██ ██ 
--  ██      ██   ██ ██   ██ ██      ██   ██ ██   ██ ██      ████  ██ 
--   ██████ ██   ██ ██   ██  ██████ ██   ██ ██   ██ ███████  ██████    


data Nonce = Nonce InfiniteInt                  -- 96-bit
data ChachaState = ChachaState (Nonce, Int)     -- Nonce + Counter
data Stream = Stream InfiniteInt                -- 512-bit
data Block = Block Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int Int -- 4x4 matrix of 32-bit values, This should be a mutable array.

_getNonce : Nonce -> InfiniteInt
_getNonce (Nonce nonceValue) = nonceValue

_getStream : Stream -> InfiniteInt
_getStream (Stream streamValue) = streamValue

_newNonce : () -> Nonce
_newNonce u = 
    let rng = newRNGState u in
    let (nonce, rng) = nextN64Bits 2 rng in
    Nonce $ modI nonce (2i ^i 96i)

-- Round assisting functions

_clampTo32 : Int -> Int
_clampTo32 x = mod x (2 ^ 32)

_bitRotation32 : Int -> Int -> Int
_bitRotation32 value shift = orBit (shiftL value shift) (shiftR value (32 - shift))

-- Chacha Rounds

_quarterRound : Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
_quarterRound a b c d =
    let a = _clampTo32 (a + b) in let d = xorBit a d in let d = _clampTo32 (_bitRotation32 d 16) in
    let c = _clampTo32 (c + d) in let b = xorBit b c in let b = _clampTo32 (_bitRotation32 b 12) in
    let a = _clampTo32 (a + b) in let d = xorBit a d in let d = _clampTo32 (_bitRotation32 d 8) in
    let c = _clampTo32 (c + d) in let b = xorBit b c in let b = _clampTo32 (_bitRotation32 b 7) in
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

_splitTo32Bit : InfiniteInt -> Int -> [Int]
_splitTo32Bit value i =
    if i == 0 then
        []
    else
        let part = infiniteToInteger $ modI value (2i ^i 32i) in
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
_chacha20 (Key keyValue) (ChachaState nonceCounter) =
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
    let stream = foldr @InfiniteInt (\x : Int y : InfiniteInt -> (shiftLI y 32) +i (integerToInfinite x)) 0i (_blockToList block) in

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
        (Stream (orBitI (shiftLI streamLValue 512) streamRValue), chachaState)




--  ███████ ███████  ██████ ██    ██ ██████  ███████      ██████  ██████  ███    ███ ███    ███ ██    ██ ███    ██ ██  ██████  █████  ████████ ██  ██████  ███    ██ 
--  ██      ██      ██      ██    ██ ██   ██ ██          ██      ██    ██ ████  ████ ████  ████ ██    ██ ████   ██ ██ ██      ██   ██    ██    ██ ██    ██ ████   ██ 
--  ███████ █████   ██      ██    ██ ██████  █████       ██      ██    ██ ██ ████ ██ ██ ████ ██ ██    ██ ██ ██  ██ ██ ██      ███████    ██    ██ ██    ██ ██ ██  ██ 
--       ██ ██      ██      ██    ██ ██   ██ ██          ██      ██    ██ ██  ██  ██ ██  ██  ██ ██    ██ ██  ██ ██ ██ ██      ██   ██    ██    ██ ██    ██ ██  ██ ██ 
--  ███████ ███████  ██████  ██████  ██   ██ ███████      ██████  ██████  ██      ██ ██      ██  ██████  ██   ████ ██  ██████ ██   ██    ██    ██  ██████  ██   ████ 


---- Secure channel establishment ----

data SecureChannelState = SecureChannelState (Key, ChachaState)

type EstablishSecureChannel = DHProtocol ; !Nonce

_getSecureChannelState : SecureChannelState -> (Key, ChachaState)
_getSecureChannelState (SecureChannelState keyChachaState) = keyChachaState

establishSecureChannelA : forall a . EstablishSecureChannel ; a -> (a, SecureChannelState)
establishSecureChannelA c =
    let (c, key) = dhA @(!Nonce ; a) c in
    let nonce = _newNonce () in
    let c = send nonce c in
    (c, SecureChannelState (key, ChachaState (nonce, 0)))

establishSecureChannelB : dualof EstablishSecureChannel ; a -> (a, SecureChannelState)
establishSecureChannelB c =
    let (c, key) = dhB @(?Nonce ; a) c in
    let (nonce, c) = receive c in
    (c, SecureChannelState (key, ChachaState (nonce, 0)))


---- Secure Send and Receive ----

--Encodes values sign into the least significant bit, shifting everyrhing else
_encodeSign : InfiniteInt -> InfiniteInt
_encodeSign value =
    if value <i 0i then
        orBitI (shiftLI (value *i -1i) 1) 1i
    else
        shiftLI value 1

_decodeSign : InfiniteInt -> InfiniteInt
_decodeSign value =
    if oddI value then
        -1i *i (shiftRI value 1)
    else
        shiftRI value 1

--Finds value's bit size in multiples of 512
_calculateSize : InfiniteInt -> Int
_calculateSize value =
    if value ==i 0i then
        0
    else
        (_calculateSize (shiftRI value 512)) + 1


--Close Wait:

type SecureClose = Close
secureClose : (Close, SecureChannelState) -> ()
secureClose ck = 
    let (c, secureState) = ck in 
    close c

type SecureWait = Wait
secureWait : (Wait, SecureChannelState) -> ()
secureWait ck = 
    let (c, secureState) = ck in 
    wait c


--Send/Recive Bits:

data Bits = Bits InfiniteInt

_getBits : Bits -> InfiniteInt
_getBits (Bits bits) = bits


type SecureSend = !Bits
_secureSend : Bits -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
_secureSend (Bits bits) sc =
    --Separate channel, key, and chacha20 state
    let (c, secureState) = sc in
    let (key, chachaState) = _getSecureChannelState secureState in
    --Encode sign into the value bit representaion, otherwise the values sign is exposed as it is not encrypted
    let bits = _encodeSign bits in
    --Calculate message size
    let size = _calculateSize bits in
    --Obtain chacha20 stream and update secure state
    let (stream, chachaState) = _multipleChacha20 key chachaState size in
    let secureState = SecureChannelState (key, chachaState) in
    --Encrypt and send message
    let bits = xorBitI bits (_getStream stream) in
    let msg = (Bits bits) in
    -- print @(String, Bits) $ ("Sending: ", msg); --TODO: Remove this line
    (send msg c, secureState)

type SecureReceive = ?Bits
_secureReceive : forall a . (SecureReceive ; a, SecureChannelState) -> (Bits, (a, SecureChannelState))
_secureReceive sc =
    --Separate channel, key, and chacha20 state
    let (c, secureState) = sc in
    let (key, chachaState) = _getSecureChannelState secureState in
    --Receive message
    let (msg, c) = receive c in
    -- print @(String, Bits) $ ("Received: ", msg); --TODO: Remove this line
    let bits = _getBits msg in
    --Calculate message size
    let size = _calculateSize bits in
    --Obtain chacha20 stream and update secure state
    let (stream, chachaState) = _multipleChacha20 key chachaState size in
    let secureState = SecureChannelState (key, chachaState) in
    --Decrypt message
    let bits = xorBitI bits (_getStream stream) in
    --Decode sign
    let bits = _decodeSign bits in
    (Bits bits, (c, secureState))


--InfiniteInt:

type SecureSendInfiniteInt = SecureSend
secureSendInfiniteInt : InfiniteInt -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInfiniteInt msg sc = _secureSend (Bits msg) @a sc

type SecureReceiveInfiniteInt = SecureReceive
secureReceiveInfiniteInt : forall a . (SecureReceive ; a, SecureChannelState) -> (InfiniteInt, (a, SecureChannelState))
secureReceiveInfiniteInt sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (msg, sc)


--Int:

type SecureSendInt = SecureSend
secureSendInt : Int -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInt msg sc = _secureSend (Bits $ integerToInfinite msg) @a sc

type SecureReceiveInt = SecureReceive
secureReceiveInt : forall a . (SecureReceive ; a, SecureChannelState) -> (Int, (a, SecureChannelState))
secureReceiveInt sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (infiniteToInteger msg, sc)


--Char

type SecureSendChar = SecureSend
secureSendChar : Char -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendChar msg sc = _secureSend (Bits (integerToInfinite (ord msg))) @a sc

type SecureReceiveChar = SecureReceive
secureReceiveChar : forall a . (SecureReceive ; a, SecureChannelState) -> (Char, (a, SecureChannelState))
secureReceiveChar sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (chr (infiniteToInteger msg), sc)


--Float:

_addMantissa : InfiniteInt -> Float -> Int -> InfiniteInt
_addMantissa bits mantissa i = 
    if i <= 0 then
        bits
    else
        let mantissa = mantissa *. 2.0 in
        let bits = shiftLI bits 1 in
        if mantissa >=. 1.0 then
            _addMantissa (bits +i 1i) (mantissa -. 1.0) (i-1)
        else
            _addMantissa bits mantissa (i-1)

_getMantissa : InfiniteInt -> Float -> Int -> Float
_getMantissa bits mantissa i =
    --let (bits, i) = removeZeros bits i in
    if i <= 0 then
        mantissa
    else
        if oddI bits then
            _getMantissa (shiftRI bits 1) ((mantissa +. 1.0) /. 2.0) (i-1)
        else
            _getMantissa (shiftRI bits 1) (mantissa /. 2.0) (i-1)

-- From Float to IEEE 754 format in InfiniteInt
_floatToBits : Float -> Bits
_floatToBits value =
    if value >=. 0.0 && value <=. 0.0 then --Why is there no ==. ??????
        Bits 0i
    else if value >=. (1.0 /. 0.0) && value <=. (1.0 /. 0.0) then
        Bits (shiftLI 2047i 52)
    else if value >=. (-1.0 /. 0.0) && value <=. (-1.0 /. 0.0) then
        Bits (shiftLI 4095i 52)
    else
        let sign = integerToInfinite $ abs $ (floor(value/.(absF value)) - 1) / 2 in --Purely arythmetic way to obtain sign bit
        let value = absF value in
        let exponent = fromInteger $ floor $ logBase 2.0 value in
        let mantissa = (value /. (2.0 ** exponent)) -. 1.0 in
        let exponent = integerToInfinite $ (truncate exponent) + 1023 in
        Bits $ _addMantissa (orBitI exponent (shiftLI sign 11)) mantissa 52

-- From IEEE 754 format in InfiniteInt to Float
_bitsToFloat : Bits -> Float
_bitsToFloat (Bits bits) = 
    if bits ==i 0i then
        0.0
    else if (shiftRI bits 52) ==i 2047i then
        1.0 /. 0.0
    else if (shiftRI bits 52) ==i 4095i then
        -1.0 /. 0.0
    else
        let mantissa = 1.0 +. (_getMantissa bits 0.0 52) in
        let exponent = fromInteger $ infiniteToInteger $ (modI (shiftRI bits 52) (2i ^i 11i)) -i 1023i in
        let sign = fromInteger $ infiniteToInteger $ shiftRI bits 63 in
        (-1.0**sign) *. mantissa *. (2.0**exponent)

type SecureSendFloat = SecureSend
secureSendFloat : Float -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendFloat msg sc = _secureSend (_floatToBits msg) @a sc

type SecureReceiveFloat = SecureReceive
secureReceiveFloat : forall a . (SecureReceive ; a, SecureChannelState) -> (Float, (a, SecureChannelState))
secureReceiveFloat sc = 
    let (msg, sc) = _secureReceive @a sc in
    (_bitsToFloat msg, sc)


--Bool:

type SecureSendBool = SecureSend
secureSendBool : Bool -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendBool msg sc = 
    if msg then 
        _secureSend (Bits 1i) @a sc
    else
        _secureSend (Bits 0i) @a sc

type SecureReceiveBool = SecureReceive
secureReceiveBool : forall a . (SecureReceive ; a, SecureChannelState) -> (Bool, (a, SecureChannelState))
secureReceiveBool sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    if msg ==i 1i then
        (True, sc)
    else if msg ==i 0i then
        (False, sc)
    else
        --TODO: Not sure how this should be handled, returning Flase for now
        (False, sc)

--Unit:

type SecureSendUnit = SecureSend
secureSendUnit : () -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendUnit msg sc = _secureSend (Bits 0i) @a sc

secureReceiveUnit : forall a . (SecureReceive ; a, SecureChannelState) -> ((), (a, SecureChannelState))
secureReceiveUnit sc =
    let (bits, sc) = _secureReceive @a sc in
    ((), sc)

--String:
--TODO: hard to do until strings can be treversed/iterated over
