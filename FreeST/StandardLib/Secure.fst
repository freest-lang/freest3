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

type Key = InfiniteInt  -- 256-bit

--8192-bit MODP group 18
_dhP : InfiniteInt
_dhP = 68171758476213495590643433084986531271759695309573263235881319185384385557674102439283279615552301110607010260567277060576543672042167353921249641611589827099759815373182646127543631281741314565457020738998902204967231262491310725795783783049784656429244984173561484961967249196407582242419973594946800739429994964183747405442971310357276197966718707884764158104700573267312586099453009222736430034928817707948384625935945930820319750986019930643497543625725884857580157971216801032457458333593944673574522525767607563684739648600967529279953296804207522649084724787612195175422263607098738741914645097619238328447106496924704452040775305936491843260905103268987718680085388156790453979273532133189443896052343062485984002920853239202462209596271775929553414982455918395215636680261863909033790451686705760043324045915933869183540411008141310762080019823231026185722122135090386984943357470309444794098585129592582456002575080219759641815274328607981268105257466267717528269137758126557117498820994251686491762873082385488730053187415605901056747044556560150308610986856182325459772201715941636488490479320970659359714988717520108307866816821009130332898895914296591024879475191128227138086669293956659118949975532739032108101571041195513818810483465657497025265512391425864443585891353148751079869810957758427910917581708563586301897105038951021059341956636980402390581605400836264440381028184599630585971267814830779433087194626769096715786560129644339884489089868936000805734334699917401287732627181269460305815405985087770871167883752365501932268283954605283352502039852443798406398221404956404606149293305776309154934233362524236421338611655298136881804242486186977076164290033373913707654134680188215768243140771030452255415899755865868664300363323597447629772697152280904844241955882490102366731357635413114450115342754879407377583554363024903414099182575081761602430505168713042908502824540598074881897103746444487643789756656866563996478864339776660187994830128180901836802609305337393169233940810850334954844734351233176485825993422195110091742991582981694723362766532450396825990637199710300836720425733904633221862633140406646325438770987356673794087934079714925391623657263106674731411202754711280639483814428913435548956637991855491756776477498248950567351547380031249267224462049204226698902680800602929639266448277227648036766258697622677257592622880796582843040065889927455946187061208465381880645326934704804407788209945307399546272072143428648959i

_dhG : InfiniteInt
_dhG = 2i

--8192-bit to 256-bit key, doing this to somewhat keep the "full output" of DH, rather then just mod by 256
reduceKey : InfiniteInt -> Key
reduceKey n =
    let n = xorBitI (shiftRI n 4096) (modI n (2i ^i 4096i)) in    --8192 -> 4096
    let n = xorBitI (shiftRI n 2048) (modI n (2i ^i 2048i)) in    --4096 -> 2048
    let n = xorBitI (shiftRI n 1024) (modI n (2i ^i 1024i)) in    --2048 -> 1024
    let n = xorBitI (shiftRI n 512)  (modI n (2i ^i 512i)) in     --1024 -> 512
            xorBitI (shiftRI n 256)  (modI n (2i ^i 256i))        --512 -> 256


dhA : forall a . DHProtocol ; a -> (a, Key)
dhA c0 =
    let (aSecret, rng) = nextN64Bits 4 (newRNG ()) in
    let aShared = _modExp _dhG aSecret _dhP in
    let c1 = send aShared c0 in
    let (bShared, c2) = receive c1 in
    (c2, reduceKey $ _modExp bShared aSecret _dhP)

dhB : forall a . dualof DHProtocol ; a -> (a, Key)
dhB c0 =
    let (bSecret, rng) = nextN64Bits 4 (newRNG ()) in
    let (aShared, c1) = receive c0 in
    let bShared = (_modExp _dhG bSecret _dhP) in
    let c2 = send bShared c1 in
    (c2, reduceKey $ _modExp aShared bSecret _dhP)




--   ██████ ██   ██  █████   ██████ ██   ██  █████  ██████   ██████  
--  ██      ██   ██ ██   ██ ██      ██   ██ ██   ██      ██ ██  ████ 
--  ██      ███████ ███████ ██      ███████ ███████  █████  ██ ██ ██ 
--  ██      ██   ██ ██   ██ ██      ██   ██ ██   ██ ██      ████  ██ 
--   ██████ ██   ██ ██   ██  ██████ ██   ██ ██   ██ ███████  ██████                                                                     


type Nonce = InfiniteInt        -- 96-bit
type ChaChaState = (Nonce, Int) -- Nonce + Counter
type Stream = InfiniteInt       -- 512-bit
type Block = [Int]              -- 4x4 matrix of 32-bit values, No list of lists (bidimentional array/matrix) in FreeST.

_newNonce : () -> Nonce
_newNonce u = 
    let rng = newRNG u in
    let (nonce, rng) = nextN64Bits 2 rng in
    modI nonce (2i ^i 96i)

-- Round assisting functions

_clampTo32 : Int -> Int
_clampTo32 x = mod x (2 ^ 32)

_bitRotation32 : Int -> Int -> Int
_bitRotation32 value shift = orBit (shiftL value shift) (shiftR value (32 - shift))

_setNth : [Int] -> Int -> Int -> [Int]
_setNth list n value = (take n list) ++ value :: (drop (n+1) list)


-- Rounds

_quarterRound : Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
_quarterRound a b c d =
    let a = _clampTo32 (a + b) in let d = xorBit a d in let d = _clampTo32 (_bitRotation32 d 16) in
    let c = _clampTo32 (c + d) in let b = xorBit b c in let b = _clampTo32 (_bitRotation32 b 12) in
    let a = _clampTo32 (a + b) in let d = xorBit a d in let d = _clampTo32 (_bitRotation32 d 8) in
    let c = _clampTo32 (c + d) in let b = xorBit b c in let b = _clampTo32 (_bitRotation32 b 7) in
    (a, b, c, d)


_columnRound : Block -> Block
_columnRound block =
    -- Column 1: 0, 4, 8, 12
    let abcd = _quarterRound (nth block 0) (nth block 4) (nth block 8) (nth block 12) in                                                 --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 0 a in let block = _setNth block 4 b in let block = _setNth block 8 c in let block = _setNth block 12 d in    --Write to the block
    -- Column 2: 1, 5, 9, 13
    let abcd = _quarterRound (nth block 1) (nth block 5) (nth block 9) (nth block 13) in                                                 --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 1 a in let block = _setNth block 5 b in let block = _setNth block 9 c in let block = _setNth block 13 d in    --Write to the block
    -- Column 3: 2, 6, 10, 14
    let abcd = _quarterRound (nth block 2) (nth block 6) (nth block 10) (nth block 14) in                                                --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 2 a in let block = _setNth block 6 b in let block = _setNth block 10 c in let block = _setNth block 14 d in   --Write to the block
    -- Column 4: 3, 7, 11, 15
    let abcd = _quarterRound (nth block 3) (nth block 7) (nth block 11) (nth block 15) in                                                --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 3 a in let block = _setNth block 7 b in let block = _setNth block 11 c in let block = _setNth block 15 d in   --Write to the block
    block

_diagonalRound : Block -> Block
_diagonalRound block =
    -- Main diagonal: 0, 5, 10, 15
    let abcd = _quarterRound (nth block 0) (nth block 5) (nth block 10) (nth block 15) in                                                --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 0 a in let block = _setNth block 5 b in let block = _setNth block 10 c in let block = _setNth block 15 d in   --Write to the block
    -- Diagonal 2: 1, 6, 11, 12
    let abcd = _quarterRound (nth block 1) (nth block 6) (nth block 11) (nth block 12) in                                                --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 1 a in let block = _setNth block 6 b in let block = _setNth block 11 c in let block = _setNth block 12 d in   --Write to the block
    -- Diagonal 3: 2, 7, 8, 13
    let abcd = _quarterRound (nth block 2) (nth block 7) (nth block 8) (nth block 13) in                                                 --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 2 a in let block = _setNth block 7 b in let block = _setNth block 8 c in let block = _setNth block 13 d in    --Write to the block
    -- Diagonal 4: 3, 4, 9, 14
    let abcd = _quarterRound (nth block 3) (nth block 4) (nth block 9) (nth block 14) in                                                 --Preforme quarter round
    let (a, bcd) = abcd in let (b,cd) = bcd in let (c, d) = cd in                                                                       --Unpackge the result
    let block = _setNth block 3 a in let block = _setNth block 4 b in let block = _setNth block 9 c in let block = _setNth block 14 d in    --Write to the block
    block


_doNRounds : Block -> Int -> Block
_doNRounds block n =
    if n == 0 then
        block
    else
        let block = _columnRound block in
        let block = _diagonalRound block in
        _doNRounds block (n - 1)

-- ChaCha

_splitTo32Bit : InfiniteInt -> Int -> [Int]
_splitTo32Bit value i =
    if i == 0 then
        []
    else
        let part = infiniteToInteger $ modI value (2i ^i 32i) in
        (_splitTo32Bit (shiftRI value 32) (i-1)) ++ [part]

__chacha20Once : Key -> ChaChaState -> (Stream, ChaChaState)
__chacha20Once key nonceCounter =
    let (nonce, counter) = nonceCounter in
    let block = [1634760805, 857760878, 2036477234, 1797285236] in  -- "expand 32-byte k" -> ["expa", "nd 3", "2-by", "te k"]
    let block = block ++ _splitTo32Bit key 8 in                      -- Key in row 2 and 3
    let block = block ++ [counter] in                               -- Counter first column last row
    let block = block ++ _splitTo32Bit nonce 3 in                    -- Nonce last 3 values of last row
    let block = _doNRounds block 20 in
    let stream = foldr @InfiniteInt (\x : Int y : InfiniteInt -> (shiftLI y 32) +i (integerToInfinite x)) 0i block in    -- Read final matrix from right to left, so the resulting right most bits are the first values of the matrix (as is supposed)
    (stream, (nonce, counter + 1))

--Gives stream of size * 512 bits
_chacha20 : Key -> ChaChaState -> Int -> (Stream, ChaChaState)
_chacha20 key chaChaState size =
    if size <= 1 then
        __chacha20Once key chaChaState
    else
        let (streamL, chaChaState) = _chacha20 key chaChaState (size - 1) in
        let (streamR, chaChaState) = __chacha20Once key chaChaState in
        ((orBitI (shiftLI streamL 512) streamR), chaChaState)


--  ███████ ███████  ██████ ██    ██ ██████  ███████      ██████  ██████  ███    ███ ███    ███ ██    ██ ███    ██ ██  ██████  █████  ████████ ██  ██████  ███    ██ 
--  ██      ██      ██      ██    ██ ██   ██ ██          ██      ██    ██ ████  ████ ████  ████ ██    ██ ████   ██ ██ ██      ██   ██    ██    ██ ██    ██ ████   ██ 
--  ███████ █████   ██      ██    ██ ██████  █████       ██      ██    ██ ██ ████ ██ ██ ████ ██ ██    ██ ██ ██  ██ ██ ██      ███████    ██    ██ ██    ██ ██ ██  ██ 
--       ██ ██      ██      ██    ██ ██   ██ ██          ██      ██    ██ ██  ██  ██ ██  ██  ██ ██    ██ ██  ██ ██ ██ ██      ██   ██    ██    ██ ██    ██ ██  ██ ██ 
--  ███████ ███████  ██████  ██████  ██   ██ ███████      ██████  ██████  ██      ██ ██      ██  ██████  ██   ████ ██  ██████ ██   ██    ██    ██  ██████  ██   ████ 


---- Secure channel establishment ----

type SecureChannelState = (Key, ChaChaState)

type EstablishSecureChannel = DHProtocol ; !Nonce

establishSecureChannelA : forall a . EstablishSecureChannel ; a -> (a, SecureChannelState)
establishSecureChannelA c =
    let (c, key) = dhA @(!Nonce ; a) c in
    let nonce = _newNonce () in
    let c = send nonce c in
    (c, (key, (nonce, 0)))


establishSecureChannelB : dualof EstablishSecureChannel ; a -> (a, SecureChannelState)
establishSecureChannelB c =
    let (c, key) = dhB @(?Nonce ; a) c in
    let (nonce, c) = receive c in
    (c, (key, (nonce, 0)))

---- Secure Send and Receive ----

type SecureSend = !InfiniteInt
type SecureReceive = ?InfiniteInt


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

secureClose : (Close, SecureChannelState) -> ()
secureClose ck = 
    let (c, secureState) = ck in 
    close c

secureWait : (Wait, SecureChannelState) -> ()
secureWait ck = 
    let (c, secureState) = ck in 
    wait c


--InfiniteInt:

secureSendInfiniteInt : InfiniteInt -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInfiniteInt msg sc =
    --Separate channel, key, and _chacha20 state
    let (c, secureState) = sc in
    let (key, chaChaState) = secureState in
    --Encode sign into the value bit representaion, otherwise the values sign is exposed as it is not encrypted
    let msg = _encodeSign msg in
    --Calculate msg size
    let size = _calculateSize msg in
    --Obtain _chacha20 stream and update secure state
    let (stream, chaChaState) = _chacha20 key chaChaState size in
    let secureState = (key, chaChaState) in
    --Encrypt and send message
    let msg = xorBitI msg stream in
    (send msg c, secureState)

secureReceiveInfiniteInt : forall a . (SecureReceive ; a, SecureChannelState) -> (InfiniteInt, (a, SecureChannelState))
secureReceiveInfiniteInt sc =
    --Separate channel, key, and _chacha20 state
    let (c, secureState) = sc in
    let (key, chaChaState) = secureState in
    --Receive message
    let (msg, c) = receive c in
    --Calculate msg size
    let size = _calculateSize msg in
    --Obtain _chacha20 stream and update secure state
    let (stream, chaChaState) = _chacha20 key chaChaState size in
    let secureState = (key, chaChaState) in
    --Decrypt message
    let msg = xorBitI msg stream in
    --Decode sign
    let msg = _decodeSign msg in
    (msg, (c, secureState))


--Int:

secureSendInt : Int -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInt msg sc = secureSendInfiniteInt (integerToInfinite msg) @a sc

secureReceiveInt : forall a . (SecureReceive ; a, SecureChannelState) -> (Int, (a, SecureChannelState))
secureReceiveInt sc = 
    let (msg, sc) = secureReceiveInfiniteInt @a sc in
    (infiniteToInteger msg, sc)


--Char
secureSendChar : Char -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendChar msg sc = secureSendInfiniteInt (integerToInfinite (ord msg)) @a sc

secureReceiveChar : forall a . (SecureReceive ; a, SecureChannelState) -> (Char, (a, SecureChannelState))
secureReceiveChar sc = 
    let (msg, sc) = secureReceiveInfiniteInt @a sc in
    (chr (infiniteToInteger msg), sc)


--Float:
--TODO:


--String:
--TODO: hard to do until strings can be treversed/iterated over
