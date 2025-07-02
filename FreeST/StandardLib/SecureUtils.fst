module SecureUtils where

import Random

----- Types ------

data Key = SessionKey Integer | AsymmetricKey Integer Integer-- Modulus PiKey/PuKey
type EncryptionAlgorithm = Integer -> Key -> (Integer, EncryptionAlgorithm)
data Signature = Signature Integer
data SecureChannelState = SecureChannelState (Key, EncryptionAlgorithm)

getSecureChannelState : SecureChannelState -> (Key, EncryptionAlgorithm)
getSecureChannelState (SecureChannelState keyEncryptionAlgorithm) = keyEncryptionAlgorithm


----- Modular Exponentiation -----

modExp : Integer -> Integer -> Integer -> Integer
modExp b e m = 
    if e ==i 0i then 
        1i
    else if evenI e then
        modExp (modI (b *i b) m) (e /i 2i) m
    else
        modI (b *i modExp b (e -i 1i) m) m


----- Hashing -----

hash256 : Integer -> Integer
hash256 value = fst @Integer @RNGState $ nextN64Bits 4 $ RNGState (1, value) --Not a good hash, but a nice shortcut


----- Assymetric Encryiption ------

asymEncrypt : Integer -> Key -> Integer
asymEncrypt msg (AsymmetricKey modulus key) = modExp msg key modulus
asymEncrypt _ _ = error @Integer "Asymmetric Encryption/Decryption does not use symmetric/session keys."

asymDecrypt : Integer -> Key -> Integer
asymDecrypt = asymEncrypt

sign : Integer -> Key -> Signature
sign value key = (Signature $ asymEncrypt (hash256 value) key)

checkSignature : Signature -> Integer -> Key -> Bool
checkSignature (Signature signature) value key = (hash256 value) ==i (asymDecrypt signature key)


---- Secure Send and Receive ----

--Encodes values sign into the least significant bit, shifting everyrhing else
_encodeSign : Integer -> Integer
_encodeSign value =
    if value <i 0i then
        lorI (shiftLI (value *i -1i) 1) 1i
    else
        shiftLI value 1

_decodeSign : Integer -> Integer
_decodeSign value =
    if oddI value then
        -1i *i (shiftRI value 1)
    else
        shiftRI value 1

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

data Bits = Bits Integer

_getBits : Bits -> Integer
_getBits (Bits bits) = bits


type SecureSend = !Bits
_secureSend : Bits -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
_secureSend (Bits bits) sc =
    --Separate channel, key, and encryption algorithm
    let (c, secureState) = sc in
    let (key, encryptionAlgorithm) = getSecureChannelState secureState in
    --Encode sign into the value bit representaion, otherwise the values sign is exposed as it is not encrypted
    let bits = _encodeSign bits in
    --Encrypt and update secure state
    let (bits, encryptionAlgorithm) = encryptionAlgorithm bits key in
    let secureState = SecureChannelState (key, encryptionAlgorithm) in    
    --Send message
    let msg = (Bits bits) in
    (send msg c, secureState)

type SecureReceive = ?Bits
_secureReceive : forall a . (SecureReceive ; a, SecureChannelState) -> (Bits, (a, SecureChannelState))
_secureReceive sc =
    --Separate channel, key, and encryption algorithm
    let (c, secureState) = sc in
    let (key, encryptionAlgorithm) = getSecureChannelState secureState in
    --Receive message
    let (msg, c) = receive c in
    let bits = _getBits msg in

    --Decrypt and update secure state
    let (bits, encryptionAlgorithm) = encryptionAlgorithm bits key in
    let secureState = SecureChannelState (key, encryptionAlgorithm) in 
    --Decode sign
    let bits = _decodeSign bits in
    (Bits bits, (c, secureState))


--Integer:

type SecureSendInfiniteInt = SecureSend
secureSendInfiniteInt : Integer -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInfiniteInt msg sc = _secureSend (Bits msg) @a sc

type SecureReceiveInfiniteInt = SecureReceive
secureReceiveInfiniteInt : forall a . (SecureReceive ; a, SecureChannelState) -> (Integer, (a, SecureChannelState))
secureReceiveInfiniteInt sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (msg, sc)


--Int:

type SecureSendInt = SecureSend
secureSendInt : Int -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendInt msg sc = _secureSend (Bits $ intToInteger msg) @a sc

type SecureReceiveInt = SecureReceive
secureReceiveInt : forall a . (SecureReceive ; a, SecureChannelState) -> (Int, (a, SecureChannelState))
secureReceiveInt sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (integerToInt msg, sc)


--Char

type SecureSendChar = SecureSend
secureSendChar : Char -> forall a . (SecureSend ; a, SecureChannelState) -> (a, SecureChannelState)
secureSendChar msg sc = _secureSend (Bits (intToInteger (ord msg))) @a sc

type SecureReceiveChar = SecureReceive
secureReceiveChar : forall a . (SecureReceive ; a, SecureChannelState) -> (Char, (a, SecureChannelState))
secureReceiveChar sc = 
    let (bits, sc) = _secureReceive @a sc in
    let msg = _getBits bits in
    (chr (integerToInt msg), sc)


--Float:

_addMantissa : Integer -> Float -> Int -> Integer
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

_getMantissa : Integer -> Float -> Int -> Float
_getMantissa bits mantissa i =
    --let (bits, i) = removeZeros bits i in
    if i <= 0 then
        mantissa
    else
        if oddI bits then
            _getMantissa (shiftRI bits 1) ((mantissa +. 1.0) /. 2.0) (i-1)
        else
            _getMantissa (shiftRI bits 1) (mantissa /. 2.0) (i-1)

-- From Float to IEEE 754 format in Integer
_floatToBits : Float -> Bits
_floatToBits value =
    if value >=. 0.0 && value <=. 0.0 then --Why is there no ==. ??????
        Bits 0i
    else if value >=. (1.0 /. 0.0) && value <=. (1.0 /. 0.0) then
        Bits (shiftLI 2047i 52)
    else if value >=. (-1.0 /. 0.0) && value <=. (-1.0 /. 0.0) then
        Bits (shiftLI 4095i 52)
    else
        let sign = intToInteger $ abs $ (floor(value/.(absF value)) - 1) / 2 in --Purely arythmetic way to obtain sign bit
        let value = absF value in
        let exponent = fromInteger $ floor $ logBase 2.0 value in
        let mantissa = (value /. (2.0 ** exponent)) -. 1.0 in
        let exponent = intToInteger $ (truncate exponent) + 1023 in
        Bits $ _addMantissa (lorI exponent (shiftLI sign 11)) mantissa 52

-- From IEEE 754 format in Integer to Float
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
        let exponent = fromInteger $ integerToInt $ (modI (shiftRI bits 52) (2i ^i 11i)) -i 1023i in
        let sign = fromInteger $ integerToInt $ shiftRI bits 63 in
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

