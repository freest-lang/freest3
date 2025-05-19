module RSA where

import SecureUtils
import File

-- Creating and Storing Keys

--3072-bit key, so 1536-bit p and q

_millerRabinLoop2 : Integer -> Integer -> Integer -> Bool
_millerRabinLoop2 x d n =
    if d ==i (n -i 1i) then
        False
    else
        let x = modI (x ^i 2i) n in
        let d = d *i 2i in
        if x ==i (n -i 1i) then
            True
        else if x ==i 1i then
            False
        else
            _millerRabinLoop2 x d n

_millerRabinLoop1 : Integer -> Integer -> Int -> RNGState -> (Bool, RNGState)
_millerRabinLoop1 d n k rng =
    if k == 0 then
        (True, rng)
    else
        let (a, rng) = nextN64Bits 24 rng in -- 24*64 = 1536 bits
        let a = 2i +i (modI a (n -i 3i)) in
        let x =  modExp a d n in
        if x ==i 1i || x ==i (n -i 1i) then
            (True, rng)
        else if _millerRabinLoop2 x d n then
            _millerRabinLoop1 d n (k - 1) rng
        else
            (False, rng)

_decompose : Integer -> Integer
_decompose n =
    if oddI n then
        n
    else
        _decompose (n /i 2i)

_millerRabin : Integer -> Int -> RNGState -> (Bool, RNGState)
_millerRabin n k rng =
    if evenI n then
        (False, rng)
    else
        let d = _decompose (n -i 1i) in
        _millerRabinLoop1 d n k rng

_generatePrime : RNGState -> (Integer, RNGState)
_generatePrime rng =
    let (n, rng) = nextN64Bits 24 rng in -- 24*64 = 1536 bits
    let (isPrime, rng) = _millerRabin n 20 rng in
    if isPrime then
        (n, rng)
    else
        _generatePrime rng

_modInverseLoop : Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
_modInverseLoop a m t new_t r new_r =
    if new_r ==i 0i then
        modI t m
    else
        let quotient = divI r new_r in
        _modInverseLoop a m new_t (t -i quotient *i new_t) new_r (r -i quotient *i new_r)

_modInverse : Integer -> Integer -> Integer
_modInverse a m = _modInverseLoop a m 0i 1i m a


generateRSAKeyPair : String -> ()
generateRSAKeyPair filePath =
    --Finding p and q --TODO: Add paralelism to this to speed up the process?
    let rng = newRNGState () in
    print @String "Finding Large Prime p, this might take a while...";
    let (p, rng) = _generatePrime rng in
    print @(String, Integer) ("Prime p", p);
    print @String "Finding Large Prime q, this might take a while...";
    let (q, rng) = _generatePrime rng in
    print @(String, Integer) ("Prime q", q);
    --Calculating Keys
    print @String "Calculating keys";
    let n = p *i q in
    let phi_n = (p -i 1i) *i (q -i 1i) in
    let e = 65537i in --Could happen to not be coprime with phi_n, but it's unlikely
    let d = _modInverse e phi_n in
    print @(String, Integer) ("Modulus", n);
    print @(String, Integer) ("Private exponent", d);
    print @(String, Integer) ("Public exponent", e);
    if (shiftRI n 3069) ==i 0i then --Up to 3 leading 0s are accepted (something like 12.5% of happening ig), up to 3069-bits can be encrypted
        print @String "Modulus too small, restarting";
        print @String "------------------//------------------";
        generateRSAKeyPair filePath
    else
        --Writing Keys to Files
        print @String "Creating key files";
        let piFile = openWriteFile $ filePath ^^ ".priv" in
        let puFile = openWriteFile $ filePath ^^ ".pub" in
        --Writing Modulus n
        let piFile = hPrint @Integer n piFile in
        let puFile = hPrint @Integer n puFile in
        --Writing Private exponent
        let piFile = hPrint @Integer d piFile in
        --Writing Public exponent
        let puFile = hPrint @Integer e puFile in
        --Closing Files
        hCloseOut piFile;
        hCloseOut puFile;
        print @String $ "RSA Keypair sucsessfully created on " ^^ filePath;
        ()

-- Reading keys

getKeyFromFile : FilePath -> Key
getKeyFromFile filePath =
    let file = openReadFile filePath in
    --Reading values
    let (_modulus, file) = hGetLine file in
    let (_key, file) = hGetLine file in
    --Closing Streams/Files
    hCloseIn file;
    --Converting Strings to Integers
    let modulus = readInteger _modulus in
    let key = readInteger _key in
    (AsymmetricKey modulus key)
