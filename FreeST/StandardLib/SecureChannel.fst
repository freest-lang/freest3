module SecureChannel where

import SecureUtils
import DiffieHellman
import ChaCha20
import RSA

type EstablishSecureChannelA = DHProtocol ; Newchacha20Exchange
type EstablishSecureChannelB = dualof EstablishSecureChannelA


--Unsigned Cypher Suit : DiffieHellman + chacha20

establishSecureChannelA : forall a . EstablishSecureChannelA ; a -> (a, SecureChannelState)
establishSecureChannelA c =
    let (c, key) = dhA @(Newchacha20Exchange ; a) c in
    let (encryptionAlgorithm, c) = newchacha20A @a c in
    (c, SecureChannelState (key, encryptionAlgorithm))

establishSecureChannelB : forall a . EstablishSecureChannelB ; a -> (a, SecureChannelState)
establishSecureChannelB c =
    let (c, key) = dhB @(dualof Newchacha20Exchange ; a) c in
    let (encryptionAlgorithm, c) = newchacha20B @a c in
    (c, SecureChannelState (key, encryptionAlgorithm))


--Signed Cypher Suit : DiffieHellman + RSA + chacha20

establishSecureAuthenticatedChannelA : forall a . String -> String -> EstablishSecureChannelA ; a -> (a, SecureChannelState)
establishSecureAuthenticatedChannelA piKeyString puKeyString c =
    let piKey = getKeyFromFile piKeyString in
    let puKey = getKeyFromFile puKeyString in
    let (c, key) = signedDHA @(Newchacha20Exchange ; a) piKey puKey c in
    let (encryptionAlgorithm, c) = newchacha20A @a c in
    (c, SecureChannelState (key, encryptionAlgorithm))

establishSecureAuthenticatedChannelB : forall a . String -> String -> EstablishSecureChannelB ; a -> (a, SecureChannelState)
establishSecureAuthenticatedChannelB piKeyString puKeyString c =
    let piKey = getKeyFromFile piKeyString in
    let puKey = getKeyFromFile puKeyString in
    let (c, key) = signedDHB @(dualof Newchacha20Exchange ; a) piKey puKey c in
    let (encryptionAlgorithm, c) = newchacha20B @a c in
    (c, SecureChannelState (key, encryptionAlgorithm))