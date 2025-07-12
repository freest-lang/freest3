--TODO:
-- Confirm that new nounces are being created per message as expected and that both sides are maintaingin the same state (rng).
-- Add Poly1305 tag generation and verification.

module SecureChannel where

import SecureUtils
import DiffieHellman
import ChaCha20Poly1305
import RSA

type EstablishSecureChannelA = DHProtocol ; NewChaChaPolyExchange
type EstablishSecureChannelB = dualof EstablishSecureChannelA


--Unsigned Cypher Suit : DiffieHellman + chacha20

establishSecureChannelA : forall a . EstablishSecureChannelA ; a -> (a, SecureChannelState)
establishSecureChannelA c =
    let (c, key) = dhA @(NewChaChaPolyExchange ; a) c in
    let (nextCrypt, c) = newChaChaPolyA @a c in
    (c, SecureChannelState (key, nextCrypt))

establishSecureChannelB : forall a . EstablishSecureChannelB ; a -> (a, SecureChannelState)
establishSecureChannelB c =
    let (c, key) = dhB @(dualof NewChaChaPolyExchange ; a) c in
    let (nextCrypt, c) = newChaChaPolyB @a c in
    (c, SecureChannelState (key, nextCrypt))


--Signed Cypher Suit : DiffieHellman + RSA + chacha20

establishSecureAuthenticatedChannelA : forall a . String -> String -> EstablishSecureChannelA ; a -> (a, SecureChannelState)
establishSecureAuthenticatedChannelA piKeyPath puKeyPath c =
    let piKey = getKeyFromFile piKeyPath in
    let puKey = getKeyFromFile puKeyPath in
    let (c, key) = signedDHA @(NewChaChaPolyExchange ; a) piKey puKey c in
    let (nextCrypt, c) = newChaChaPolyA @a c in
    (c, SecureChannelState (key, nextCrypt))

establishSecureAuthenticatedChannelB : forall a . String -> String -> EstablishSecureChannelB ; a -> (a, SecureChannelState)
establishSecureAuthenticatedChannelB piKeyPath puKeyPath c =
    let piKey = getKeyFromFile piKeyPath in
    let puKey = getKeyFromFile puKeyPath in
    let (c, key) = signedDHB @(dualof NewChaChaPolyExchange ; a) piKey puKey c in
    let (nextCrypt, c) = newChaChaPolyB @a c in
    (c, SecureChannelState (key, nextCrypt))