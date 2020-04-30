-- | This module relies on `purescript-text-encoding` and the `text-encoding` NPM module
-- | for turning a String into an ArrayBuffer and vise-versa.

module WebSocket.Text
  (dimapTextEncoding, dimapTextDecoding, Encoding (..)) where

import WebSocket (WebSocketsApp)

import Prelude ((<<<), pure, bind)
import Data.Either (Either (..))
import Data.Profunctor (dimap)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Data.TextDecoding as TD
import Data.TextEncoding as TE
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throwException)



data Encoding
  = Utf8
  | Utf_16Be
  | Utf_16Le

toTd :: Encoding -> TD.Encoding
toTd x = case x of
  Utf8 -> TD.Utf8
  Utf_16Be -> TD.Utf_16Be
  Utf_16Le -> TD.Utf_16Le

toTe :: Encoding -> TE.Encoding
toTe x = case x of
  Utf8 -> TE.Utf8
  Utf_16Be -> TE.Utf_16Be
  Utf_16Le -> TE.Utf_16Le



dimapTextEncoding :: forall m
                   . Encoding
                  -> WebSocketsApp m String String
                  -> WebSocketsApp m ArrayBuffer ArrayBuffer
dimapTextEncoding encoding = dimap (whole' (toTd encoding)) (TA.buffer <<< TE.encode (toTe encoding))


dimapTextDecoding :: forall m
                   . Encoding
                  -> WebSocketsApp m ArrayBuffer ArrayBuffer
                  -> WebSocketsApp m String String
dimapTextDecoding encoding = dimap (TA.buffer <<< TE.encode (toTe encoding)) (whole' (toTd encoding))


whole' :: TD.Encoding -> ArrayBuffer -> String
whole' encoding x = unsafePerformEffect do
  (buf :: Uint8Array) <- TA.whole x
  case TD.decode encoding buf of
    Left e -> throwException e
    Right y -> pure y
