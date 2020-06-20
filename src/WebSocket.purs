module WebSocket
  ( Capabilities
  , Environment
  , WebSocketsApp (..)
  , hoistWebSocketsApp
  , dimap'
  , dimapJson
  , dimapStringify
  , newWebSocket
  , newWebSocketString
  , newWebSocketBinary
  , newWebSocketBoth
  , class BinaryType
  , class WebSocketBinary
  , isBinary
  ) where

import Prelude ((*>), Unit, class Applicative, (<<<), pure, unit, ($), class Semigroup, class Monoid, mempty, (>>=), class Bind)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonParser, printJsonDecodeError, stringify)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Profunctor (class Profunctor)
import Data.Symbol (SProxy(..), reflectSymbol, class IsSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, throw)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, mkEffectFn1, mkEffectFn2)
import Foreign (Foreign)
import Type.Proxy (Proxy(..))
import Web.File.Blob (Blob)




type Capabilities m send =
  { send              :: send -> m Unit
  , close             :: m Unit
  , close'            :: { code :: Maybe Int, reason :: Maybe String } -> m Unit
  , getBufferedAmount :: m Int
  }

type Environment =
  { url      :: String
  , protocol :: String
  }

newtype WebSocketsApp m receive send = WebSocketsApp
  ( Environment ->
    { onclose   :: { code     :: Int
                   , reason   :: Maybe String
                   , wasClean :: Boolean
                   } -> m Unit
    , onerror   :: Error                          -> m Unit
    , onmessage :: Capabilities m send -> receive -> m Unit
    , onopen    :: Capabilities m send            -> m Unit
    }
  )

derive instance genericWebSocketsApp :: (Generic (m c) m', Generic a a', Generic b b') => Generic (WebSocketsApp m a b) _
instance profunctorWebSocketsApp :: Profunctor (WebSocketsApp m) where
  dimap receiveF sendF (WebSocketsApp continue) = WebSocketsApp \env ->
    let cont = continue env
        newCaps cs =
          { send: cs.send <<< sendF
          , close: cs.close
          , close': cs.close'
          , getBufferedAmount: cs.getBufferedAmount
          }
    in  { onclose: cont.onclose
        , onerror: cont.onerror
        , onmessage: \cs x -> cont.onmessage (newCaps cs) (receiveF x)
        , onopen: cont.onopen <<< newCaps
        }
instance semigroupWebSocketsApp :: Applicative m => Semigroup (WebSocketsApp m receive send) where
  append (WebSocketsApp f) (WebSocketsApp g) = WebSocketsApp \env ->
    let fCont = f env
        gCont = g env
    in  { onclose: \ps -> fCont.onclose ps *> gCont.onclose ps
        , onerror: \e -> fCont.onerror e *> gCont.onerror e
        , onmessage: \cs x -> fCont.onmessage cs x *> gCont.onmessage cs x
        , onopen: \cs -> fCont.onopen cs *> gCont.onopen cs
        }
instance monoidWebSocketsApp :: Applicative m => Monoid (WebSocketsApp m receive send) where
  mempty = WebSocketsApp \_ ->
    { onclose: \_ -> pure unit
    , onerror: \_ -> pure unit
    , onmessage: \_ _ -> pure unit
    , onopen: \_ -> pure unit
    }


hoistWebSocketsApp :: forall m n s r
                    . (forall a. m a -> n a)
                   -> (forall a. n a -> m a)
                   -> WebSocketsApp m r s
                   -> WebSocketsApp n r s
hoistWebSocketsApp to from (WebSocketsApp f) = WebSocketsApp \env ->
  let {onclose,onerror,onmessage,onopen} = f env
  in  { onclose: to <<< onclose
      , onerror: to <<< onerror
      , onmessage: \cs r -> to (onmessage (mapCapabilities cs) r)
      , onopen: to <<< onopen <<< mapCapabilities
      }
  where
    mapCapabilities {send,close,close',getBufferedAmount} =
      { send: from <<< send
      , close: from close
      , close': from <<< close'
      , getBufferedAmount: from getBufferedAmount
      }


dimap' :: forall m send receive send' receive'
        . Bind m
       => (receive' -> m receive)
       -> (send -> send')
       -> WebSocketsApp m receive send
       -> WebSocketsApp m receive' send'
dimap' from to (WebSocketsApp f) = WebSocketsApp \env ->
  let {onclose,onerror,onmessage,onopen} = f env
  in  { onclose
      , onerror
      , onmessage: \cs r -> from r >>= onmessage (mapCapabilities cs)
      , onopen: onopen <<< mapCapabilities
      }
  where
    mapCapabilities {send,close,close',getBufferedAmount} =
      { close
      , close'
      , getBufferedAmount
      , send: send <<< to
      }


dimapJson :: forall m send receive
           . EncodeJson send
          => DecodeJson receive
          => MonadEffect m
          => WebSocketsApp m receive send
          -> WebSocketsApp m Json Json
dimapJson = dimap' fromJson encodeJson
  where
    fromJson :: Json -> m receive
    fromJson x = case decodeJson x of
      Right y -> pure y
      Left e -> liftEffect (throw (printJsonDecodeError e))



dimapStringify :: forall m
                . MonadEffect m
               => WebSocketsApp m Json Json
               -> WebSocketsApp m String String
dimapStringify = dimap' fromJson stringify
  where
    fromJson :: String -> m Json
    fromJson x = case jsonParser x of
      Right y -> pure y
      Left e -> liftEffect (throw e)



-- | Creates a new websocket, where the send and receive types are encoded and decoded as JSON strings
-- | internally.
newWebSocket :: forall send receive
              . DecodeJson receive
             => EncodeJson send
             => String -- ^ Url
             -> Array String -- ^ Protocols
             -> WebSocketsApp Effect receive send
             -> Effect Unit
newWebSocket url protocols app = newWebSocketString url protocols $ dimapStringify $ dimapJson app


-- | Creates a new websocket, where the send and receive types are monomorphically typed as a String.
newWebSocketString :: String -- ^ Url
                   -> Array String -- ^ Protocols
                   -> WebSocketsApp Effect String String
                   -> Effect Unit
newWebSocketString url protocols app =
  newWebSocketBoth url protocols app (mempty :: WebSocketsApp Effect ArrayBuffer ArrayBuffer)


-- | Creates a new websocket, where the send and receive types are expected to be binary-compatible over
-- | the websocket (a `Blob` or `ArrayBuffer`).
newWebSocketBinary :: forall send receive binaryType
              . WebSocketBinary receive
             => WebSocketBinary send
             => BinaryType receive binaryType
             => IsSymbol binaryType
             => String -- ^ Url
             -> Array String -- ^ Protocols
             -> WebSocketsApp Effect receive send
             -> Effect Unit
newWebSocketBinary url protocols app =
  newWebSocketBoth url protocols mempty app



-- | Creates a new websocket, where binary data is handled by the binary app when available, otherwise
-- | handled by the string app by default.
newWebSocketBoth :: forall send receive binaryType
                  . WebSocketBinary send
                 => WebSocketBinary receive
                 => BinaryType receive binaryType
                 => IsSymbol binaryType
                 => String -- ^ Url
                 -> Array String -- ^ Protocols
                 -> WebSocketsApp Effect String String
                 -> WebSocketsApp Effect receive send
                 -> Effect Unit
newWebSocketBoth url protocols (WebSocketsApp continue) (WebSocketsApp continueBinary) =
  runEffectFn1 newWebSocketImpl
    { url
    , protocols
    , binaryType: reflectSymbol (SProxy :: SProxy binaryType)
    , continue: \env ->
        let conts = continue env
        in  { onclose: mkEffectFn1 $ \{code,reason,wasClean} -> conts.onclose
                {code, reason: toMaybe reason, wasClean}
            , onerror: mkEffectFn1 conts.onerror
            , onmessage: mkEffectFn2 (conts.onmessage <<< runCapabilitiesImpl)
            , onopen: mkEffectFn1 (conts.onopen <<< runCapabilitiesImpl)
            }
    , continueBinary: \env ->
        let conts = continueBinary env
        in  { onclose: mkEffectFn1 $ \{code,reason,wasClean} -> conts.onclose
                {code, reason: toMaybe reason, wasClean}
            , onerror: mkEffectFn1 conts.onerror
            , onmessage: mkEffectFn2 (conts.onmessage <<< runCapabilitiesImpl)
            , onopen: mkEffectFn1 (conts.onopen <<< runCapabilitiesImpl)
            }
    , isBinary: isBinary (Proxy :: Proxy receive)
    }



-- * Impl

type CapabilitiesImpl send =
  { send              :: EffectFn1 send Unit
  , close             :: Effect Unit
  , close'            :: EffectFn1 { code :: Nullable Int, reason :: Nullable String } Unit
  , getBufferedAmount :: Effect Int
  }


runCapabilitiesImpl :: forall send. CapabilitiesImpl send -> Capabilities Effect send
runCapabilitiesImpl cs =
  { send: runEffectFn1 cs.send
  , close: cs.close
  , close': \{code,reason} -> runEffectFn1 cs.close' {code: toNullable code, reason: toNullable reason}
  , getBufferedAmount: cs.getBufferedAmount
  }


type ParamsImpl binary binary' =
  { url       :: String
  , protocols :: Array String
  , binaryType :: String
  , continue  :: Environment ->
      { onclose   :: EffectFn1 { code     :: Int
                               , reason   :: Nullable String
                               , wasClean :: Boolean
                               } Unit
      , onerror   :: EffectFn1 Error Unit
      , onmessage :: EffectFn2 (CapabilitiesImpl String) String Unit
      , onopen    :: EffectFn1 (CapabilitiesImpl String) Unit
      }
  , continueBinary  :: Environment ->
      { onclose   :: EffectFn1 { code     :: Int
                               , reason   :: Nullable String
                               , wasClean :: Boolean
                               } Unit
      , onerror   :: EffectFn1 Error Unit
      , onmessage :: EffectFn2 (CapabilitiesImpl binary') binary Unit
      , onopen    :: EffectFn1 (CapabilitiesImpl binary') Unit
      }
  , isBinary :: Foreign -> Boolean
  }



foreign import newWebSocketImpl :: forall binary binary'. EffectFn1 (ParamsImpl binary binary') Unit





class BinaryType (a :: Type) (binaryType :: Symbol) | a -> binaryType
instance binaryTypeArrayBuffer :: BinaryType ArrayBuffer "arraybuffer"
instance binaryTypeBlob :: BinaryType Blob "blob"


class WebSocketBinary a where
  isBinary :: Proxy a -> Foreign -> Boolean


foreign import isBinaryArrayBufferImpl :: Foreign -> Boolean
foreign import isBinaryBlobImpl :: Foreign -> Boolean


instance webSocketBinaryArrayBuffer :: WebSocketBinary ArrayBuffer where
  isBinary Proxy = isBinaryArrayBufferImpl

instance webSocketBinaryBlob :: WebSocketBinary Blob where
  isBinary Proxy = isBinaryBlobImpl
