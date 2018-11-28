module WebSocket
  ( Capabilities
  , Environment
  , WebSocketsApp (..)
  , newWebSocket
  ) where

import Prelude
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, jsonParser, stringify)
import Data.Profunctor (class Profunctor)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, mkEffectFn1, mkEffectFn2)
import Effect.Exception (Error, throw)




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


newWebSocket :: forall send receive
              . DecodeJson receive
             => EncodeJson send
             => String -- ^ Url
             -> Array String -- ^ Protocols
             -> WebSocketsApp Effect receive send
             -> Effect Unit
newWebSocket url protocols (WebSocketsApp continue) =
  runEffectFn1 newWebSocketImpl
    { url
    , protocols
    , continue: \env ->
        let conts = continue env
        in  { onclose: mkEffectFn1 $ \{code,reason,wasClean} -> conts.onclose
                {code, reason: toMaybe reason, wasClean}
            , onerror: mkEffectFn1 conts.onerror
            , onmessage: mkEffectFn2 \cs s -> case decodeJson =<< jsonParser s of
              Left e -> throw e
              Right x -> conts.onmessage (runCapabilitiesImpl cs) x
            , onopen: mkEffectFn1 (conts.onopen <<< runCapabilitiesImpl)
            }
    }
  where
    runCapabilitiesImpl :: CapabilitiesImpl -> Capabilities Effect send
    runCapabilitiesImpl cs =
      { send: runEffectFn1 cs.send <<< stringify <<< encodeJson
      , close: cs.close
      , close': \{code,reason} -> runEffectFn1 cs.close' {code: toNullable code, reason: toNullable reason}
      , getBufferedAmount: cs.getBufferedAmount
      }



-- * Impl

type CapabilitiesImpl =
  { send              :: EffectFn1 String Unit
  , close             :: Effect Unit
  , close'            :: EffectFn1 { code :: Nullable Int, reason :: Nullable String } Unit
  , getBufferedAmount :: Effect Int
  }

type ParamsImpl =
  { url       :: String
  , protocols :: Array String
  , continue  :: Environment ->
      { onclose   :: EffectFn1 { code     :: Int
                               , reason   :: Nullable String
                               , wasClean :: Boolean
                               } Unit
      , onerror   :: EffectFn1 Error Unit
      , onmessage :: EffectFn2 CapabilitiesImpl String Unit
      , onopen    :: EffectFn1 CapabilitiesImpl Unit
      }
  }



foreign import newWebSocketImpl :: EffectFn1 ParamsImpl Unit
