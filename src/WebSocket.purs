module WebSocket
  ( Capabilities
  , Environment
  , Params
  , newWebSocket
  ) where

import Prelude
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, mkEffectFn1, mkEffectFn2)
import Effect.Exception (Error)




type Capabilities m =
  { send              :: String -> m Unit
  , close             :: m Unit
  , close'            :: { code :: Maybe Int, reason :: Maybe String } -> m Unit
  , getBufferedAmount :: m Int
  }

type Environment =
  { url      :: String
  , protocol :: String
  }

type Params m =
  { url       :: String
  , protocols :: Array String
  , continue  :: Environment ->
      { onclose   :: { code     :: Int
                     , reason   :: Maybe String
                     , wasClean :: Boolean
                     } -> m Unit
      , onerror   :: Error                    -> m Unit
      , onmessage :: Capabilities m -> String -> m Unit
      , onopen    :: Capabilities m           -> m Unit
      }
  }


newWebSocket :: Params Effect
             -> Effect Unit
newWebSocket params =
  runEffectFn1 newWebSocketImpl
    { url: params.url
    , protocols: params.protocols
    , continue: \env ->
        let conts = params.continue env
        in  { onclose: mkEffectFn1 $ \{code,reason,wasClean} -> conts.onclose
                {code, reason: toMaybe reason, wasClean}
            , onerror: mkEffectFn1 conts.onerror
            , onmessage: mkEffectFn2 (conts.onmessage <<< runCapabilitiesImpl)
            , onopen: mkEffectFn1 (conts.onopen <<< runCapabilitiesImpl)
            }
    }
  where
    runCapabilitiesImpl :: CapabilitiesImpl -> Capabilities Effect
    runCapabilitiesImpl cs =
      { send: runEffectFn1 cs.send
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
