module WebSocket
  ( WEBSOCKET
  , Capabilities
  , Environment
  , Params
  , newWebSocket
  ) where

import Prelude
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, mkEffFn1, mkEffFn2)
import Control.Monad.Eff.Exception (EXCEPTION)



foreign import data WEBSOCKET :: Effect


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
      , onerror   :: String                   -> m Unit
      , onmessage :: Capabilities m -> String -> m Unit
      , onopen    :: Capabilities m           -> m Unit
      }
  }


newWebSocket :: forall eff
              . Params (Eff (ws :: WEBSOCKET | eff))
             -> Eff (err :: EXCEPTION, ws :: WEBSOCKET | eff) Unit
newWebSocket params =
  runEffFn1 newWebSocketImpl
    { url: params.url
    , protocols: params.protocols
    , continue: \env ->
        let conts = params.continue env
        in  { onclose: mkEffFn1 $ \{code,reason,wasClean} -> conts.onclose
                {code, reason: toMaybe reason, wasClean}
            , onerror: mkEffFn1 conts.onerror
            , onmessage: mkEffFn2 (conts.onmessage <<< runCapabilitiesImpl)
            , onopen: mkEffFn1 (conts.onopen <<< runCapabilitiesImpl)
            }
    }
  where
    runCapabilitiesImpl :: forall eff1. CapabilitiesImpl eff1 -> Capabilities (Eff eff1)
    runCapabilitiesImpl cs =
      { send: runEffFn1 cs.send
      , close: cs.close
      , close': \{code,reason} -> runEffFn1 cs.close' {code: toNullable code, reason: toNullable reason}
      , getBufferedAmount: cs.getBufferedAmount
      }



-- * Impl

type CapabilitiesImpl eff =
  { send              :: EffFn1 eff String Unit
  , close             :: Eff eff Unit
  , close'            :: EffFn1 eff { code :: Nullable Int, reason :: Nullable String } Unit
  , getBufferedAmount :: Eff eff Int
  }

type ParamsImpl eff =
  { url       :: String
  , protocols :: Array String
  , continue  :: Environment ->
      { onclose   :: EffFn1 eff { code     :: Int
                                , reason   :: Nullable String
                                , wasClean :: Boolean
                                } Unit
      , onerror   :: EffFn1 eff String Unit
      , onmessage :: EffFn2 eff (CapabilitiesImpl eff) String Unit
      , onopen    :: EffFn1 eff (CapabilitiesImpl eff) Unit
      }
  }



foreign import newWebSocketImpl :: forall eff
                                . EffFn1 (err :: EXCEPTION, ws :: WEBSOCKET | eff)
                                    (ParamsImpl (ws :: WEBSOCKET | eff))
                                    Unit
