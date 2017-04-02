module WebSocket.Aff where

import Prelude
import WebSocket (WEBSOCKET, Environment)
import WebSocket as WS

import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Aff (Aff, makeAff', runAff, Canceler)



type Capabilities eff =
  { send :: String -> Aff eff Unit
  , close :: Aff eff Unit
  , close' :: {code :: Maybe Int, reason :: Maybe String} -> Aff eff Unit
  , getBufferedAmount :: Aff eff Int
  }

type Params eff =
  { url       :: String
  , protocols :: Array String
  , continue  :: Environment ->
      { onclose   :: { code :: Int
                     , reason :: Maybe String
                     , wasClean :: Boolean
                     } -> Aff eff Unit
      , onerror   :: String                     -> Aff eff Unit
      , onmessage :: Capabilities eff -> String -> Aff eff Unit
      , onopen    :: Capabilities eff           -> Aff eff Unit
      }
  }



newWebSocket :: forall eff
              . Params (ws :: WEBSOCKET | eff)
             -> Aff (err :: EXCEPTION, ws :: WEBSOCKET | eff) Unit
newWebSocket params = liftEff $ do
  let onError :: forall eff. Error -> Eff eff Unit
      onError _ = pure unit

      onSuccess :: forall eff a. a -> Eff eff Unit
      onSuccess _ = pure unit

  WS.newWebSocket
    { url: params.url
    , protocols: params.protocols
    , continue: \env -> case params.continue env of
        {onclose,onerror,onmessage,onopen} ->
          { onclose: \e -> void $ runAff onError onSuccess $ onclose e
          , onerror: \e -> void $ runAff onError onSuccess $ onerror e
          , onmessage: \cs m -> void $ runAff onError onSuccess $ onmessage (toAffCapabilities cs) m
          , onopen: \cs -> void $ runAff onError onSuccess $ onopen $ toAffCapabilities cs
          }
    }

  where
    toAffCapabilities :: forall eff. WS.Capabilities eff -> Capabilities eff
    toAffCapabilities cs =
      { send: \m -> liftEff $ cs.send m
      , close: liftEff cs.close
      , close': \r -> liftEff $ cs.close' r
      , getBufferedAmount: liftEff cs.getBufferedAmount
      }
