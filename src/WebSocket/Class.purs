module WebSocket.Class where

import Prelude
import WebSocket (WEBSOCKET, Capabilities, Params)
import WebSocket as WS

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)


newWebSocket :: forall eff m resultM
              . MonadEff (ws :: WEBSOCKET | eff) m
             => MonadEff (err :: EXCEPTION, ws :: WEBSOCKET | eff) resultM
             => (forall a. m a -> Eff (ws :: WEBSOCKET | eff) a)
             -> Params m
             -> resultM Unit
newWebSocket runM params =
  liftEff $ WS.newWebSocket
    { url: params.url
    , protocols: params.protocols
    , continue: \env ->
        let conts = params.continue env
        in  { onclose:   \cs   -> runM $ conts.onclose cs
            , onerror:   \e    -> runM $ conts.onerror e
            , onmessage: \cs m -> runM $ conts.onmessage (runCapabilities cs) m
            , onopen:    \cs   -> runM $ conts.onopen $ runCapabilities cs
            }
    }
  where
    runCapabilities :: forall eff1. Capabilities (Eff (ws :: WEBSOCKET | eff)) -> Capabilities m
    runCapabilities {close,close',send,getBufferedAmount} =
      { close:             liftEff close
      , close':            \cs -> liftEff $ close' cs
      , send:              \m -> liftEff $ send m
      , getBufferedAmount: liftEff getBufferedAmount
      }
