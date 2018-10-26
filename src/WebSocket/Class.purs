module WebSocket.Class where

import Prelude
import WebSocket (Capabilities, Params)
import WebSocket as WS

import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Effect (Effect)
import Control.Monad.Trans.Control (class MonadBaseControl)


newWebSocket :: forall m stM
              . MonadBaseControl Effect m stM
             => SingletonFunctor stM
             => Params m
             -> m Unit
newWebSocket params =
  liftBaseWith_ \runInBase -> WS.newWebSocket
    { url: params.url
    , protocols: params.protocols
    , continue: \env ->
        let conts = params.continue env
        in  { onclose:   \cs   -> runInBase (conts.onclose cs)
            , onerror:   \e    -> runInBase (conts.onerror e)
            , onmessage: \cs m -> runInBase (conts.onmessage (runCapabilities cs) m)
            , onopen:    \cs   -> runInBase (conts.onopen (runCapabilities cs))
            }
    }
  where
    runCapabilities :: Capabilities Effect -> Capabilities m
    runCapabilities {close,close',send,getBufferedAmount} =
      { close:             liftBaseWith_ \_ -> close
      , close':            \cs -> liftBaseWith_ \_ -> close' cs
      , send:              \m -> liftBaseWith_ \_ -> send m
      , getBufferedAmount: liftBaseWith_ \_ -> getBufferedAmount
      }
