module WebSocket.Class where

import Prelude (($), Unit)
import WebSocket (Capabilities, WebSocketsApp (..))
import WebSocket (newWebSocket) as WS

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Effect (Effect)
import Control.Monad.Trans.Control (class MonadBaseControl)


newWebSocket :: forall m stM send receive
              . EncodeJson send
             => DecodeJson receive
             => MonadBaseControl Effect m stM
             => SingletonFunctor stM
             => String -- ^ Url
             -> Array String -- ^ Protocols
             -> WebSocketsApp m receive send
             -> m Unit
newWebSocket url protocols (WebSocketsApp continue) =
  liftBaseWith_ \runInBase -> WS.newWebSocket
    url
    protocols $ WebSocketsApp
    \env ->
      let conts = continue env
      in  { onclose:   \cs   -> runInBase (conts.onclose cs)
          , onerror:   \e    -> runInBase (conts.onerror e)
          , onmessage: \cs m -> runInBase (conts.onmessage (runCapabilities cs) m)
          , onopen:    \cs   -> runInBase (conts.onopen (runCapabilities cs))
          }
  where
    runCapabilities :: Capabilities Effect send -> Capabilities m send
    runCapabilities {close,close',send,getBufferedAmount} =
      { close:             liftBaseWith_ \_ -> close
      , close':            \cs -> liftBaseWith_ \_ -> close' cs
      , send:              \m -> liftBaseWith_ \_ -> send m
      , getBufferedAmount: liftBaseWith_ \_ -> getBufferedAmount
      }
