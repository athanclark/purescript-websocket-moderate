module WebSocket.Class (newWebSocket, newWebSocketString, newWebSocketBinary, newWebSocketBoth) where

import Prelude (($), Unit, pure, bind)
import WebSocket (Capabilities, WebSocketsApp (..))
import WebSocket
  ( class WebSocketBinary, class BinaryType
  , newWebSocket, newWebSocketBoth, newWebSocketString, newWebSocketBinary) as WS

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Data.Symbol (class IsSymbol)
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
newWebSocket url protocols app = do
  app' <- getApp app
  liftBaseWith_ \_ -> WS.newWebSocket url protocols app'



newWebSocketString :: forall m stM
                    . MonadBaseControl Effect m stM
                   => SingletonFunctor stM
                   => String -- ^ Url
                   -> Array String -- ^ Protocols
                   -> WebSocketsApp m String String
                   -> m Unit
newWebSocketString url protocols app = do
  app' <- getApp app
  liftBaseWith_ \_ -> WS.newWebSocketString url protocols app'



newWebSocketBinary :: forall m stM send receive binaryType
                    . WS.WebSocketBinary send
                   => WS.WebSocketBinary receive
                   => WS.BinaryType receive binaryType
                   => IsSymbol binaryType
                   => MonadBaseControl Effect m stM
                   => SingletonFunctor stM
                   => String -- ^ Url
                   -> Array String -- ^ Protocols
                   -> WebSocketsApp m receive send
                   -> m Unit
newWebSocketBinary url protocols appBinary = do
  appBinary' <- getApp appBinary
  liftBaseWith_ \_ -> WS.newWebSocketBinary url protocols appBinary'



newWebSocketBoth :: forall m stM send receive binaryType
                  . WS.WebSocketBinary send
                 => WS.WebSocketBinary receive
                 => WS.BinaryType receive binaryType
                 => IsSymbol binaryType
                 => MonadBaseControl Effect m stM
                 => SingletonFunctor stM
                 => String -- ^ Url
                 -> Array String -- ^ Protocols
                 -> WebSocketsApp m String String
                 -> WebSocketsApp m receive send
                 -> m Unit
newWebSocketBoth url protocols app appBinary = do
  app' <- getApp app
  appBinary' <- getApp appBinary
  liftBaseWith_ \_ -> WS.newWebSocketBoth url protocols app' appBinary'


getApp :: forall m stM send receive
        . MonadBaseControl Effect m stM
       => SingletonFunctor stM
       => WebSocketsApp m receive send -> m (WebSocketsApp Effect receive send)
getApp (WebSocketsApp continue) = liftBaseWith_ \runInBase -> pure $ WebSocketsApp \env ->
  let conts = continue env
  in  { onclose:   \cs   -> runInBase (conts.onclose cs)
      , onerror:   \e    -> runInBase (conts.onerror e)
      , onmessage: \cs m -> runInBase (conts.onmessage (runCapabilities cs) m)
      , onopen:    \cs   -> runInBase (conts.onopen (runCapabilities cs))
      }


runCapabilities :: forall q m stM
                 . MonadBaseControl Effect m stM
                => SingletonFunctor stM
                => Capabilities Effect q -> Capabilities m q
runCapabilities {close,close',send,getBufferedAmount} =
  { close:             liftBaseWith_ \_ -> close
  , close':            \cs -> liftBaseWith_ \_ -> close' cs
  , send:              \m -> liftBaseWith_ \_ -> send m
  , getBufferedAmount: liftBaseWith_ \_ -> getBufferedAmount
  }
