module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import WebSocket (WEBSOCKET, newWebSocket)

main :: forall e. Eff (console :: CONSOLE, ws :: WEBSOCKET, err :: EXCEPTION | e) Unit
main = do
  log "mer"
  newWebSocket
    { url: "ws://echo.websocket.org"
    , protocols: []
    , continue: \env ->
        { onclose: \{code,reason,wasClean} -> do
            log $ "code: " <> show code
            log $ "reason: " <> show reason
            log $ "was clean: " <> show wasClean
        , onerror: \e -> error $ "error: " <> e
        , onmessage: \_ m -> do
            log $ "message: " <> m
        , onopen: \{send} -> do
            log "open"
            send "yo"
        }
    }
