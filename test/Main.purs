module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException)
import WebSocket (newWebSocket, WebSocketsApp (..))
import WebSocket.Class as WSC

main :: Effect Unit
main = do
  newWebSocket
    "ws://echo.websocket.org"
    [] $ WebSocketsApp
    \env ->
        { onclose: \{code,reason,wasClean} -> do
            log $ "code: " <> show code
            log $ "reason: " <> show reason
            log $ "was clean: " <> show wasClean
        , onerror: throwException
        , onmessage: \{close} m -> do
            log $ "message: " <> m
            close
        , onopen: \{send} -> do
            log "open"
            send "yo"
        }

  WSC.newWebSocket
    "ws://echo.websocket.org"
    [] $ WebSocketsApp
    \env ->
        { onclose: \{code,reason,wasClean} -> liftEffect $ do
            log $ "class code: " <> show code
            log $ "class reason: " <> show reason
            log $ "class was clean: " <> show wasClean
        , onerror: \e -> liftEffect $ throwException e
        , onmessage: \{close} m -> liftEffect $ do
            log $ "class message: " <> m
            close
        , onopen: \{send} -> liftEffect $ do
            log "class open"
            send "class yo"
        }


  -- void $ runAff errorShow (\_ -> log "aff success") $ do
  --   (var :: AVar Int) <- makeVar
  --   WSA.newWebSocket
  --     { url: "ws://echo.websocket.org"
  --     , protocols: []
  --     , continue: \env ->
  --         { onclose: \{code,reason,wasClean} -> liftEff $ do
  --             log $ "aff code: " <> show code
  --             log $ "aff reason: " <> show reason
  --             log $ "aff was clean: " <> show wasClean
  --         , onerror: \e -> liftEff $ error $ "aff error: " <> e
  --         , onmessage: \{close} m -> do
  --             x <- takeVar var
  --             liftEff $ log $ "aff message: " <> m <> ", val: " <> show x
  --             putVar var 2
  --             close
  --         , onopen: \{send} -> do
  --             liftEff $ log "aff open"
  --             putVar var 1
  --             send "aff yo"
  --         }
  --     }
  --   x <- takeVar var
  --   liftEff $ log $ "aff val: " <> show x
