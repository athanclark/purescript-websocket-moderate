module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException)
import Effect.Aff (Aff, runAff_)
import WebSocket (newWebSocket, WebSocketsApp (..))
import WebSocket.Class as WSC

main :: Effect Unit
main = runAff_ (\_ -> pure unit) do
  let app :: WebSocketsApp Aff String String
      app = WebSocketsApp
        \env ->
            { onclose: \{code,reason,wasClean} -> do
                liftEffect $ log $ "code: " <> show code
                liftEffect $ log $ "reason: " <> show reason
                liftEffect $ log $ "was clean: " <> show wasClean
            , onerror: liftEffect <<< throwException
            , onmessage: \{close} m -> do
                liftEffect $ log $ "message: " <> m
                close
            , onopen: \{send} -> do
                liftEffect $ log "open"
                send "yo"
            }
  newWebSocket "ws://echo.websocket.org" [] app

  let app' :: WebSocketsApp Aff String String
      app' = WebSocketsApp
        \env ->
            { onclose: \{code,reason,wasClean} -> liftEffect $ do
                log $ "class code: " <> show code
                log $ "class reason: " <> show reason
                log $ "class was clean: " <> show wasClean
            , onerror: \e -> liftEffect $ throwException e
            , onmessage: \{close} m -> do
                liftEffect $ log $ "class message: " <> m
                close
            , onopen: \{send} -> do
                liftEffect $ log "class open"
                send "class yo"
            }
  WSC.newWebSocket "ws://echo.websocket.org" [] app'


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
