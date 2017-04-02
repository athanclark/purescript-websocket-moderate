module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log, error, errorShow)
import WebSocket (WEBSOCKET, newWebSocket)

main :: forall e. Eff (console :: CONSOLE, ws :: WEBSOCKET, err :: EXCEPTION, avar :: AVAR | e) Unit
main = do
  log "mer mer mer tests n sturf"
  newWebSocket
    { url: "ws://echo.websocket.org"
    , protocols: []
    , continue: \env ->
        { onclose: \{code,reason,wasClean} -> do
            log $ "code: " <> show code
            log $ "reason: " <> show reason
            log $ "was clean: " <> show wasClean
        , onerror: \e -> error $ "error: " <> e
        , onmessage: \{close} m -> do
            log $ "message: " <> m
            close
        , onopen: \{send} -> do
            log "open"
            send "yo"
        }
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
