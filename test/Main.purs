module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log, error, errorShow)
import WebSocket (WEBSOCKET, newWebSocket)
import WebSocket.Class as WSC

main :: forall e. Eff (console :: CONSOLE, ws :: WEBSOCKET, err :: EXCEPTION | e) Unit
main = do
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

  WSC.newWebSocket id
    { url: "ws://echo.websocket.org"
    , protocols: []
    , continue: \env ->
        { onclose: \{code,reason,wasClean} -> liftEff $ do
            log $ "class code: " <> show code
            log $ "class reason: " <> show reason
            log $ "class was clean: " <> show wasClean
        , onerror: \e -> liftEff $ error $ "class error: " <> e
        , onmessage: \{close} m -> liftEff $ do
            log $ "class message: " <> m
            close
        , onopen: \{send} -> liftEff $ do
            log "class open"
            send "class yo"
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
