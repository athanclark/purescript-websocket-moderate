module WebSocket.PingPong where

import Prelude (class Eq, class Show)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.JSONMaybe (JSONMaybe)

-- | Simple type intended to be isomorphic to Haskell's websocket-simple
newtype PingPong a = PingPong (JSONMaybe a)
derive instance genericPingPong :: Generic a rep => Generic (PingPong a) _
derive newtype instance eqPingPing :: (Eq a, Generic a rep) => Eq (PingPong a)
derive newtype instance showPingPing :: (Show a, Generic a rep) => Show (PingPong a)
derive newtype instance encodeJsonPingPong :: EncodeJson a => EncodeJson (PingPong a)
derive newtype instance decodeJsonPingPong :: DecodeJson a => DecodeJson (PingPong a)
