module WebSocket.PingPong where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..))

newtype PingPong a = PingPong (Maybe a)
derive instance genericPingPong :: Generic a a' => Generic (PingPong a) _
