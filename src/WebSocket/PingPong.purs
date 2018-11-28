module WebSocket.PingPong where

import Prelude (bind, pure, ($), (==), (/=))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..))
import Data.Array (unsafeIndex, length) as Array
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Control.Alternative ((<|>))
import Partial.Unsafe (unsafePartial)

-- | Simple type intended to be isomorphic to Haskell's websocket-simple
newtype PingPong a = PingPong (Maybe a)
derive instance genericPingPong :: Generic a a' => Generic (PingPong a) _
instance encodeJsonPingPong :: EncodeJson a => EncodeJson (PingPong a) where
  encodeJson (PingPong mx) = case mx of
    Nothing -> encodeJson ""
    Just x -> encodeJson [x]
instance decodeJsonPingPong :: DecodeJson a => DecodeJson (PingPong a) where
  decodeJson json =
    let str = do
          s <- decodeJson json
          if s == ""
             then pure (PingPong Nothing)
             else fail "Not a PingPong"
        arr = do
          as <- decodeJson json
          if Array.length as /= 1
             then fail "Not a PingPong"
             else pure $ unsafePartial $ PingPong $ Just $ Array.unsafeIndex as 0
    in  str <|> arr
