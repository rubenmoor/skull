{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Base64
  ( Base64
  , encode
  , unBase64
  , fromByteString
  , fromByteStringUnsafe
  , decode
  , fromText
  , toByteString
  )where

import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..))
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Base64               as Base64
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Opaleye                              (PGText, QueryRunnerColumnDefault (..),
                                                       fieldQueryRunnerColumn)
import qualified TextShow                             as Text

newtype Base64 = Base64 { unBase64 :: ByteString }

instance ToJSON Base64 where
  toJSON = toJSON . Text.decodeUtf8 . unBase64

instance FromJSON Base64 where
  parseJSON = fmap (Base64 . Text.encodeUtf8) . parseJSON

-- postgres: a base64 object can be constructed by a text object
--           (which has a sql representation as PGText)
instance FromField Base64 where
  fromField f mdata = fromText <$> fromField f mdata

instance QueryRunnerColumnDefault PGText Base64 where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

encode :: ByteString -> Base64
encode = Base64 . Base64.encode

fromByteStringUnsafe :: ByteString -> Base64
fromByteStringUnsafe = Base64

fromByteString :: ByteString -> Either Text Base64
fromByteString bs = case Base64.decode bs of
  Left  err -> Left $ Text.pack err
  Right _   -> Right $ Base64 bs

toByteString :: Base64 -> ByteString
toByteString = unBase64

decode :: Base64 -> ByteString
decode = Base64.decodeLenient . unBase64

instance Text.TextShow Base64 where
  showb = Text.fromText . Text.decodeUtf8 . unBase64

fromText :: Text -> Base64
fromText = Base64 . Text.encodeUtf8
