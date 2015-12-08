{-# LANGUAGE OverloadedStrings #-}
module Web.Naver.Fortune ( tellFortune
                         , Sex (..)
                         , CalendarType (..)
                         , FortuneDateType (..)
                         , FortuneType (..)
                         ) where

import Data.Maybe
import Data.List ( map )
import Data.Text.Lazy ( Text
                      , pack
                      , unpack
                      , fromStrict
                      , toStrict
                      , replace
                      , stripSuffix
                      , stripPrefix )
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Time.Calendar
import Data.Aeson.Lens ( key )
import Control.Lens
import Control.Applicative
import Network.Wreq ( Options, getWith, responseBody, param, defaults )


-- ====
-- Data
-- ====

-- | Sex type of a user
data Sex = Male | Female

-- | Calendar type to use for fortune telling
data CalendarType = Solar | LunarGeneral | LunarLeap

-- | Fortune type to be telled
data FortuneType = Overall | Love | Wealth | Work | Study

-- | Fortune date type
data FortuneDateType = Today | Tommorow | Monthly


-- ============
-- IO / Parsing
-- ============

-- | Tell today's fortune based on given birth year, month and day.
tellFortune :: Int              -- ^ The birth year of a user to be fortune telled
            -> Int              -- ^ The birth month of a user to be fortune telled
            -> Int              -- ^ The birth day of a user to be fortune telled
            -> Sex              -- ^ The sex of a user to be fortune telled
            -> CalendarType     -- ^ The calendar type to use
            -> FortuneDateType  -- ^ The fortune date type to be telled (e.g. today or monthly)
            -> FortuneType      -- ^ The fortune type to be telled
            -> IO Text          -- ^ The fortune
tellFortune y m d s ct fdt ft =
    parseFortune fdt ft . parseJson <$> fetch y m d s ct

-- | Fetch raw response from naver fortune telling API based on
-- | given birtyh date and calendar type.
fetch :: Int            -- ^ The birth year of a user to be fortune telled
      -> Int            -- ^ The birth month of a user to be fortune telled
      -> Int            -- ^ The birth day of a user to be fortune telled
      -> Sex            -- ^ The sex of a user to be fortune telled
      -> CalendarType   -- ^ The calendar type to use
      -> IO Text        -- ^ The fortune
fetch y m d s ct = do
    raw <- getWith (options y m d s ct) url
    return . decodeUtf8 $ raw ^. responseBody

-- | Strips non JSON part from raw response
parseJson :: Text -> Text
parseJson =
    let prefix = " window.__jindo2_callback._fortune_my_0("
        suffix = ");"
        stripPrefix' = fromMaybe "" . stripPrefix prefix
        stripSuffix' = fromMaybe "" . stripSuffix suffix
    in
        stripPrefix' . stripSuffix'

-- | Parse fortune content from parsed JSON text response
parseFortune :: FortuneDateType -> FortuneType -> Text -> Text
parseFortune fdt ft response =
    let fdk = fortuneDateTypeKey fdt
        fk = fortuneTypeKey ft
    in
      fromStrict . fromMaybe ""  $ response ^? key fdk . _String


-- ===============
-- Option builders
-- ===============

-- | Build query string parameter options from given arguments
options :: Int
        -> Int
        -> Int
        -> Sex
        -> CalendarType
        -> Options
options year month day sex calendarType =
    param "birth" .~ [toStrict $ dateOption year month day] $
      param "gender" .~ [toStrict $ sexOption sex] $
        param "solarCal" .~ [toStrict $ calendarTypeOption calendarType] $
          defaults

-- | Build a date string parameter from given integers of user's birth date.
dateOption :: Int -> Int -> Int -> Text
dateOption y m d = let y' = toInteger y in
  replace "-" "" . pack . showGregorian $ fromGregorian y' m d

-- | Build a sex string parameter from given user's sex.
sexOption :: Sex -> Text
sexOption Male = "m"
sexOption Female = "f"

-- | Build a calendar type string parameter .
calendarTypeOption :: CalendarType -> Text
calendarTypeOption Solar = "solar"
calendarTypeOption LunarGeneral = "lunarGenearl"
calendarTypeOption LunarLeap = "lunarLeap"


-- ================
--  Declared values
-- ================

-- | URL of naver fortune API endpoint
url :: String
url = scheme ++ "://" ++ host ++ path

-- | Scheme of naver fortune API endpoint
scheme :: String
scheme = "https"

-- | Host of naver fortune API endpoint
host :: String
host = "m.search.naver.com"

-- | path to naver fortune API endpoint
path :: String
path = "/p/csearch/dcontent/external_api/json_todayunse_v2.naver"

-- | JSON key for given fortune type
fortuneTypeKey :: FortuneType -> Text
fortuneTypeKey Overall = "총운"
fortuneTypeKey Love = "사랑"
fortuneTypeKey Wealth = "사랑"
fortuneTypeKey Work = "사랑"
fortuneTypeKey Study = "사랑"

-- | JSON key for given fortune date type
fortuneDateTypeKey :: FortuneDateType -> Text
fortuneDateTypeKey Today = "오늘"
fortuneDateTypeKey Tommorow = "오늘"
fortuneDateTypeKey Monthly = "오늘"
