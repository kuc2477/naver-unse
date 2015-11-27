{-# LANGUAGE OverloadedStrings #-}
module Web.Naver.Fortune ( tellFortune ) where

import Data.Text
import Network.Wreq ( get, defaults, param )


scheme = "https"
host = "m.search.naver.com"
path = "/p/csearch/dcontent/external_api/json_todayunse_v2.naver"


-- | Tell today's fortune based on given birth year, month and day.
tellFortune :: Int  -- ^ The birth year of a user to be fortune telled
            -> Int  -- ^ The birth month of a user to be fortune telled
            -> Int  -- ^ The birth day of a user to be fortune telled
            -> Text -- ^ The fortune
tellFortune year month day =
    get scheme ++ "://" ++ host ++ path
