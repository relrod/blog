{-# LANGUAGE OverloadedStrings #-}
module DJSet where

import qualified Data.ByteString.Lazy.Char8 as C8L
import Data.ByteString.UTF8 (fromString)
import Data.Csv hiding (header)
import Data.List.Split
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Units
import qualified Data.Vector as V
import Hakyll
import Text.Read (readMaybe)

data Song =
  Song { songNumber :: Integer
       , songAlbum :: String
       , songArtist :: String
       , songTitle :: String
       , songKey :: String
       , songBPM :: Double
       , songDuration :: String
       }

instance FromNamedRecord Song where
  parseNamedRecord s =
    Song
    <$> s .: "#"
    <*> s .: "Album"
    <*> s .: "Artist"
    <*> s .: "Title"
    <*> s .: "Key"
    <*> s .: "BPM"
    <*> s .: "Duration"

-- Annoying stuff for calculating total duration

-- | Attempt to parse time duration into seconds.
parseDuration :: String -> Maybe Integer
parseDuration s =
  case splitOn ":" s of
    (min':sec:_) -> do
      minInteger <- readMaybe min' :: Maybe Integer
      secInteger <- readMaybe sec :: Maybe Integer
      return (fromIntegral (addTime (tm minInteger) (ts secInteger) :: Second))
    _ -> Nothing
  where
    ts :: Integer -> Second
    ts = fromIntegral

    tm :: Integer -> Minute
    tm = fromIntegral

-- | Convert seconds into human readable form.
humanReadableDuration :: Integer -> String
humanReadableDuration i =
  let hours = i `div` 3600
      minutes = (i - (hours * 3600)) `div` 60
      seconds = (i - (hours * 3600) - (minutes * 60))
  in show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"

-- | Given a vector of songs, parse and add up all the durations, showing the
-- result in human readable form.
totalDurations :: V.Vector Song -> Maybe String
totalDurations v =
  let maybeParses = V.map (parseDuration . songDuration) v
  in if all isJust maybeParses
     then return . humanReadableDuration . V.sum . catVectorMaybes $ maybeParses
     else Nothing
  where
    catVectorMaybes v' = V.map fromJust . V.filter isJust $ v'

-- End annoying stuff

songContext :: Context Song
songContext =
  field "number"   (return . show . songNumber . itemBody) <>
  field "album"    (return . songAlbum . itemBody) <>
  field "artist"   (return . songArtist . itemBody) <>
  field "title"    (return . songTitle . itemBody) <>
  field "key"      (return . songKey . itemBody) <>
  field "bpm"      (return . take 7 . show . songBPM . itemBody) <>
  field "duration" (return . songDuration . itemBody)

totalDuration :: V.Vector Song -> String
totalDuration songs = fromMaybe "(unknown)" (totalDurations songs)

rawDjSet :: Compiler (V.Vector Song)
rawDjSet = do
  body <- itemBody <$> getResourceBody
  let decode' = decodeByName
        . C8L.fromStrict
        . fromString
        $ body :: Either String (Header, V.Vector Song)
  case decode' of
    Left e -> error (show e)
    Right (_, v) -> return v

djSet :: V.Vector Song -> Compiler [Item Song]
djSet raw = sequence . map makeItem . V.toList $ raw
