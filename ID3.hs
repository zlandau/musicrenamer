-- This module is a horrible, hackish mess.  It is intended as a (hopefully
-- very) temporary placeholder until a proper binding to an ID3 lib or a
-- proper ID3 parser can be made.  It is error prone and not tested well
-- enough.  Don't use it.  It will eat your children.
module ID3 ( id3v1ToTag, id3v2ToTag, Tag(Tag), artist, album, title, year, genre, track, writeID3v1Tag, writeID3v2Tag, removeID3v1Tag, removeID3v2Tag ) where

import Control.Monad ( liftM )
import Maybe ( fromJust )
import List ( find, isPrefixOf )
import System ( system )
import System.Process ( runInteractiveCommand )
import IO

data Tag = Tag {
    artist :: String,
    album :: String,
    title :: String,
    year :: Integer,
    genre :: Integer,
    track :: Integer
} deriving ( Eq, Show, Read )

squeeze = unwords . words

parseGenre str = read $ takeWhile (/=')') $ drop 1 $ dropWhile (/='(') str

parseTrack str = read $ takeWhile (/='/') str

fieldLookup :: String -> String -> String -> [(String, String)] -> String
fieldLookup name1 name2 dfl fields = fromJust $
    case lookup name1 fields of
      Just a -> Just a
      Nothing -> case lookup name2 fields of
                   Just a -> Just a
                   Nothing -> Just dfl

parseID3v2 :: String -> Tag
parseID3v2 text = tag
  where tag = Tag {
            artist = fieldLookup "TP1" "TPE1" "Unknown" parts,
            album = fieldLookup "TAL" "TALB" "Unknown" parts,
            title = fieldLookup "TT2" "TIT2" "Unknown" parts,
            year = read $ fieldLookup "TYE" "TYER" "1900" parts,
            genre = parseGenre $ fieldLookup "TCO" "TCON" "255" parts,
            track = parseTrack $ fieldLookup "TRCK" "TRK" "0" parts
        }
        splitParts x = (takeWhile (/=' ') x, drop 2 $ dropWhile (/=':') x)
        parts = map splitParts $ lines text

parseID3v1 :: String -> Tag
parseID3v1 text = tag
    where tag = Tag {
              artist = art,
              album = alb,
              title = titl,
              track = trck,
              year = yr,
              genre = gen
          }
          titl = squeeze $ take 32 $ drop 9 $ parts !! 0
          art = squeeze $ take 32 $ drop 49 $ parts !! 0
          alb = squeeze $ take 32 $ drop 9 $ parts !! 1
          trck = read $ drop 48 $ parts !! 2
          yr = read $ take 4 $ drop 47 $ parts !! 1
          --parts = tail $ lines text
          parts = lines text
          gen = parseGenre $ drop 60 $ parts !! 1

id3ToTag :: (String -> Tag) -> String -> FilePath -> IO (Maybe Tag)
id3ToTag parser tag path = do
    (_,fd,_,_) <- runInteractiveCommand $ "id3v2 -l \"" ++ path ++ "\""
    contents <- readBlock fd
    case contents of
        Just content -> do
           c <- content
           return $ Just $ parser c
        Nothing -> return Nothing
  where readBlock f = do
          eof <- hIsEOF f
          if eof
             then do return Nothing
             else do
                line <- hGetLine f
                if isPrefixOf tag line
                   then return $ Just $ hGetContents f
                   else readBlock f

id3v2Cmd :: FilePath -> Tag -> String -> String
id3v2Cmd path tag tagver =
    "id3v2 -" ++ tagver ++ " -a \"" ++ (artist tag) ++ "\"" ++
    " -A \"" ++ (album tag) ++ "\"" ++
    " -t \"" ++ (title tag) ++ "\"" ++
    " -g " ++ (show $ genre tag) ++
    " -y " ++ (show $ year tag) ++
    " -T " ++ (show $ track tag) ++
    " -c ''" ++
    " \"" ++ path ++ "\""

-- XXX: Restores the comment, weird
writeID3v1Tag :: FilePath -> Tag -> IO ()
writeID3v1Tag path tag = do
    system $ id3v2Cmd path tag "1"
    -- For some reason, id3v2 won't remove the comment.  Use mp3info for that:
    system $ "mp3info -c '' \"" ++ path ++ "\""
    return ()

removeID3v1Tag :: FilePath -> IO ()
removeID3v1Tag path = do
    system $ "id3v2 -s \"" ++ path ++ "\""
    return ()

writeID3v2Tag :: FilePath -> Tag -> IO ()
writeID3v2Tag path tag = do
    system $ id3v2Cmd path tag "2"
    return ()

removeID3v2Tag :: FilePath -> IO ()
removeID3v2Tag path = do
    system $ "id3v2 -d \"" ++ path ++ "\""
    return ()

id3v1ToTag = id3ToTag parseID3v1 "id3v1 tag info for"
id3v2ToTag = id3ToTag parseID3v2 "id3v2 tag info for"
