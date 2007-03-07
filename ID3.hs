-- This module is a horrible, hackish mess.  It is intended as a (hopefully
-- very) temporary placeholder until a proper binding to an ID3 lib or a
-- proper ID3 parser can be made.  It is error prone and not tested well
-- enough.  Don't use it.  It will eat your children.
module ID3 ( id3v1ToTag, id3v2ToTag, Tag(Tag), artist, album, title, year, genre, track, writeID3v1Tag, writeID3v2Tag, removeID3v1Tag, removeID3v2Tag ) where

import Control.Monad ( liftM )
import Maybe ( fromJust )
import List ( find, isPrefixOf )
import System ( system )
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

parseID3v2 :: String -> Tag
parseID3v2 text = tag
  where tag = Tag {
            artist = fromJust $ case lookup "TP1" parts of
                                  Just a -> Just a
                                  Nothing -> lookup "TPE1" parts,
            album = fromJust $ case lookup "TAL" parts of
                                  Just a -> Just a
                                  Nothing -> lookup "TALB" parts,
            title = fromJust $ case lookup "TT2" parts of
                                  Just a -> Just a
                                  Nothing -> lookup "TIT2" parts,
            year = read $ fromJust $ case lookup "TYE" parts of
                                       Just a -> Just a
                                       Nothing -> lookup "TYER" parts,
            genre = parseGenre gen_full,
            track = read $ fromJust $ case lookup "TRCK" parts of
                                       Just a -> Just a
                                       Nothing -> parseTrack
        }
        gen_full = fromJust $ case lookup "TCO" parts of
                                Just a -> Just a
                                Nothing -> lookup "TCON" parts
        splitParts x = (takeWhile (/=' ') x, drop 2 $ dropWhile (/=':') x)
        parts = map splitParts $ lines text
        parseTrack = Just $ takeWhile (/='/') $ fromJust $ lookup "TRK" parts

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

-- runInteractiveCommand / runInteractiveProcess
--id3v1ToTag :: FilePath -> IO (Maybe Tag)
id3ToTag :: (String -> Tag) -> String -> FilePath -> IO (Maybe Tag)
--id3v1ToTag path = do
id3ToTag parser tag path = do
    ret <- system $ "id3v2 -l \"" ++ path ++ "\" >" ++ tmpfile
    fd <- openFile tmpfile ReadMode
    contents <- readBlock fd
    case contents of
        Just content -> do
           c <- content
           return $ Just $ parser c
        Nothing -> return Nothing
  where tmpfile = "/tmp/parse_id3v1.tmp"
        readBlock f = do
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
