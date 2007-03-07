-- TODO:
-- * if -c option, just check to see what (if anything) is wrong with the
--   current setup.  Possibly (or an alternative of) just display the actions we
--   would have taken
--      -- Issue here is with ID3v1 tag writing
-- * Comment removing seems broken
-- * Remove unnecessary imports

module Main where

import ID3 ( id3v1ToTag, id3v2ToTag, Tag(Tag), artist, album, title, year, genre, track, writeID3v1Tag, writeID3v2Tag, removeID3v1Tag, removeID3v2Tag )
import Control.Monad ( liftM, when )
import Maybe ( fromJust, isJust, fromMaybe )
import Directory ( getDirectoryContents, renameFile, setCurrentDirectory,
                   removeFile )
import Debug.Trace ( trace )
import List ( isSuffixOf )
import Char ( isSpace, isAlpha, isDigit, toUpper, toLower )
import System ( getArgs, system, ExitCode )
import System.IO ( openFile, hClose, IOMode(ReadMode), hGetContents,
                   openTempFile, Handle, hPutStrLn, hFlush, hSeek,
                   SeekMode(AbsoluteSeek) )
import System.Console.GetOpt
import IO ( stdout )

data OptFlag = Check | Verbose |
               Edit | Artist String | Album String | Year String | Genre String
  deriving ( Eq, Show )

options :: [OptDescr OptFlag]
options =
  [ Option ['c'] ["check"] (NoArg Check) "Check directory for conformance"
  , Option ['v'] ["verbose"] (NoArg Verbose) "Display actions to stdout"
  , Option ['e'] ["edit"] (NoArg Edit) "Edit the actions before running them"
  , Option ['a'] ["artist"] (ReqArg Artist "ARTIST") "Specify an artist"
  , Option ['l'] ["album"] (ReqArg Album "ARTIST") "Specify an album"
  , Option ['y'] ["year"] (ReqArg Year "YEAR") "Specify a year"
  , Option ['g'] ["genre"] (ReqArg Genre "GENRENUM") "Specify genre as number"
  ]

data TagType = TagID3v1 | TagID3v2
  deriving ( Eq, Show, Read )

data Mp3File = Mp3File {
    mp3_filename :: FilePath,
    v1Tag :: Maybe Tag,
    v2Tag :: Maybe Tag
} deriving ( Eq, Show, Read )

data Action = Rename FilePath FilePath |
              WriteTag FilePath TagType Tag |
              RemoveTag FilePath TagType |
              Abort | NoAction
  deriving ( Eq, Show, Read )

-- Find all files in dir with the extension of_type
getFiles :: FilePath -> FilePath -> IO [FilePath]
getFiles of_type dir = filter has_suffix `liftM` files
  where has_suffix = isSuffixOf of_type
        files = getDirectoryContents dir

-- Convert a directory of mp3s to list of Mp3File instances
getMp3Files :: FilePath -> IO [Mp3File]
getMp3Files dir = (getFiles ".mp3" dir) >>= mapM mp3ToMp3File_
  where mp3ToMp3File_ :: FilePath -> IO Mp3File
        mp3ToMp3File_ path = do
            v1 <- id3v1ToTag path
            v2 <- id3v2ToTag path
            return Mp3File { mp3_filename = path, v1Tag = v1, v2Tag = v2 }

hasID3v2 :: Mp3File -> Bool
hasID3v2 file = isJust $ v2Tag file
hasID3v1 :: Mp3File -> Bool
hasID3v1 file = isJust $ v1Tag file
hasDecentFilename :: Mp3File -> Bool
-- TODO: implement this
hasDecentFilename file = False -- for now

titleExceptions :: [String]
titleExceptions = [ "the", "a", "an", "it", "the", "in", "and", "or", "to",
                    "nor", "but", "is", "isnt", "of" ]

substTable :: [(String, String)]
substTable = [
                ("&", "and")
             ]

-- Chicago Manual of Style says to always capitalize the last word
--   and to lowercase all articles, coordinate conjunctions, and prepositions
-- http://answers.google.com/answers/threadview?id=349913
-- TODO: clean this up, separate it a bit
tagToFilename :: Tag -> FilePath
tagToFilename tag = trackNum ++ "-" ++ (cleanup $ title tag) ++ ".mp3"
    where cleanup = removeExtra . titleCase . fixCases . downCase . substWords . removePunc . removeParens
          removeParens str = (takeToParens str) ++ (dropUntilParens str)
          takeToParens = takeWhile (/='(')
          dropUntilParens = drop 1 . dropWhile (/=')')
          removePunc = filter (\x -> (isSpace x || isAlpha x || isDigit x))
          removeExtra = map mapChars
          fixCases = id
          titleCase x = unwords $ (fixCase $ head $ words x):(tail $ titleCaseSub x)
          titleCaseSub w = map matchOrNot $ words w
          substWords w = unwords $ map matchReplace (words w)
          matchReplace w = maybe w id (lookup w substTable)
          matchOrNot w = if w `elem` titleExceptions then w else fixCase w
          downCase = map toLower
          mapChars x | isAlpha x = x
                     | isDigit x = x
                     | otherwise = '_'
          trackNum = if length trackStr == 1 then '0':trackStr else trackStr
          trackStr = show $ track tag

fixCase [] = []
fixCase (c:cs) = toUpper c:map toLower cs

-- TODO: implement this
filenameToTag :: FilePath -> Tag
filenameToTag file = Tag {
    artist = "ArtistF", album = "AlbumF", title = "SongF",
    year = 1900, genre = 255, track = 0
}

getRenameAction :: Mp3File -> Action
getRenameAction file
    | hasID3v2 file          = Rename filename v2_filename
    | hasDecentFilename file = Rename filename sanitized_filename
    | hasID3v1 file          = Rename filename v1_filename
    | otherwise              = error "No file actions available"
  where filename = mp3_filename file
        v1_filename = tagToFilename $ fromJust $ v1Tag file
        sanitized_filename = sanitizeFilename $ mp3_filename file
        v2_filename = tagToFilename $ fromJust $ v2Tag file

getTagAction :: Mp3File -> Action
getTagAction file
    -- rewrite ID3v1 tag so we can easily sanitize it later
    | hasID3v1 file          = WriteTag filename TagID3v1 v1_tag
    | hasID3v2 file          = WriteTag filename TagID3v1 v2_tag
    | hasDecentFilename file = WriteTag filename TagID3v1 fToTag
    | otherwise              = error "No tag actions available"
  where filename = mp3_filename file
        v1_tag = fromJust $ v1Tag file
        v2_tag = fromJust $ v2Tag file
        fToTag = filenameToTag filename

getFileActions :: Mp3File -> [Action]
getFileActions file = [getTagAction file, remove_act, getRenameAction file]
    where remove_act = if hasID3v2 file
                         then RemoveTag (mp3_filename file) TagID3v2
                         else NoAction

-- Remove punctuation, etc
-- XXX: this may not be necessary, considering that we were the ones who
-- originally created the filename
sanitizeFilename :: FilePath -> FilePath
sanitizeFilename file = file

-- TODO: Can we simplify these repeated definitions?
getArtist :: [OptFlag] -> Maybe String
getArtist ((Artist str):_) = Just str
getArtist (_:opts) = getArtist opts
getArtist [] = Nothing

getAlbum :: [OptFlag] -> Maybe String
getAlbum ((Album str):_) = Just str
getAlbum (_:opts) = getAlbum opts
getAlbum [] = Nothing

getYear :: [OptFlag] -> Maybe Integer
getYear ((Year yr):_) = Just $ read yr
getYear (_:opts) = getYear opts
getYear [] = Nothing

getGenre :: [OptFlag] -> Maybe Integer
getGenre ((Genre gn):_) = Just $ read gn
getGenre (_:opts) = getGenre opts
getGenre [] = Nothing

-- If each given option was specified, modify the tag.  Otherwise, leave the
-- tag alone
artistMod,albumMod,yearMod,genreMod :: [OptFlag] -> Tag -> Tag
artistMod opts tag = maybe tag (\x -> tag { artist = x }) $ getArtist opts
albumMod opts tag = maybe tag (\x -> tag { album = x }) $ getAlbum opts
yearMod opts tag = maybe tag (\x -> tag { year = x }) $ getYear opts
genreMod opts tag = maybe tag (\x -> tag { genre = x }) $ getGenre opts

-- TODO: Using something like this would be a bit nicer
--tagMods :: [([OptFlag], Tag) -> Tag]
--tagMods =
--        [ \(opts,tag) -> maybe tag (\x -> tag { artist = x }) $ getArtist opts
--        , \(opts,tag) -> maybe tag (\x -> tag { album = x }) $ getAlbum opts
--        , \opts tag -> maybe tag (\x -> tag { year = x }) $ getYear opts
--        ]

-- Apply the above modifications to the passed in tag
sanitizeTag :: [OptFlag] -> Tag -> Tag
sanitizeTag opts tag = applyMods [artistMod opts, albumMod opts, yearMod opts,
                                  genreMod opts]
    where applyMods mods = foldr (.) id mods tag

sanitizeAction :: [OptFlag] -> Action -> Action
sanitizeAction opts (WriteTag file TagID3v1 tag) = WriteTag file TagID3v1 $ sanitizeTag opts tag
sanitizeAction opts a@(Rename f1 f2) = if f1 == f2 then NoAction else a
sanitizeAction opts tag = tag
-- we generated the rename filenames, so they should always be good.
-- if this isn't the case, we can sanitize it here

handleAction :: Action -> IO ()
handleAction (Rename from to) = renameFile_ from to
handleAction (WriteTag name TagID3v1 tag) = putStrLn "writeID3v1Tag" >> writeID3v1Tag name tag
handleAction (WriteTag name TagID3v2 tag) = putStrLn "writeID3v2Tag" >> writeID3v2Tag name tag
handleAction (RemoveTag name TagID3v1) = putStrLn "removeID3v1Tag" >> removeID3v1Tag name
handleAction (RemoveTag name TagID3v2) = putStrLn "removeID3v2Tag" >> removeID3v2Tag name
handleAction (Abort) = putStrLn "abort" >> error "User requested abort"
handleAction (NoAction) = putStrLn "no action"

renameFile_ :: FilePath -> FilePath -> IO ()
renameFile_ from to = do
    putStrLn $ "renaming " ++ from ++ " to " ++ to
    renameFile from to

writeActions :: Handle -> [Action] -> IO ()
writeActions h actions = mapM_ (hPutStrLn h . show) actions

readActions :: Handle -> IO [Action]
readActions handle = do
    whole <- hGetContents handle
    putStrLn whole
    actions <- mapM (return . read) $ lines whole
    return actions

editFile :: FilePath -> IO ExitCode
editFile file = system $ "vim " ++ file

editActions :: [Action] -> IO [Action]
editActions actions = do
    (path,handle) <- openTempFile "/tmp" "MRXXXXXX"
    writeActions handle actions
    hFlush handle
    editFile path
    hSeek handle AbsoluteSeek 0
    acts <- readActions handle
    hClose handle >> removeFile path
    return acts

parseArgs :: [String] -> IO ([OptFlag], [FilePath])
parseArgs args = do
  case getOpt Permute options args of
       (o,n,[])   -> return (o,n)
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: musicrenamer [OPTION...] files..."

default_dir :: FilePath
default_dir = "/tmp/test-id3v2" -- TODO: change this to "."

processDir :: [OptFlag] -> FilePath -> IO ()
processDir opts dir = do
    setCurrentDirectory dir
    files <- getMp3Files dir
    finalActions <- getFinalActions files
    when (Verbose `elem` opts) $ writeActions IO.stdout finalActions
    handle finalActions
  where handle = if Check `elem` opts && not (Verbose `elem` opts)
                   then writeActions IO.stdout
                   else mapM_ handleAction
        sanitize = map (sanitizeAction opts)
        getFinalActions f = if Edit `elem` opts
                              then editActions $ actions f
                              else return $ actions f
        actions f = collapse . sanitize $ concatMap getFileActions f
        collapse = filter (/= NoAction)

main = do
    args <- getArgs
    (opts,rest) <- parseArgs args
    mapM_ (processDir opts) (getDirs rest)
  where getDirs :: [FilePath] -> [FilePath]
        getDirs dirs | length dirs == 0 = [default_dir]
                     | otherwise        = dirs
