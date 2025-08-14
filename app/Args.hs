module Args (FuseTorrentArgs (..), parseArgs, execParser, programInfo, toFuseArgs) where

import Options.Applicative

data FuseTorrentArgs
  = FuseTorrentArgs
      { mountpoint :: FilePath,
        stateDir :: Maybe FilePath,
        mountOptions :: Maybe String,
        debug :: Bool,
        foreground :: Bool,
        singleThread :: Bool,
        lostAndFound :: Bool,
        nonBlockTimeout :: Word
      }
  | FuseVersion
  deriving (Show)

toFuseArgs :: FuseTorrentArgs -> [String]
toFuseArgs FuseTorrentArgs {mountpoint = m, mountOptions = mo, debug = d, foreground = f, singleThread = s} =
  attach s "-s" . attach f "-f" . attach d "-d" . (m :) $ case mo of
    Just o -> ["-o", o]
    Nothing -> []
  where
    attach t v =
      if (t)
        then (v :)
        else id
toFuseArgs FuseVersion = undefined

parseArgs' :: Parser FuseTorrentArgs
parseArgs' =
  FuseTorrentArgs
    <$> strArgument (help "Mountpoint" <> action "directory" <> metavar "Mountpoint")
    <*> optional (strArgument (action "directory" <> help "Directory where torrents and their data will be stored. By default this will be the same as Mountpoint." <> metavar "State"))
    <*> optional (strOption (long "mount-options" <> short 'o' <> help "Options forwarded to mount -o"))
    <*> switch (long "debug" <> short 'd' <> help "Print debug information. Implies -f.")
    <*> switch (long "foreground" <> short 'f' <> help "Do not fork before running the mount")
    <*> switch (long "single-thread" <> short 's' <> help "Single threaded fuse operations")
    <*> switch (long "lost-found" <> short 'l' <> help "Show a lost+found directory with files that have been removed")
    <*> option auto (long "non-block-timeout" <> short 'b' <> value 10000 <> showDefault <> help "How long a read operation may block before it returns WOULDBLOCK on non-blocking file handles")

parseFuseVersion :: Parser FuseTorrentArgs
parseFuseVersion = flag' FuseVersion (long "fuse-version" <> help "Get Fuse version information")

parseArgs :: Parser FuseTorrentArgs
parseArgs = parseArgs' <|> parseFuseVersion

programInfo :: ParserInfo FuseTorrentArgs
programInfo =
  info
    (parseArgs <**> helper)
    ( fullDesc
        <> progDesc "Mount a directory with a BitTorrent backend"
        <> header "Hi"
    )
