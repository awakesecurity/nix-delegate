{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

{-| This modules provides a basic library API to @nix-delegate@'s functionality
-}

module Nix.Delegate
    ( -- * Options
      OptArgs(..)
    , Command(..)
    , OperatingSystem(..)

      -- * Commands
    , delegate
    , delegateStream
    , main
    ) where

import           Control.Applicative       (empty, many, (<**>), (<|>))
import           Control.Exception         (SomeException)
import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Managed     (MonadManaged)
import qualified Data.Foldable             as Foldable
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Prelude                   hiding (FilePath)
import           Turtle                    (ExitCode (..), FilePath, Line,
                                            Shell, d, fp, liftIO, s, (%), (</>))

import qualified Control.Exception
import qualified Control.Foldl             as Foldl
import qualified Data.Text
import qualified NeatInterpolation
import qualified Options.Applicative       as Options
import qualified Options.Applicative.Types as Options
import qualified Turtle
import qualified Turtle.Line

-- | @delegate@ options
data OptArgs = OptArgs
    { host    :: Text
    -- ^ Build host to add
    , os      :: [OperatingSystem]
    -- ^ Supported platform types (Default: @x86_64-linux@)
    , key     :: Maybe FilePath
    -- ^ SSH private key used to log in to build host (Default: @~/.ssh/id_rsa@)
    , cores   :: Maybe Integer
    -- ^ Number of cores available on the build host (Default: @1@)
    , feature :: [Text]
    -- ^ Supported system features for the build host
    , cmd     :: Command
    -- ^ Command to run with distributed builds enabled
    } deriving (Show)

-- | Operating system
data OperatingSystem
    = X86_64_Linux
    | X86_64_Darwin
    deriving (Show)

parseOptions :: Options.Parser OptArgs
parseOptions =
  OptArgs
  <$> parseHost
  <*> many parseOS
  <*> parseKey
  <*> parseCores
  <*> many parseFeature
  <*> parseCommand

parseHost :: Options.Parser Text
parseHost = Data.Text.pack <$>
  (Options.option Options.str $
   ( Options.long "host"
  <> Options.help "Machine to use as a build slave"
   )
  )

parseKey :: Options.Parser (Maybe FilePath)
parseKey =
  (Options.optional $  (Turtle.fromText . Data.Text.pack) <$>
   (Options.option Options.str $
    ( Options.long "key"
   <> Options.help "Path to SSH private key (Default: ~/.ssh/id_rsa)"
    )
   )
  )

parseCores :: Options.Parser (Maybe Integer)
parseCores =
  (Options.optional
   (Options.option ( Options.auto) $
    ( Options.long "cores"
   <> Options.help "Number of cores to use (Default: 1)"
    )
   )
  )

parseFeature :: Options.Parser Text
parseFeature = Data.Text.pack <$>
  (Options.option Options.str $
   ( Options.long "feature"
  <> Options.help "Supported system features"
   )
  )

parseOS :: Options.Parser OperatingSystem
parseOS =
      Options.flag' X86_64_Linux  (Options.long "x86_64-linux" )
  <|> Options.flag' X86_64_Darwin (Options.long "x86_64-darwin")

renderOS :: OperatingSystem -> Text
renderOS X86_64_Linux  = "x86_64-linux"
renderOS X86_64_Darwin = "x86_64-darwin"

-- | Command to run
data Command = Command Text [Text]
  deriving (Show)

parseCommand :: Options.Parser Command
parseCommand = parseCmd <*> many (Data.Text.pack <$> Options.strArgument (Options.metavar "ARGS"))
  where
    cmdP :: Options.ReadM ([Text] -> Command)
    cmdP = Command . Data.Text.pack <$> Options.readerAsk

    parseCmd =
      (Options.argument cmdP $
        ( Options.metavar "COMMAND"
       <> Options.help    "Command to delegate (if 'nix-build', sudo will be used if $NIX_REMOTE=daemon)"
        )
      )

renderCmd :: Command -> Text
renderCmd (Command cmd args) = Turtle.format (s%" "%s) cmd (Data.Text.intercalate " " args)

canSudo :: Command -> Bool
canSudo (Command command _) = Turtle.filename path == "nix-build"
  where
    path = Turtle.fromText command

-- | @main@ used by the @delegate@ executable
main :: IO ()
main = do
    options <- do
        Options.execParser
            (Options.info (parseOptions <**> Options.helper)
                (   Options.fullDesc
                <>  Options.progDesc "Run a subcommand with distributed builds transiently enabled"
                <>  Options.noIntersperse
                )
            )
    delegate options

exchangeKeys :: FilePath -> Text -> IO ()
exchangeKeys key host = do
  let key' = Turtle.format fp key

  -- When performing a distributed build you need to share a key pair
  -- (both the public and private key) with the machine you're
  -- deploying to (or from). Both machines must store the same private
  -- key at `/etc/nix/signing-key.sec` and the same public key at
  -- `/etc/nix/signing-key.pub`. The private must also be only
  -- user-readable and not group- or world-readable (i.e. `400`
  -- permissions using `chmod` notation).
  --
  -- By default, neither machine will have a key pair installed.  This script
  -- will first ensure that the remote machine has a key pair (creating one if
  -- if missing) and copy the remote key pair to the local machine.  We
  -- install the remote key pair locally on every run of this script because we
  -- do not assume that all remote machines share the same key pair.  Quite the
  -- opposite: every production machine should have a unique signing key pair.
  let privateKey = "/etc/nix/signing-key.sec"
  let publicKey  = "/etc/nix/signing-key.pub"

  let handler0 :: SomeException -> IO ()
      handler0 e = do
          let exceptionText = Data.Text.pack (show e)
          let msg           = [NeatInterpolation.text|
[x] Could not ensure that the remote machine has signing keys installed

    Debugging tips:

    1. Check if you can log into the remote machine by running:

        $ ssh -i $key' $host

    2. If you can log in, then check if you have permission to `sudo` without a
       password by running the following command on the remote machine:

        $ sudo -n true
        $ echo $?
        0

    Original error: $exceptionText
|]
          Turtle.die msg

  let openssl :: Turtle.Format a a
      openssl =
          "$(nix-build --no-out-link \"<nixpkgs>\" -A libressl)/bin/openssl"
  let fmt = "ssh -i "%fp%" "%s%" '"
              % "test -e "%fp%" || "
              % "sudo sh -c \""
                  % "(umask 277 && "%openssl%" genrsa -out "%fp%" 2048) && "
                  % openssl%" rsa -in "%fp%" -pubout > "%fp
              % "\""
          % "'"
  let cmd = Turtle.format fmt key host privateKey privateKey privateKey publicKey
  Control.Exception.handle handler0 (Turtle.shells cmd empty)

  let mirror path = Turtle.runManaged $ do
          let message = Turtle.format ("[+] Downloading: "%fp) path
          mapM_ Turtle.err (Turtle.Line.textToLines message)

          localPath <- Turtle.mktempfile "/tmp" "signing-key"
          let download =
                  Turtle.procs "rsync"
                      [ "--archive"
                      , "--checksum"
                      , "--rsh", Turtle.format ("ssh -i "%fp) key
                      , "--rsync-path", "sudo rsync"
                      , Turtle.format (s%":"%fp) host path
                      , Turtle.format fp localPath
                      ]
                      empty
          let handler1 :: SomeException -> IO ()
              handler1 e = do
                  let pathText      = Turtle.format fp path
                  let exceptionText = Data.Text.pack (show e)
                  let msg           = [NeatInterpolation.text|
[x] Could not download: $pathText

    Debugging tips:

    1. Check if you can log into the remote machine by running:

        $ ssh -i $key' $host

    2. If you can log in, then check if you have permission to `sudo` without a
       password by running the following command on the remote machine:

        $ sudo -n true
        $ echo $?
        0

    3. If you can `sudo` without a password, then check if the file exists by
       running the following command on the remote machine:

        $ test -e $pathText
        $ echo $?
        0

    Original error: $exceptionText
|]
                  Turtle.die msg

          liftIO (Control.Exception.handle handler1 download)

          -- NB: path shouldn't is a FilePath and won't have any
          -- newlines, so this should be okay
          Turtle.err (Turtle.unsafeTextToLine $ Turtle.format ("[+] Installing: "%fp) path)

          warnSudo

          let install =
                  Turtle.procs "sudo"
                      [ "mv"
                      , Turtle.format fp localPath
                      , Turtle.format fp path
                      ]
                      empty
          let handler2 :: SomeException -> IO ()
              handler2 e = do
                  let pathText      = Turtle.format fp path
                  let exceptionText = Data.Text.pack (show e)
                  let msg           = [NeatInterpolation.text|
[x] Could not install: $pathText

    Debugging tips:

    1. Check to see that you have permission to `sudo` by running:

        $ sudo true
        $ echo $?
        0

    Original error: $exceptionText
|]
                  Turtle.die msg

          liftIO (Control.Exception.handle handler2 install)

  mirror privateKey
  mirror publicKey

delegateShared
    :: MonadManaged managed => OptArgs -> managed (Text, SomeException -> IO a)
delegateShared OptArgs{..}  = do
    home <- Turtle.home
    let key'      = fromMaybe (home </> ".ssh/id_rsa") key
    let os'       = case os of [] -> [X86_64_Linux]; _ -> os
    let os''      = Data.Text.intercalate "," (Foldable.toList (fmap renderOS os'))
    let feature'  = Data.Text.intercalate "," feature
    let cores'    = fromMaybe 1 cores

    isDaemon <- maybe False (== "daemon") <$> Turtle.need "NIX_REMOTE"

    let sudo | isDaemon && canSudo cmd = "sudo"
             | otherwise               = ""

    host' <-
      if isDaemon && not (Data.Text.any (== '@') host)
      then do
        mUser <- Turtle.need "USER"
        case mUser of
            Nothing   -> Turtle.die [NeatInterpolation.text|
[x] You must set the `USER` environment variable in order for `nix-delegate` to
    work in a multi-user Nix installation
|]
            Just user -> return (user <> "@" <> host)
      else return host

    {-| Do a test @ssh@ command in order to prompt the user to recognize the
        host if the host is not known

        Use @sudo@ if we are in multi-user mode since the @root@ user will be
        initiating the build and therefore the @root@ user needs to authorize
        the known host
    -}
    Turtle.err "[+] Testing SSH access"
    if sudo == "sudo" then warnSudo else return ()
    let testSSH = s%" ssh -i "%fp%" "%s%" :"
    Turtle.shells (Turtle.format testSSH sudo key' host') Turtle.stdin

    liftIO (exchangeKeys key' host')

    let debuggingTips = [NeatInterpolation.text|
    Debugging tips:

    1.  Make sure that you have installed Nix:

        $ nix-build --version

    2.  Make sure that you log into a new shell after installing Nix
|]

    remoteSystemsFile <- Turtle.mktempfile "/tmp" "remote-systems.conf"
    let line =
            Turtle.format
                (s%" "%s%" "%fp%" "%d%" 1 "%s)
                host'
                os''
                key'
                cores'
                feature'

    case Turtle.textToLine line of
      Just line' ->
        Turtle.output remoteSystemsFile (pure line')
      Nothing ->
        Turtle.die [NeatInterpolation.text|
[x] The generated 'remote-systems.conf' file content contains a newline (it should not)

    $line
|]

    loadDirectory <- Turtle.mktempdir "/tmp" "build-remote-load"

    mNixPath <- Turtle.need "NIX_PATH"
    nixPath  <- case mNixPath of
            Just nixPath -> return nixPath
            Nothing      -> Turtle.die [NeatInterpolation.text|
[x] Your NIX_PATH environment variable is unset

    $debuggingTips
|]

    let configFile = home </> ".ssh/config"

    configExists <- Turtle.testfile configFile
    let sshConfigFile
          | configExists = Turtle.format ("ssh-config-file="%fp%":") configFile
          | otherwise    = ""

    mAuthSock <- Turtle.need "SSH_AUTH_SOCK"
    let sshAuthSock = maybe "" (Turtle.format ("ssh-auth-sock="%s%":")) mAuthSock
    let nixpkgpath  = Turtle.inproc "nix-build" [ "--no-out-link", "--realise", "<nixpkgs>", "--attr", "nix" ] empty

    hook <- Turtle.fold nixpkgpath Foldl.head >>= \case
      Just nixpkgpath' -> do
        let nixpkgfp = Turtle.fromText $ Turtle.lineToText nixpkgpath'
        return $ Turtle.format fp (nixpkgfp </> "libexec/nix/build-remote.pl")
      Nothing ->
        Turtle.die [NeatInterpolation.text|
[x] The 'build-remote.pl' script could not be found on your system!

    $debuggingTips
|]

    let renderedCmd = renderCmd cmd
    let pfxcmd = Turtle.format
          (s %
           " NIX_BUILD_HOOK="%s%
           " NIX_PATH="%s%
           " NIX_REMOTE_SYSTEMS="%s%
           " NIX_CURRENT_LOAD="%s%" "%s)
          sudo
          hook
          (sshConfigFile <> sshAuthSock <> nixPath)
          (Turtle.format fp remoteSystemsFile)
          (Turtle.format fp loadDirectory)
          renderedCmd

    let handler2 :: SomeException -> IO a
        handler2 e = do
            let exceptionText = Data.Text.pack (show e)
            let msg           = [NeatInterpolation.text|
[x] The subcommand you specified exited with a non-zero exit code:

    Original error: $exceptionText
|]
            Turtle.die msg

    -- NB: path shouldn't is a FilePath and won't have any
    -- newlines, so this should be okay
    Turtle.err (Turtle.unsafeTextToLine $ Turtle.format ("[+] Running command: "%s%" "%s) sudo renderedCmd)
    Turtle.err (Turtle.unsafeTextToLine $ Turtle.format ("[+] Full command context: "%s) pfxcmd)
    return (pfxcmd, handler2)

{-| Run a command with distributed builds transiently enabled

    This version outputs a helpful error message if the command fails
-}
delegate :: OptArgs -> IO ()
delegate options = Turtle.runManaged $ do
    (command, handler) <- delegateShared options
    let build = Turtle.shells command empty
    liftIO (Control.Exception.handle handler build)

{-| Run a command with distributed builds transiently enabled

    This version captures the output as a stream
-}
delegateStream :: OptArgs -> Shell Line
delegateStream options = do
    (command, _) <- delegateShared options
    Turtle.inshell command empty

warnSudo :: MonadIO io => io ()
warnSudo = do
    exitCode <- Turtle.shell "sudo -n true 2>/dev/null" empty

    case exitCode of
        ExitFailure _ -> do
            Turtle.err ""
            Turtle.err "    This will prompt you for your `sudo` password"
        _             -> do
            return ()
