{-# LANGUAGE OverloadedStrings #-}
module YacBuildServer.Git
    ( readFile
    , runCmd
    , GitSource (..)
    , GitURI (..)
    , GitRevOrRef (..)
    )
where

import qualified Data.ByteString as B
import Data.Text.Lazy (Text, unpack, pack)
import Formatting
import Prelude hiding (readFile)
import System.Exit
import System.FilePath.Posix
import System.IO hiding (readFile)
import System.Process

newtype GitURI = GitURI { uri :: Text }
    deriving (Show, Read)

newtype GitRevOrRef = GitRevOrRef { revOrRef :: Text }
    deriving (Show, Read)

data GitSource = GitSource
               { gsUri      :: GitURI
               , gsRevOrRef :: GitRevOrRef
               }
    deriving (Show, Read)

-- | readFile reads FilePath from repository at GitSource
--
-- Implemented by clone uri; checkout ref/rev and then just readFile
--
-- Other approach could be:
--    git archive --remote=<uri> <tree-ish> <filepath>
-- But it's not currently possible as the <tree-ish> is too
-- restricted:
--
--    3. Clients may not use other sha1 expressions, even if the end
--       result is reachable. E.g., neither a relative commit like
--       master^ nor a literal sha1 like abcd1234 is allowed, even if
--       the result is reachable from the refs. [1]_
--
-- .. [1]: man git-upload-archive

readFile
    :: GitSource
    -> FilePath     -- The file to read
    -> FilePath     -- Working directory
    -> IO B.ByteString
readFile (GitSource u r) p wd = do
    _ <- clone u wd (Just "repo")
    _ <- checkout (wd </> "repo") r
    B.readFile (wd </> "repo" </> p)

clone
    :: GitURI
    -> FilePath         -- Directory to clone into
    -> Maybe FilePath   -- Directory name for the repository
    -> IO (String, String)
clone (GitURI u) wd (Just n)  = runGit wd ["clone", u, pack n]
clone (GitURI u) wd (Nothing) = runGit wd ["clone", u]

checkout :: FilePath -> GitRevOrRef -> IO (String, String)
checkout wd (GitRevOrRef r) = runGit wd ["checkout", r]

runGit
    :: FilePath -- working directory
    -> [Text] -- argv
    -> IO (String, String)
runGit wd argv = runCmd (Just wd) "git" $ fmap unpack argv

runCmd
    :: Maybe FilePath       -- cwd
    -> FilePath             -- exe
    -> [String]             -- argv
    -> IO (String, String)  -- out, err
runCmd cwd' exe argv = do
    let cp = (proc exe argv)
            { cwd       = cwd'
            , std_out   = CreatePipe
            , std_err   = CreatePipe
            }
    (_, Just hout, Just herr, ph) <- createProcess cp
    ec  <- waitForProcess ph
    out <- hGetContents hout
    err <- hGetContents herr

    ret ec out err
  where
    ret (f @ (ExitFailure _))  out  err = error . unpack $ format
        ( shown
        % ": " % shown
        % " " % shown
        % " out: " % shown
        % " err: " % shown
        % "\n"
        )
        f exe argv out err
    ret ExitSuccess out err = return (out, err)
