-- | Provide high-level functions to build Haskell-project using some docker image
module System.Build(BuildTarget(..), stackInDocker) where

import           Data.Functor
import           System.Directory
import           System.Docker
import           System.IO
import           System.IO.Extra
import           System.Process

data BuildTarget = SimpleTarget String
                 | FullTarget String String

asStackArg :: BuildTarget -> String
asStackArg (SimpleTarget t)    = "exe:" ++ t
asStackArg (FullTarget pref t) = pref ++ ":exe:" ++ t

asBinaryName :: BuildTarget -> String
asBinaryName (SimpleTarget t)    = t
asBinaryName (FullTarget pref t) = t


-- | Build a Haskell project using some docker image.
--
-- In order to maximize reuse, this process creates in the current directory a file called `.cidfile` which contains
-- the id of the latest container that ran the build. When this file exists, the next run will reuse the volumes of
-- the previous run which means built dependencies will normally be available.
--
-- The built target, which is assumed to be a binary executable, is then extracted from the container and copied
-- locally in a file called `targetName`.
--
-- TODO: run with current user in the container or reuse stack's docker capabilities
stackInDocker :: ImageName -> FilePath -> BuildTarget -> IO FilePath
stackInDocker img@(ImageName imgName) srcDir buildTarget = do
  absSrcDir <- canonicalizePath srcDir
  buildAlreadyRun <- doesFileExist ".cidfile"
  if buildAlreadyRun
    then do
    cid <- readFile ".cidfile"
    removeFile ".cidfile"
    callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "--volumes-from=" ++ cid,
                          "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user", asStackArg buildTarget ]
    else callProcess "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build",
                               "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user", asStackArg buildTarget ]

  exportBinary img (asBinaryName buildTarget)


exportBinary :: ImageName -> String -> IO FilePath
exportBinary (ImageName imgName) targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm", "--volumes-from=" ++ cid,  "-w", "/build", imgName, "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--rm", "--volumes-from=" ++ cid, "busybox","dd", "if=" ++ stackRoot ++ "/bin/" ++ targetName ]) { std_out = CreatePipe }
  withBinaryFile targetName WriteMode $ \ hDst -> copy hout hDst
  void $ waitForProcess phdl
  return targetName
