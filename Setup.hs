
import qualified Distribution.Simple as DS
import qualified Distribution.PackageDescription as PD
import Distribution.Simple (UserHooks(..), Args )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Setup ( ConfigFlags, CleanFlags,
                                   configVerbosity, cleanVerbosity, fromFlag )
import Distribution.Simple.Utils ( rawSystemExit )
import System.Environment ( getEnv )
import System.Directory ( getCurrentDirectory )
import Data.List ( intercalate )
import Data.Maybe ( fromJust )



preConf' :: Args -> ConfigFlags -> IO PD.HookedBuildInfo
preConf' args flags = do
    dynetRoot <- getEnv "DYNET"
    eigenInclude <- getEnv "EIGEN3_INCLUDE_DIR"
    let cmd =  ["-c", "cd c && make DYNET="
                   ++ dynetRoot
                   ++ " EIGEN3_INCLUDE_DIR="
                   ++ eigenInclude]
    rawSystemExit (fromFlag $ configVerbosity flags) "/bin/sh" cmd
    preConf DS.simpleUserHooks args flags


confHook' :: (PD.GenericPackageDescription, PD.HookedBuildInfo)
          -> ConfigFlags -> IO LocalBuildInfo
confHook' (description, buildInfo) flags = do
    localBuildInfo <- confHook DS.simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.includeDirs = (dir ++ "/c"):PD.includeDirs libraryBuildInfo,
                    PD.extraLibDirs = (dir ++ "/c"):PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }

postClean' :: Args -> CleanFlags -> PD.PackageDescription -> () -> IO ()
postClean' args flags description _ = do
    rawSystemExit (fromFlag $ cleanVerbosity flags) "/bin/sh" ["-c", "cd c && make clean"]
    postClean DS.simpleUserHooks args flags description ()

main :: IO ()
main = DS.defaultMainWithHooks DS.simpleUserHooks{preConf = preConf'
                                                 ,confHook = confHook'
                                                 ,postClean = postClean'}
