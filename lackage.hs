import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

import Data.List
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text

import Codec.Compression.GZip (decompress, compress)
import Codec.Archive.Tar       as Tar hiding (unpack)
import Codec.Archive.Tar.Entry as Tar

import System.IO
import System.Environment
import System.Directory



----------------------------------------------------------------
-- Main
----------------------------------------------------------------

upload :: FilePath -> FilePath -> IO ()
upload tarball dir = do
  -- Read tarball
  bs <- case tarball of
          "-" -> L.getContents
          _   -> L.readFile tarball
  -- Retrieve cabal file
  let cabal = getCabal $ Tar.read $ decompress $ forceBS bs
      descr = getDescription cabal
  -- Build pathes
  let pkg              = package $ packageDescription descr
      PackageName name = pkgName    pkg
      version          = pkgVersion pkg
      -- Pathes
      dirPath   = name ++ "/" ++ display version ++ "/"
      tarDir    = dir ++ "/" ++ dirPath
      tarPath   = tarDir ++ name ++ "-" ++ display version ++ ".tar.gz"
      indexPath = dirPath ++ name ++ ".cabal"
  -- Append file to index
  appendToTar (dir ++ "/" ++ "00-index.tar.gz") indexPath cabal
  -- Write file to directory
  createDirectoryIfMissing True tarDir
  L.writeFile tarPath bs



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["upload", sdist, dir] -> upload sdist dir
    _                      -> error "Unknow command"



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Write compressed tarball
writeTar :: FilePath -> [Entry] -> IO ()
writeTar path = L.writeFile path . compress . Tar.write

-- | Read compressed tarball
readTar :: FilePath -> IO [Entry]
readTar path = do 
  bs <- L.readFile path
  return $ unpackTar $ forceBS bs
--   withFile path ReadMode hreadTar

-- | Read compressed tarball from file handle
hreadTar :: Handle -> IO [Entry]
hreadTar h = (unpackTar . forceBS) `fmap` L.hGetContents h

unpackTar :: L.ByteString -> [Entry]
unpackTar 
  = foldEntries (:) [] (error . show)
  . Tar.read
  . decompress

-- | Append file to the gzip compressed tarball
appendToTar :: FilePath -> FilePath -> L.ByteString -> IO ()
appendToTar tar nm content = do
  -- Create entry for tarball
  let entry = case toTarPath False nm of
                Left  e    -> error e
                Right path -> fileEntry path content
  f <- doesFileExist tar
  case f of
    -- No file.
    False -> writeTar tar [entry]
    -- Append to existing file
    True  -> do
      es <- readTar tar
      -- Check for duplication
      case find ((==nm) . entryPath) es of
        Just _  -> error "Duplicate file in the tarball"
        Nothing -> return ()
      -- Write everything
      writeTar tar (entry : es)



-- | Retrieve cabal file from tarball as bytestring
getCabal :: Show a => Entries a -> L.ByteString
getCabal Done = error "No cabal file"
getCabal (Next entry rest)
  | reverse (take 6 $ reverse $ entryPath entry) == ".cabal" =
      case entryContent entry of
        NormalFile bs _ -> bs
        _  -> error   "Cabal file must be normal file"

  | otherwise                                                = getCabal rest
getCabal (Fail s) = error $ "Invalid tarball: " ++ show s

-- | Extract description from package
getDescription :: L.ByteString -> GenericPackageDescription
getDescription cabal =
  case parsePackageDescription $ unpack $ decodeUtf8With lenientDecode cabal of
    ParseOk _ x -> x
    err         -> error $ "Invalid cabal file: " ++ show err


forceBS :: L.ByteString -> L.ByteString
forceBS lazy = L.fromChunks [bs `seq` bs]
  where
    bs = B.concat $ L.toChunks lazy
