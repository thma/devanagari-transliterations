module IoUtils
  ( writeFileUtf8,
    readFileUtf8,
  )
where

import qualified Data.Text.IO    as TIO
import           Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import           System.IO

writeFileWithEncoding :: TextEncoding -> FilePath -> ShortText -> IO ()
writeFileWithEncoding enc f txt =
  withFile
    f
    WriteMode
    ( \hdl -> do
        hSetEncoding hdl enc
        TIO.hPutStr hdl $ TS.toText txt
    )

readFileWithEncoding :: TextEncoding -> FilePath -> IO ShortText
readFileWithEncoding enc name = do
  inh <- openFile name ReadMode
  hSetEncoding inh enc
  text <- TIO.hGetContents inh
  pure $ TS.fromText text

writeFileUtf8 :: FilePath -> ShortText -> IO ()
writeFileUtf8 = writeFileWithEncoding utf8

readFileUtf8 :: FilePath -> IO ShortText
readFileUtf8 = readFileWithEncoding utf8
