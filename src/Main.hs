{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main (main) where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Maybe
import           Data.Version                 (showVersion)
import           Network.HTTP.Conduit
import           Options.Declarative
import           System.IO
import qualified Text.PrettyPrint.Leijen.Text as PP
import           Text.Regex.TDFA

import           Paths_html2hamlet            (version)
import           Text.Hamlet.Html2Hamlet

main :: IO ()
main = run "html2hamlet" (Just $ showVersion version) cmd

cmd :: Arg "FILES/URLS..." [String]
    -> Cmd "HTML to Hamlet converter" ()
cmd (get -> []) = liftIO $
  writeHamlet L.getContents PP.putDoc
cmd (get -> files) = do
  logger <- getLogger
  liftIO $ forM_ files $ \file -> do
    if file =~ ("^https?://" :: String)
      then do
      writeHamlet (simpleHttp file) $ \doc -> do
        let saveName = changeSuffix $ httpFileName file
        logger 1 $ "Convert " ++ show file ++ " to " ++ show saveName
        withFile saveName WriteMode (`PP.hPutDoc` doc)
      else do
      writeHamlet (L.readFile file) $ \doc -> do
        let saveName = changeSuffix file
        logger 1 $ "Convert " ++ show file ++ " to " ++ show saveName
        withFile saveName WriteMode (`PP.hPutDoc` doc)

writeHamlet :: IO L.ByteString -> (PP.Doc -> IO ()) -> IO ()
writeHamlet reader writer =
  writer . convert2Doc =<< reader

httpFileName :: String -> String
httpFileName url = fromMaybe "index.html" $ do
  [_, _, f, _, _, _] <- listToMaybe $ url =~ ("https?://(.*/)*([^#?]*)((#[^?]*)|(\\?[^#]*))*" :: String)
  guard $ not $ null f
  return f

changeSuffix :: String -> String
changeSuffix file = (++ ".hamlet") $ fromMaybe file $ do
  [_, baseName] <- listToMaybe $ file =~ ("(.*)\\.html?$" :: String)
  return baseName

