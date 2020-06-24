{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module EroReports (report, reportListPage, allReports) where


import           Control.Monad.IO.Class
import           Control.Monad.Parallel     (forM_)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Csv                   as Csv
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import           Data.Time
import qualified Data.Vector                as V
import           Data.Void
import           Development.Shake.FilePath
import           Network.HTTP.Req
import           System.Directory
import           System.FilePath
import           Text.HTML.Scalpel
import           Text.HTML.TagSoup
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.URI

import           Common
import           Types

type Parser = Parsec Void Text

parseDay :: Parser Day
parseDay = do
  d <- try (count 2 digitChar) <|> count 1 digitChar
  _ <- char '-'
  m <- try (count 2 digitChar) <|> count 1 digitChar
  _ <- char '-'
  y <- count 4 digitChar
  parseTimeM True defaultTimeLocale "%Y-%m-%d" $ y ++ "-" ++ m ++ "-" ++ d

tagLink :: Scraper Text [Text]
tagLink = chroots ("article" // "a") $ do
    content <- attr "href" "a"
    return content


extractDate :: Text -> Maybe Day
extractDate = parseMaybe parseDay -- (skipSomeTill printChar parseDay <* printChar)

reportListPage :: String -> IO ()
reportListPage schoolNumber = do
  let p = "data" </> "reports" </> "lists" </> schoolNumber -<.> "html"
  exists <- doesFileExist p
  if exists
     then pure ()
     else
        runReq defaultHttpConfig $ do
          r <- req GET (https "www.ero.govt.nz" /: "report-view") (NoReqBody) bsResponse (queryParam "id" (pure schoolNumber) <> nzhScraper)
          liftIO $ T.writeFile p (T.decodeUtf8With (\_ _ -> Nothing) $ responseBody r)

allReports :: FilePath -> [FilePath] -> IO ()
allReports outfile files = do
  d <- mapM go files
  BL.writeFile outfile (Csv.encodeDefaultOrderedByName $ concat d)

  where
    go fp = do
      bdy <- T.readFile fp
      let links = map (\l -> "https://www.ero.govt.nz" <> l) $ maybe [] nub $ scrape tagLink $ parseTags bdy
          dates = map show $ catMaybes $ map (extractDate . T.takeEnd 10 . T.replace "/" "") links
          sn = takeBaseName fp
      pure $ map (\(d,l) -> ReportLink sn d l) $ zip dates links


report :: FilePath -> IO ()
report fn = do
  exists <- doesFileExist fn
  if exists
     then print ("exists " ++ fn)
     else do
            let d = takeBaseName fn
                sn = takeDirectory1 $ dropDirectory1 $ dropDirectory1 fn
            (Right (_, reps)) <- Csv.decodeByName <$> BL.readFile "data/reports.txt"
            let l = reportLinkUrl $ V.head $ V.filter (\(ReportLink {reportLinkSchoolNumber, reportLinkDate}) -> reportLinkSchoolNumber == sn && reportLinkDate == d) reps
            createDirectoryIfMissing False ("data" </> "reports" </> d)

            bdy <- runReq defaultHttpConfig $ do
              let p = parser <* eof :: Parsec Void Text URI
              case parseMaybe p l of
                      Nothing -> pure ""
                      Just uri ->
                        do
                          let (Just (url, _)) = useHttpsURI uri
                          liftIO $ print url
                          rep <- req GET url NoReqBody bsResponse nzhScraper
                          pure (responseBody rep)
            T.writeFile fn ((T.decodeUtf8With (\_ _ -> Nothing)) bdy)


