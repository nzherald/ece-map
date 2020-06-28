{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EroRank (ranks) where

import qualified Control.Monad.Parallel as P
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Csv               as Csv
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Vector            as V
import           GHC.Generics
import           System.FilePath

import           Common
import           Types



ranks :: FilePath -> FilePath -> IO ()
ranks outfile fp = do
  (Right (_, sch)) <- Csv.decodeByName <$> BL.readFile fp
  r <- mapM readRating (V.toList sch)
  BL.writeFile "data/ranked-schools.csv" $ Csv.encodeDefaultOrderedByName r



readRating :: ReportLink -> IO Ranked
readRating (ReportLink id_ s u) = do
  print $ id_ ++ " " ++ s
  htmlstr <- T.readFile $ "data" </> "reports" </> id_ </> s -<.> "html"
  let rank = if T.isInfixOf "<b>Very well placed</b>" htmlstr || T.isInfixOf "<strong>Very well placed</strong>" htmlstr
            then VeryWellPlaced
            else if T.isInfixOf "<b>Well placed</b>" htmlstr || T.isInfixOf "<strong>Well placed</strong>" htmlstr
            then WellPlaced
            else if T.isInfixOf "<b>Requires further development</b>" htmlstr ||T.isInfixOf "<strong>Requires further development</strong>" htmlstr
            then FurtherDevelopment
            else if T.isInfixOf "<b>Not well placed</b>" htmlstr || T.isInfixOf "<strong>Not well placed</strong>" htmlstr
            then NotWellPlaced
            else NoRating
  pure $ Ranked id_ s rank u



-- parseDay :: Parser Day
-- parseDay = do
--   d <- try (count 2 digitChar) <|> count 1 digitChar
--   _ <- char '-'
--   m <- try (count 2 digitChar) <|> count 1 digitChar
--   _ <- char '-'
--   y <- count 4 digitChar
--   parseTimeM True defaultTimeLocale "%Y-%m-%d" $ y ++ "-" ++ m ++ "-" ++ d

-- tagLink :: Scraper Text [Text]
-- tagLink = chroots ("article" // "a") $ do
--     content <- attr "href" "a"
--     return content


-- extract :: BS.ByteString -> [Text]
-- extract v = do
--   case decodeUtf8' v of
--     Left _ -> [] -- for now just skip decode errors - if there are a lot of them I'll figure out a better strategy
--     Right vv -> maybe [] nub $ scrape tagLink $ parseTags vv

-- extractDate :: Text -> Maybe Day
-- extractDate = parseMaybe (skipSomeTill printChar parseDay <* printChar)

-- get :: School -> IO ()
-- get (School sch_id n _ _) = do
--   let p = "data" </> "reports" </> sch_id -<.> "html"
--   exists <- doesFileExist p
--   if exists
--   then
--      print $ n <> " exists"
--   else
--     runReq defaultHttpConfig $ do
--         r <- req GET (https "www.ero.govt.nz" /: "report-view") (NoReqBody) bsResponse (queryParam "id" (pure sch_id) <> nzhScraper)
--         let bdy = responseBody r :: BS.ByteString
--             links = map (\l -> "https://www.ero.govt.nz" <> l) $ extract bdy
--             dates = map show $ catMaybes $ map extractDate links
--             summary = zip dates links

--         mapM_ fetchReport summary

--         liftIO $ do
--           BS.writeFile p (responseBody r)
--           print $ n <> " fetched"
--           BL.writeFile (p -<.> "csv") (Csv.encode $ reverse $ sortOn fst summary)
--       where
--         fetchReport :: (FilePath, Text) -> Req ()
--         fetchReport (d, l)  = do
--           let p = parser <* eof :: Parsec Void Text URI
--           case parseMaybe p l of
--             Nothing -> liftIO $ print $ "could not decode " <> l
--             Just uri ->
--                       do
--                         let fo = "data" </> "reports" </> sch_id ++ "-" ++ d -<.> "html"
--                             (Just (url, _)) = useHttpsURI uri
--                         rep <- req GET url NoReqBody bsResponse nzhScraper
--                         liftIO $ BS.writeFile fo (responseBody rep)
