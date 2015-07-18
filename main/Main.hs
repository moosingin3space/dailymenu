module Main where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Food2ForkAPI as F2F
import Network.HTTP.Conduit

data Timeframe = Day | Week

data SearchOptions = SearchOptions
                   { optQuery :: String }

data SaveOptions = SaveOptions
                 { optRecipeId :: String }

data PlanOptions = PlanOptions
                 { optTimeframe :: Timeframe }

data Options = Options Command

data Command = Search SearchOptions
             | Save SaveOptions
             | Plan PlanOptions

-- TODO
f2fApiKey :: B.ByteString
f2fApiKey = B.pack ""

httpGetter :: B.ByteString -> IO (Maybe BL.ByteString)
httpGetter url = do
    response <- simpleHttp $ show url
    return $ Just response

searchOptions :: Parser Command
searchOptions = Search <$> (SearchOptions
            <$> argument str
                ( metavar "QUERY"
               <> help "Query to submit to Food2Fork" ))

saveOptions :: Parser Command
saveOptions = undefined

planOptions :: Parser Command
planOptions = undefined

options :: Parser Options
options = Options
   <$> subparser (command "search" (info searchOptions
                                   (progDesc "Search through Food2Fork"))
               <> command "save" (info saveOptions
                                 (progDesc "Save recipe for planning"))
               <> command "plan" (info planOptions
                                 (progDesc "Plan a certain length of time")))

-- TODO
runApp :: Options -> IO ()
runApp (Options (Search opt)) = do
    results <- search
    print $ show $ F2F.recipes <$> results
      where
          search = F2F.search httpGetter f2fApiKey (B.pack (optQuery opt)) F2F.Rating 1
runApp (Options (Save opt))= undefined
runApp (Options (Plan opt)) = undefined
runApp _ = return ()

main :: IO ()
main = execParser opts >>= runApp
    where
        opts = info (helper <*> options)
            ( fullDesc
           <> header "dailymenu - a meal-planning utility" )
