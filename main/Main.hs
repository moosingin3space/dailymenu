module Main where

import Options.Applicative

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
runApp (Options (Search opt)) = undefined
runApp (Options (Save opt))= undefined
runApp (Options (Plan opt)) = undefined
runApp _ = return ()

main :: IO ()
main = execParser opts >>= runApp
    where
        opts = info (helper <*> options)
            ( fullDesc
           <> header "dailymenu - a meal-planning utility" )
