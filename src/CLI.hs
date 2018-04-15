{-# LANGUAGE OverloadedStrings #-}

module CLI (testerCLI) where

import System.IO
import System.Environment
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Aeson (decode)
import Data.Maybe

import Turtle

import Tester.Model
import Tester.Model.AesonInstances

data CLITester = CompleteTest { src :: T.Text, shuffle :: Bool }
               | Learn        { src :: T.Text, shuffle :: Bool }
               | Train        { src :: T.Text, shuffle :: Bool }

data TesterError = InvalidJSON { jsSrc :: String }
                 deriving (Show)

instance Exception TesterError

shuffleQuestionsSwitch :: Parser Bool
shuffleQuestionsSwitch = switch "shuffle" 's' "Shuffles input questions"

inputFileArgument :: Parser T.Text
inputFileArgument = argText "test" "JSON file containing test data"

testerCommand :: Parser (T.Text, Bool)
testerCommand = (,) <$> inputFileArgument
                    <*> shuffleQuestionsSwitch

cli :: Parser CLITester
cli =   fmap (uncurry Learn) (subcommand "learn" "Prints preview of the test" testerCommand)
    <|> fmap (uncurry Train) (subcommand "train" "Interactive testing" testerCommand)
    <|> fmap (uncurry CompleteTest) testerCommand

testerCLI :: IO ()
testerCLI = do
    command <- options "JSON-driven exam self-tester" cli
    run command
  -- TODO: make CLI for answering loaded Test
  --       stack exec selftester JSON_FILE [cmd]
  --       - in args there will be one .json file with test, load the TestSet
  --       - print the name of TestSet
  --       - start giving questions and receiving answers
  --       - print summary with correct/wrong answers and total/max score
  --
  --       - if [cmd]=="learn", dont ask but show answer immediately
  --       - else if [cmd]=="train", show the correct answer immediately after getting answer
  --       - else if no [cmd] is given, show question, get answer and print results at the end
  --       - else print some error message, same if file is not provided
  --
  --       Be creative, design it as you want and as you would like to use it!
  --       If you want to try cmdargs or other, feel free to do it! (you can have nice --version and --help)

-- TODO: try to deal here just with IO, dealing with "pure" Strings should be in different modules

run :: CLITester -> IO ()
run t@(CompleteTest _ _) = processInputs t >>= runCompleteTest
run t@(Learn _ _)        = processInputs t >>= runLearn 
run t@(Train _ _)        = processInputs t >>= runTrain

runCompleteTest :: TestSet -> IO ()
runCompleteTest t = undefined


runLearn :: TestSet -> IO ()
runLearn = undefined

runTrain :: TestSet -> IO ()
runTrain = undefined

loadTestData :: String -> IO TestSet
loadTestData src = do
    handle  <- openFile src ReadMode
    content <- BS.hGetContents handle
    case (decode content) of
        Just t  -> return t
        Nothing -> throw (InvalidJSON src)

processInputs :: CLITester -> IO TestSet
processInputs t = permuteQuestionsIfNeeded <$> testerData
    where testerData = loadTestData . T.unpack . src $ t
          permuteQuestionsIfNeeded = permuteQuestions (shuffle t)

permuteQuestions :: Bool -> TestSet -> TestSet
permuteQuestions True t  = t { tsItems = shuffledItems } 
    where shuffledItems = tsItems t
permuteQuestions False t = t
