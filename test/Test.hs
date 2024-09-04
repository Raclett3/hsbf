module Main (main) where

import Parser
import qualified System.Exit as Exit
import Test.HUnit

tests :: Test
tests =
  TestList
    [ "test parse success 1"
        ~: parseStringToBf "++++++++[>++++++++<-]>+."
        ~?= Right (replicate 8 Incr ++ [Loop ([IncrPtr] ++ replicate 8 Incr ++ [DecrPtr, Decr]), IncrPtr, Incr, PutChar]),
      "test parse success 2"
        ~: parseStringToBf "++++ [ >++++ [ >++++<- ] <- ] >>+."
        ~?= Right [Incr, Incr, Incr, Incr, Loop [IncrPtr, Incr, Incr, Incr, Incr, Loop [IncrPtr, Incr, Incr, Incr, Incr, DecrPtr, Decr], DecrPtr, Decr], IncrPtr, IncrPtr, Incr, PutChar],
      "test parse failure 1"
        ~: parseStringToBf "++++f----"
        ~?= Left (UnexpectedChar 'f'),
      "test parse failure 2"
        ~: parseStringToBf "++++[++["
        ~?= Left UnexpectedEof
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
