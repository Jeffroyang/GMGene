import ChessParserTest qualified as CPT
import ChessTest qualified as CT
import GameAITest
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "*** Testing Parser ***"
  CPT.test_all
  putStrLn "*** Testing Move Validity ***"
  CT.test_all
  return ()
