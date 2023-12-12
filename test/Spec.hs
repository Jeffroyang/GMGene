import ChessParserTest qualified as CPT
import ChessSimpleAITest as CSAT
import ChessTest qualified as CT
import GameAITest qualified as GAT
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do
  -- putStrLn "*** Testing Parser ***"
  -- CPT.test_all
  -- CPT.qc
  -- putStrLn "*** Testing Move Validity ***"
  -- CT.test_all
  -- CT.qc
  -- putStrLn "*** Testing Simple Evaluation ***"
  -- CSAT.test_all
  -- CSAT.qc
  putStrLn "*** Testing Game AI ***"
  GAT.qc
  return ()
