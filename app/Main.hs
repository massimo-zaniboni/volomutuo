module Main where

import Lib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv

outcomes :: [Money]
outcomes = [100, 100, 100, 100]

incomes = [[200, 300], [200, 300], [100, 300]]

main :: IO ()
main = do putStrLn $ "Outcomes: " ++ show outcomes
          putStrLn $ "Incomes: " ++ show incomes

          let h = toHeader (length $ head incomes)

          LBS.putStr $ Csv.encode h
          LBS.putStrLn $ Csv.encode $ pos_calc outcomes incomes
