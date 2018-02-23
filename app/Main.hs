module Main where

import Lib

outcomes :: [Money]
outcomes = [100, 100, 100, 100]

incomes = [[200, 300], [200, 300], [100, 300]]

main :: IO ()
main = do putStrLn $ "Outcomes: " ++ show outcomes
          putStrLn $ "Incomes: " ++ show incomes
          putStrLn $ "Movements: " ++ (show $ pos_calc outcomes incomes)
