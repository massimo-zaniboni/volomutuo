module Lib
    ( Movement(..)
    , Balance
    , Money
    , mov_empty
    , mov_balance
    , mov_calc
    ) where

import Data.List

type Money = Rational

type Balance = Money

data Movement
       = Movement {
           mov_income :: Money
         , mov_totDeposited :: Money
         , mov_totWithdrawed :: Money
         , mov_share :: Rational
         , mov_withdraw :: Money
                  }
  deriving(Eq, Show)
      
mov_empty :: Movement
mov_empty
  = Movement {
      mov_income = 0
    , mov_totDeposited = 0
    , mov_totWithdrawed = 0
    , mov_share = 0
    , mov_withdraw = 0
             }

mov_balance :: Movement -> Balance
mov_balance m = (mov_totDeposited m) - (mov_totWithdrawed m)

mov_calc
  :: [Money]
  -- ^ the monthly outcomes to share
  -> [[Money]]
  -- ^ the monthly incomes,
  --   month by month,
  --   person by person.
  --
  --   > [[month1_person1, month1_person2], [month2_person1, month2_person2]]
  --
  --  @require every person has an income also of 0 for each month
  -> [(Balance, [Movement])]
  -- ^ the total balance, month by month,
  --   and the movements, month by month, and person by person

mov_calc _ [] = []
mov_calc targetOutcomes allIncomes
  = let upd (totBalance1, movements1) (targetOutcome, incomes)
          = let
                currentTotBalance = totBalance1 + (sum incomes)

                movements2
                  = map (\(m, income)
                             -> let previousWithdraw = mov_withdraw m
                                    currentIncome = income
                                    previousTotDeposited = mov_totDeposited m
                                    currentTotDeposited = previousTotDeposited + currentIncome
                                    currentTotWithdrawed = (mov_totWithdrawed m) + previousWithdraw
                                in  m { mov_income = currentIncome
                                      , mov_totDeposited = currentTotDeposited
                                      , mov_totWithdrawed = currentTotWithdrawed
                                      , mov_share = (currentTotDeposited - currentTotWithdrawed) / currentTotBalance
                                  }

                        ) (zip movements1 incomes)


                movements3
                  = map (\m -> m { mov_withdraw = (targetOutcome * (mov_share m))}) movements2

            in (currentTotBalance, movements3)

        initialMovements
          = map (\_ -> mov_empty) (head allIncomes)

    in  scanl' upd (0, initialMovements) (zip targetOutcomes allIncomes)

