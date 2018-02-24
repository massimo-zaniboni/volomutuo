{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module Lib
    ( Position(..)
    , SharedAccount
    , Money
    , pos_empty
    , pos_balance
    , pos_calc
    , toHeader
    , Positions
    ) where

import Data.List as L
import Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding

type Money = Rational

-- | A shared account of money.
type SharedAccount = Money

-- | Position of a Party inside a shared balance, at a certain period.
data Position
       = Position {
           pos_income :: Money
           -- ^ the current income
         , pos_pastIncomes :: Money
           -- ^ the sum of past incomes
         , pos_withdraw :: Money
           -- ^ the current withdraw
         , pos_pastWithdraws :: Money
           -- ^ the sum of past withdraws
                  }
  deriving(Eq, Ord, Show)

pos_empty :: Position
pos_empty
  = Position {
      pos_income = 0
    , pos_pastIncomes = 0
    , pos_withdraw = 0
    , pos_pastWithdraws = 0
             }

-- | The balance of a Party.
pos_balance :: Position -> SharedAccount
pos_balance p = (pos_pastIncomes p) - (pos_pastWithdraws p) + (pos_income p) - (pos_withdraw p)

-- | The share of a Party respect the total balance.
pos_share :: SharedAccount -> Position -> Rational
pos_share totAccount p = (pos_balance p) / totAccount


-- | The positions of all parties in a period.
newtype Positions
          = Positions
              (SharedAccount
               -- ^ the balance of the shared account
               , [Position]
               -- ^ the position of each Party
               )
 deriving(Eq, Ord, Show)


-- | Given a list of outcomes to share between Parties,
--   and a list of Parties with respective Incomes,
--   share the outcome between parties in a proportional way
--   respect their progressive total income.
--   I.e. the party with the x% of the total balance,
--   pays the x% of the total outcome, and the other party the (100-x)%.
--
pos_calc
  :: [Money]
  -- ^ the periodic outcomes to share
  -> [[Money]]
  -- ^ the periodic incomes, grouped by month, and by Party. month,
  --   (i.e. [[month1_party1, month1_party1], [month2_party1, month2_party2]]
  --
  --  @require every Party has an income (also of 0) for each period.
  --  @require lenght targetOutcomes == lenght allIncomes
  -> [Positions]
     -- ^ the positions of all parties in all periods
     
pos_calc _ [] = []
pos_calc targetOutcomes allIncomes
  = let upd (previousAccount, positions1) (targetOutcome, incomes)
          = let
                currentTotAccount = previousAccount + (sum incomes)

                -- | Add the income to all positions,
                --   and update past withdraws, and past incomes.
                positions2 :: [Position]
                positions2
                  = map (\(p, income) -> p { pos_income = income
                                           , pos_pastIncomes = (pos_pastIncomes p) + (pos_income p)
                                           , pos_withdraw = 0
                                           , pos_pastWithdraws = (pos_pastWithdraws p) + (pos_withdraw p)
                                           }
                        ) (zip positions1 incomes)

                -- | Remove the outcome from parties in a proportional way respect the account
                positions3 :: [Position]
                positions3
                  = map (\p -> p { pos_withdraw = (targetOutcome * (pos_share currentTotAccount p))}) positions2

            in (currentTotAccount - targetOutcome, positions3)

        -- | Start with an empty position.
        initialPositions :: [Position]
        initialPositions
           = map (\_ -> pos_empty) (head allIncomes)

    in L.map Positions $ scanl' upd (0, initialPositions) (zip targetOutcomes allIncomes)

instance ToField Money where
  toField m =  encodeUtf8 $ Text.pack $ show $ fromRational m

instance Csv.ToRecord Positions where
  toRecord (Positions (totAccount, positions))
    = toRecord $ [totAccount] ++ L.concatMap (\p -> [pos_income p, pos_withdraw p]) positions

toHeader :: Int -> [Csv.Header]
toHeader n 
  = [V.fromList $ ["Tot account"] ++ (L.map (\s -> encodeUtf8 $ Text.pack s) $ L.concatMap (\i -> ["Party " ++ show i ++ " income", "Party " ++ show i ++ " withdrawal"]) [1.. n])]
  
