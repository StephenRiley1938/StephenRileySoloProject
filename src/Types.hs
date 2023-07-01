module Types where

import  qualified Data.Set as Set
import  qualified Data.Map as Map

_nRows_ :: Int
_nRows_ = 15 -- First row (by stage) is Row A.

_nSeats_ :: Int
_nSeats_ = 30 -- number of seats per row, numbered from 1 to _nSeats_ left to right, looking at stage.

_SeatingStateFilePath_ :: String
_SeatingStateFilePath_ = "./assets/seating_state.txt"

_TestSeatingStateOutputFilePath_ :: String
_TestSeatingStateOutputFilePath_ = "./assets/test_seating_state_output.txt"

_SeatingPricesFilePath_ :: String
_SeatingPricesFilePath_ = "./assets/row_seating_prices.txt"

_TestSeatingPricesOutputFilePath_ :: String
_TestSeatingPricesOutputFilePath_ = "./assets/test_seating_prices_output.txt"


data Seat = Seat
    {   
    -- Field containing Row letter.
        getSeatRow :: Char

    -- Number of the seat in a row, numbered from 1 to _nSeats_ left to right, looking at stage.
    ,   getSeatNum :: Int

    -- Same price for all seats in a row, but easier to store and work with per seat
    -- because purchases can spill over multiple rows.
    ,   getSeatPrice :: Float

    -- Can be computed from row and num, but stored when initialized
    -- for speed, and, if needed, clarity of debugging a seat.
    -- Global "snake" number from upper-left corner to A1.
    -- E.g., for the _nRows_ = 15 and _nSeats_ = 30,
    -- seat O1 = 1, seat O30 = 30, N30 = 31, N1 = 60, M1 = 61 ... A1 = 420 ... A30 = 450.
    -- Odd rows count up from left to right.
    -- Even rows count up from right to left.
    -- This is to handle a purchase of more seats than are remaining in a row.
    -- It snakes around the right end to the row in front heading to the left.
    -- If the purchasers wants more that _nSeats_, it will snake around the left
    -- at the end of the next row on the left, alternating right, left....

    -- getSnakeNumOddRowUpToRight is used when the first seat in purchase is in a odd row.
    ,   getSnakeNumOddRowUpToRight :: Int

     -- This is the same concept, but is for snaking the other way.   
    -- getSnakeNumEvenRowUpToRight is used when the first seat in purchase is in a even row.
    ,   getSnakeNumEvenRowUpToRight :: Int

    }   deriving Show

-- These are needed to make a Set of Seats for newtype SoldSeatsState.
instance Eq Seat where
  (Seat r1 n1 p1 snO1 snE1) == (Seat r2 n2 p2 snO2 snE2) = snO1 == snO2

instance Ord Seat where
  compare (Seat _ _ _ sn1 _) (Seat _ _ _ sn2 _) =
    compare sn1 sn2 
    
-- Store properies of the auditorium that are invariant
-- after initialization. The initialization reads seat prices,
-- (the same prices for each seat in a row) from a file.
data AudProps = AudProps 
    {
    -- A list of the rows of seats.
    -- The first row is A, closest to the stage.
    -- Each row is a list of seats. The first element is Seat 1,
    -- starting at the left side of the auditorium, looking toward
    -- the stage.
        getAudSeats :: [[Seat]]

    -- A map from the snakeNum to a Seat.
    -- This will get to a seat quickly, rather than indexing
    -- into the list of lists in getAudSeats.
    ,   getMapSnakeNumOddRowUpToRight2Seat :: Map.Map Int Seat
    ,   getMapSnakeNumEvenRowUpToRight2Seat :: Map.Map Int Seat


    } deriving Show

-- A set of Seat types containing seats that are sold.
-- This set represents the "state" of the auditorium.
-- It is the only thing that changes while the program runs.
-- Elements of AudProps getAudSeats are tested for membership in this set
-- to determine if they are sold. This set is replaced with a new set
-- with additional members as seats are sold.
newtype SoldSeatsState = SoldSeatsState { getSoldSeats :: Set.Set Seat } deriving Show 


