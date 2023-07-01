module Lib where

import  Types
import  qualified Data.Map as Map
import  qualified Data.Set as Set
import  Data.Char (intToDigit)
import  Data.Maybe (fromMaybe)
import  Text.Printf (PrintfType, printf)

_rowInt2Letter_ :: Map.Map Int Char
_rowInt2Letter_ = Map.fromList $ zip [1..] ['A'.."ABCDEFGHIJKLMNOPQRSTUVWXYZ" !! (_nRows_ - 1)]

lookupRowLetter :: Int -> Char
lookupRowLetter rowNum = case Map.lookup rowNum _rowInt2Letter_ of
    Nothing -> '_' -- This won't be hit because error checking will bound rows to _nRows_
    Just (x) -> x

_rowLetter2Int_ :: Map.Map Char Int
_rowLetter2Int_ = Map.fromList $ zip (['A'.."ABCDEFGHIJKLMNOPQRSTUVWXYZ" !! (_nRows_ - 1)]) [1..]    

lookupRowInt :: Char -> Int
lookupRowInt rowLetter = case Map.lookup rowLetter _rowLetter2Int_ of
    Nothing -> -1 -- This won't be hit because error checking will bound rows to _nRows_
    Just (x) -> x

-- Not currently used.
-- Global number from seat A1 = 1 to _nSeats_, to represent the index (1 based)
-- of a seat in 1-D array of all seats.
-- At the end of the row, the count continues at the beginning of the next.
-- E.g. for _nSeats_ = 30, seat A30 = 30, B1 = 31.
computeArrayNum :: Seat -> Int
computeArrayNum seat = getSeatNum seat + ((lookupRowInt $ getSeatRow seat) - 1)*_nSeats_

-- Used to initialize AudProps and verify first seat.
computeSnakeNum :: Char -> Char -> Int -> Int    
computeSnakeNum rowLetterFirstSeat rowLetter seatNum = -- See data type for explanation.
    let
        rnfs = lookupRowInt rowLetterFirstSeat
        rn = lookupRowInt rowLetter
    in
        (_nRows_ - rn)*_nSeats_ + --Offset from the seats in rows behind the seat.
        if odd rnfs
        then
            if even rn
            then _nSeats_ - seatNum + 1 -- Count from right for even rows.
            else seatNum -- Count from left for odd rows.
        else
            if odd rn
            then _nSeats_ - seatNum + 1 -- Count from right odd rows.
            else seatNum -- Count from left for even rows. 

-- Used to access snakeNum from a seat.
getSnakeNum :: Char -> Seat -> Int
getSnakeNum rowLetterFirstSeat seat =
    if odd $ lookupRowInt rowLetterFirstSeat
    then getSnakeNumOddRowUpToRight seat
    else getSnakeNumEvenRowUpToRight seat

-- Given a list of row prices, return an AudProps type with the invariant properties of the Auditorium.
initializeAudProps :: [Float] -> AudProps
initializeAudProps rowPrices = 
    let
        -- Recurse through the list of row prices until empty, accumulating a list of rows of seats.
        -- The accumulated seats are returned after being reversed to put row 1 at the head.
        -- A counter for the row number is incremented as the recusion goes. It is converted to
        -- the row letter for the seat type.
        go :: Int -> [[Seat]] -> [Float] -> [[Seat]]
        go rowNumM1 acc [] = reverse acc
        go rowNumM1 acc (rowPrice:rowPrices) =
            let rowNum = rowNumM1 + 1
                rowLetter = lookupRowLetter rowNum
            in
                go rowNum ((initialize1RowOfSeatProps rowLetter rowPrice):acc) rowPrices
        allSeats = go 0 [] rowPrices
        flattenedSeats = concat allSeats
    in
        AudProps
            {
                getAudSeats = allSeats
            ,   getMapSnakeNumOddRowUpToRight2Seat = Map.fromList $ zip (getSnakeNumOddRowUpToRight <$> flattenedSeats) flattenedSeats
            ,   getMapSnakeNumEvenRowUpToRight2Seat = Map.fromList $ zip (getSnakeNumEvenRowUpToRight <$> flattenedSeats) flattenedSeats
            }

-- Given the row letter, the price for each seat in a row, 
-- return a row of Seat types.
initialize1RowOfSeatProps :: Char -> Float -> [Seat]
initialize1RowOfSeatProps row price =
    let
        -- The seat numbers of the row.
        seatNums = [1.._nSeats_]
        
        -- Recurse through the list of seat numbers until empty, accumulating a list of seats.
        -- The accumulated seats are returned after being reverse to put seat 1 at the head. 
        go :: [Seat] -> [Int] -> [Seat]
        go acc [] = reverse acc -- return of function
        go acc (seatNum:remSeatNums) =
           let seat =   Seat
                            {
                                getSeatRow = row
                            ,   getSeatNum = seatNum
                            ,   getSeatPrice = price
                            ,   getSnakeNumOddRowUpToRight = computeSnakeNum 'A' row seatNum
                            ,   getSnakeNumEvenRowUpToRight = computeSnakeNum 'B' row seatNum
                            }
            in
                go (seat:acc) remSeatNums
    in
        go [] seatNums

-- Given the AudProps and the seating state read from file, return a SoldSeatsState set.
-- Assume the list of of list of seats matches the dimensions of the list of list of chars
-- for the sold/available state read from file based on checks in the read functions.
initializeSoldSeatsState :: AudProps -> [String] -> SoldSeatsState
initializeSoldSeatsState audProps rowsOfStateChars =
    let
        rowsOfSeats = getAudSeats audProps
        go :: [[Seat]] -> [[Seat]] -> [String] -> [[Seat]]
        go acc [] [] = acc -- Don't care about reversing order for a set.
        go acc (seatList:seatLists) (stateCharList:stateCharLists) =
            go ((initialize1RowOfSoldSeatsState seatList stateCharList):acc) seatLists stateCharLists
    in
        SoldSeatsState { getSoldSeats = Set.fromList (concat (go [] rowsOfSeats rowsOfStateChars)) }
        
initialize1RowOfSoldSeatsState :: [Seat] -> String -> [Seat]
initialize1RowOfSoldSeatsState rowOfSeats rowOfStateChars =
    let
        go :: [Seat] -> [Seat] -> String -> [Seat]
        go acc [] [] = acc -- Don't care about reversing order for a set.
        go acc (seat:seats) (stateChar:chars) =
            if stateChar == '*'
                    then go (seat:acc) seats chars
                    else go acc seats chars
    in
        go [] rowOfSeats rowOfStateChars


seatString :: Seat -> String
seatString seat = printf "%s%d" [getSeatRow seat] (getSeatNum seat)

isSeatSold :: SoldSeatsState -> Seat -> Bool
isSeatSold soldSeatsState seat = Set.member seat (getSoldSeats soldSeatsState)

-- Given a row of Seats, return a list of '*' or '#'.
getListSeatStatesFromRow :: SoldSeatsState -> [Seat] -> String
getListSeatStatesFromRow soldSeatsState row =
    let paIsSeatSold = isSeatSold soldSeatsState
    in
    fmap (\seat -> if paIsSeatSold seat
                    then '*'
                    else '#'
            ) row

-- Given a SoldSeatsState, return a list String of '*' and '#'.
getListOfRowsOfSeatStates :: AudProps -> SoldSeatsState -> [String]
getListOfRowsOfSeatStates audProps soldSeatsState =
    -- Partially applied function for getting a row:
    let getRowOfSeatStates = getListSeatStatesFromRow soldSeatsState
    in
        getRowOfSeatStates <$> getAudSeats audProps

getSeatFromSnakeNum :: AudProps -> Char -> Int -> Seat
getSeatFromSnakeNum audProps rowLetterFirstSeat snakeNum =
    let rn = lookupRowInt rowLetterFirstSeat
        map2seat = if odd rn
                    then getMapSnakeNumOddRowUpToRight2Seat audProps
                    else getMapSnakeNumEvenRowUpToRight2Seat audProps
        mseat = Map.lookup snakeNum map2seat
    in
        -- Return the first seat in the Auditorium of the Map fails.
        -- This should never happen because the map was constructed
        -- with verified data.
        fromMaybe (head $ head $ getAudSeats audProps) mseat

-- Check return the intersection of desired seats and sold seats.
-- If none, all desired seats can be purchased.
checkDesiredSeats :: SoldSeatsState -> [Seat] -> [Seat]
checkDesiredSeats soldSeatsState allSeatsDesired =
    let setSoldSeats = getSoldSeats soldSeatsState
        setDesiredSeats = Set.fromList allSeatsDesired
        setUnavailableSeats = Set.intersection setSoldSeats setDesiredSeats
    in
        Set.toList setUnavailableSeats

-- Utility found on the internet.        
wrapString :: Int -> String -> String
wrapString lineWidth str = unlines $ wrapLines lineWidth (words str)
  where
    wrapLines _ [] = []
    wrapLines width (w:ws) = go width [w] ws

    go _ acc [] = unwords (reverse acc) : []
    go width acc (x:xs)
      | length (unwords (x:acc)) <= width = go width (x:acc) xs
      | otherwise = unwords (reverse acc) : go width [x] xs
