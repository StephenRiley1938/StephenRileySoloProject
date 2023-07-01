module Main where

import  System.IO

import  Actions
import  Lib
import  Types

main :: IO ()
main = do
  -- Print banner.
  printStartupBanner

  -- Read the seating prices from a file.
  seatingPrices <- readSeatingPricesFromFile _SeatingPricesFilePath_
  case seatingPrices of
    Left errorMsg -> putStrLn errorMsg
    Right seatingPrices -> do
    -- Initialize the prices and other invariants of the Auditorium: the AudProps.
      let audProps = initializeAudProps seatingPrices

      -- Read the state of available/sold seats in the auditorium from a file.
      -- This is a List of Strings of '*' (sold) and '#' (available) seats.
      -- It is a String representation of the initial state.
      seatingStateLines <- readSeatingStateFromFile _SeatingStateFilePath_
      case seatingStateLines of
        Left errorMsg -> putStrLn errorMsg
        Right seatingStateLines -> do
          -- Initialize the running state of the system. It is a Set of Seats.
          -- Any Seat in the set is Sold.
          -- Seats are added to the Set as they are sold.
          let soldSeatsState = initializeSoldSeatsState audProps seatingStateLines

          -- Enter the main loop with the initial running state.
          -- When the main loop is broken, the program returns here and ends.
          mainMenuLoop audProps soldSeatsState
