module Actions where

import  Control.Monad (forever)
import  System.IO
import  Text.Printf (printf)
import  Text.Read (readMaybe)
import  System.Directory (doesFileExist)
import  Data.Char (ord)
import  Data.Char (isAlphaNum)
import  Data.Char (toUpper)

import  qualified Data.Map as Map
import  qualified Data.Set as Set
import  Data.Set (toList)

import  Data.List 
import  Control.Monad (forM_)

import  Types
import  Lib

printStartupBanner :: IO ()
printStartupBanner =
  putStrLn "________________________________________________________________________________" >>
  putStrLn "|                                                                              |" >>
  putStrLn "|                      Cardano (Formerly Crypto.com) Arena                     |" >>
  putStrLn "|                                  Ticket Manager                              |" >>
  putStrLn "|                                                                              |" >>
  putStrLn "________________________________________________________________________________"

mainMenuLoop :: AudProps -> SoldSeatsState -> IO ()
mainMenuLoop audProps soldSeatsState = do
  hSetBuffering stdin NoBuffering -- Set buffering mode to NoBuffering for immediate character reading
  putStrLn "\nWhat would you like to do?"
  putStrLn "1. Print a seating chart."
  putStrLn "2. Buy Tickets."
  putStrLn "3. Print a sales report."
  putStrLn "4. Reset all seats to available."
  putStrLn "q. Quit program."
  putStr "Please enter your selection: "
  hFlush stdout
  char <- getChar
  if isAlphaNum char
    then case toUpper char of
      '1' -> putStr "\n"  >> printSeatingChart audProps soldSeatsState
                          >> mainMenuLoop audProps soldSeatsState
      '2' -> do
                putStr "\n"
                buyResult <- buySeats audProps soldSeatsState
                case buyResult of
                  Left (someString) -> case someString of
                    "mainMenuLoop" -> putStr "\n" >> mainMenuLoop audProps soldSeatsState
                    -- Otherwise, assume quit
                    _   -> writeSoldSeatsStateToFile audProps soldSeatsState -- Write final state.
                        >> putStrLn "\n\nThanks for shopping!\n" -- Looping ends.
                  Right (newSoldSeatState) -> mainMenuLoop audProps newSoldSeatState
      '3' -> putStr "\n"  >> printSalesReport audProps soldSeatsState
                          >> mainMenuLoop audProps soldSeatsState
      '4' -> putStrLn "\n\nAll seats are now available." >> mainMenuLoop audProps SoldSeatsState { getSoldSeats = Set.empty }
      'Q' -> writeSoldSeatsStateToFile audProps soldSeatsState -- Write final state.
                              >> putStrLn "\n\nThanks for shopping!\n" -- Looping ends.
      _ -> invalidCase >> mainMenuLoop audProps soldSeatsState
    else invalidCase >> mainMenuLoop audProps soldSeatsState
  where invalidCase = putStrLn "\n\nInvalid selection: please enter 1 thru 4 or Q/q."

printSeatingChart :: AudProps -> SoldSeatsState -> IO ()
printSeatingChart audProps soldSeatsState = do
  let

-- Format Seating Chart like this, depending on the number of seats in a row:

--                    Seating Chart        
--            Available (#) -- Sold (*)  
--                 Seats 1 thru 30       
--         123456789012345678901234567890   Price $ (Per Seat In Row)
-- Row A   ***###***###*########*****####             12.50

    -- Center a given String in a longer String of n spaces (unless it is shorter).
    centerText :: String -> Int -> String
    centerText str n =  let rem = n - (length str)
                        in  if rem <= 0
                            then str
                            else  let rem2 = (fromIntegral rem / 2)
                                    in      replicate (ceiling rem2) ' '
                                        ++  str
                                        ++  replicate (floor rem2) ' '
    padLeft = "        "
    h1 = padLeft ++ centerText "Seating Chart" _nSeats_
    h2 = padLeft ++ centerText "Available (#) -- Sold (*)" _nSeats_
    h3' :: String
    h3' = printf "Seats 1 thru %d" (_nSeats_ :: Int) 
    h3 = padLeft ++ centerText h3' _nSeats_
    h4 = padLeft ++ take _nSeats_ (concat $ replicate (ceiling (fromIntegral _nSeats_ / 10)) "1234567890")
                  ++ "   Price $ (Per Seat In Row)"
    -- Partially applied function for getting a row:
    getRowOfSeatStates = getListSeatStatesFromRow soldSeatsState
    listOfRowStrings =
      fmap (\row -> let prf = "Row "
                    in  prf ++ [getSeatRow $ head row] -- Add Row letter to prf.
                            ++  take ((length padLeft) - ((length prf) + 1)) padLeft -- Remove leading spaces from padLeft to line stuff up.
                            ++  getRowOfSeatStates row
                            ++  printf "%18.2f" (getSeatPrice $ head row) -- Add Row price to prf.
                              ) (getAudSeats audProps)
  mapM_ putStrLn $ "":h1:h2:h3:h4:listOfRowStrings

buySeats :: AudProps -> SoldSeatsState -> IO (Either String SoldSeatsState) 
buySeats audProps soldSeatsState = do
  firstSeat <- readFirstSeat audProps soldSeatsState
  case firstSeat of
    Left errorMsg -> return $ Left errorMsg
    Right firstSeat -> do
      allSeatsDesired <- readRemainingSeats audProps soldSeatsState firstSeat
      case allSeatsDesired of
        Left errorMsg -> return $ Left errorMsg
        Right allSeatsDesired -> do
          let unavailableSeats = checkDesiredSeats soldSeatsState allSeatsDesired
              nseatsUA = length unavailableSeats
          if nseatsUA == 0
          then do
            let nseats = length allSeatsDesired
            case nseats of
              1 -> putStrLn $ printf "\nOkay! Seat %s is available and reserved." (seatString $ head allSeatsDesired)
              2 -> putStrLn $ printf "\nOkay! Seats %s and %s are available and reserved."
                (seatString $ head allSeatsDesired) (seatString $ last allSeatsDesired)
              _ -> putStrLn $ printf "\nOkay! Seats %s thru %s are available and reserved."
                (seatString $ head allSeatsDesired) (seatString $ last allSeatsDesired)
            putStrLn $ printf "Total price: $%.2f\nPickup your tickets at Will Call." (sum $ getSeatPrice <$> allSeatsDesired)
            return $ Right $ SoldSeatsState (Set.fromList (allSeatsDesired
                                                            ++ Set.toList (getSoldSeats soldSeatsState)))
          else do
            case nseatsUA of
              1 -> putStrLn $ printf "\nUnfortunately, seat %s is unavailable."
                (seatString $ head unavailableSeats)
              2 -> putStrLn $ printf "\nUnfortunately, seats %s and %s are unavailable."
                (seatString $ head unavailableSeats) (seatString $ last unavailableSeats)
              _ -> putStrLn $ wrapString 80
                              (printf "\nUnfortunately, seats %s are unavailable."
                              -- Separate with ", " and drop trailing one.
                                (reverse
                                  (drop 2
                                    (reverse
                                      (concat $ fmap (\s -> (seatString s) ++ ", ") unavailableSeats)))))
            boolYorN <- readYorN "Do you want to select another seat"                        
            if boolYorN
              then putStr "\n" >> buySeats audProps soldSeatsState
            else
              return $ Left ("mainMenuLoop")

-- Read what is entered for the first seat and return that seat object if it is valid.
readFirstSeat :: AudProps -> SoldSeatsState -> IO (Either String Seat)
readFirstSeat audProps soldSeatsState = do
  putStr "\nEnter the first Seat, e.g., \"F12\": "
  hFlush stdout
  input <- getLine
  let verifySeat = verifyFirstSeat audProps input
  case verifySeat of
    Left (errStr) ->  putStrLn errStr
                      >> readFirstSeat audProps soldSeatsState 
    Right (firstSeat) -> do
      let isFirstSeatSold = isSeatSold soldSeatsState firstSeat
      if isFirstSeatSold
      then do
        putStrLn $ printf "\nSeat %s is already sold." (seatString firstSeat)
        boolYorN <- readYorN "Do you want to select another seat"                        
        if boolYorN
          then putStr "\n" >> readFirstSeat audProps soldSeatsState
        else
          return $ Left ("mainMenuLoop")
      else return $ Right firstSeat

-- Verify the String entered for the first seat and return that Seat if it is valid.
-- The first seat is not checked for availability.                            
verifyFirstSeat :: AudProps -> String -> Either String Seat
verifyFirstSeat audProps seatString
  | seatString == "" =
      Left errorEmptyString
  | length seatString == 1 =
      Left errorOneChar
  | (\c -> not $ elem c ['A'..lastRow]) $ toUpper $ head seatString =
      Left errorRowOutOfRange
  | otherwise = let ival = readMaybe $ tail seatString :: Maybe Int
      in  case ival of
          Nothing   -> Left errorNonInt
          Just inum -> if inum > _nSeats_
            then Left errorSeatOutOfRange
            else  let firstChar = (toUpper $ head seatString)
                      map = if odd $ lookupRowInt firstChar
                            then (getMapSnakeNumOddRowUpToRight2Seat audProps)
                            else (getMapSnakeNumEvenRowUpToRight2Seat audProps)
                      sn = (computeSnakeNum firstChar firstChar inum)
                  in case Map.lookup sn map of
                    Nothing -> Left "Internal error." -- Should not be hit because the Map was initialized on valid data.
                    Just s -> Right s
  where
    lastRow = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" !! (_nRows_ - 1)
    errorPrefix = "Invalid seat specification: "
    errorEmptyString = errorPrefix
      ++ "received an empty string."
    errorOneChar = errorPrefix
      ++ "at least two characters are needed."
    errorRowOutOfRange = errorPrefix
      ++ printf "choose a row from A to %s." [lastRow]
    errorNonInt = errorPrefix
      ++ "an integer is needed for the seat number."
    errorSeatOutOfRange = errorPrefix
      ++ printf "choose a seat from 1 to %d." _nSeats_  

readRemainingSeats :: AudProps -> SoldSeatsState -> Seat -> IO (Either String [Seat])
readRemainingSeats audProps soldSeatsState firstSeat = do
  hSetBuffering stdin NoBuffering
  putStr "\nIncluding the first seat, how many seats do you want to buy? "
  hFlush stdout
  inputStr <- getLine
  let verifySeats = verifyRemainingSeats audProps soldSeatsState firstSeat inputStr
  case verifySeats of
    Left (errStr) -> putStrLn errStr
                  >> readRemainingSeats audProps soldSeatsState firstSeat
    Right (seats) -> return $ Right seats  

-- Read the Int for all the seats to buy and return a list of all seats.
-- These are not checked for availability. Only that they are valid seats.
-- The first seat is includedin the list.
verifyRemainingSeats :: AudProps -> SoldSeatsState -> Seat -> String -> Either String [Seat]
verifyRemainingSeats audProps soldSeatsState firstSeat inputStr = 
  let inum = readMaybe inputStr :: Maybe Int
      -- maxSeatsPossible seats are including firstSeat snaking to the right/left front of the auditorium.
      firstSeatRowLetter = getSeatRow firstSeat
      snFs = getSnakeNum firstSeatRowLetter firstSeat
      maxSeatsPossible = 1 + (_nRows_*_nSeats_ - snFs)
      pAgetSeatFromSnakeNum = getSeatFromSnakeNum audProps firstSeatRowLetter 
  in
  case inum of
    Nothing -> Left "Invalid input. Please enter an integer."
    Just (inum) ->
      if inum < 1
        then Left "Invalid input. Please enter an integer greater than zero."
      else if inum > maxSeatsPossible
        then Left $ "Invalid input. Please enter an integer less than or equal to " ++ show maxSeatsPossible ++ "."
      else if inum == 1
        then Right [firstSeat]
      else
        Right $ pAgetSeatFromSnakeNum <$> [snFs..(snFs + inum - 1)]

printSalesReport :: AudProps -> SoldSeatsState -> IO ()
printSalesReport audProps soldSeatsState = do
  let
-- Format sales report like this:
-- Sales Report:
-- 140 seats of house capacity of 450 seats have been sold.
-- Total sales revenue = $1478.00.
    seatSumStr = printf "%d seats of the house capacity of %d seats have been sold."
                 (Set.size (getSoldSeats soldSeatsState))
                ((_nRows_ * _nSeats_) :: Int) -- Cast to Int so printf is happy.
    --sales = sum $ fmap (\seat -> getSeatPrice seat) (toList (getSoldSeats soldSeatsState))
    sales = sum $ getSeatPrice <$> (toList (getSoldSeats soldSeatsState))
    salesSumStr = printf "Total sales revenue = $%.2f." (sales :: Float) -- Cast to Float so printf is happy.
  putStrLn "\nSales Report:"
  putStrLn seatSumStr
  putStrLn salesSumStr

readSeatingStateFromFile :: String -> IO (Either String [String])
readSeatingStateFromFile filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Left(errorHeading ++ "File: \"" ++ filePath ++ "\" does not exist.")
    else do
      handle <- openFile filePath ReadMode
      fileContents <- hGetContents' handle
      hClose handle
      let linesOfFile = reverse.dropWhile null.reverse $ lines fileContents -- Remove trailing empty lines.
          verifyLines = verifySeatingState linesOfFile
      case verifyLines of
        Left (errStr) -> return $ Left (errorHeading ++ errStr)
        Right () -> return $ Right linesOfFile
  where
    errorHeading = "Error in \"readSeatingStateFromFile\":\n"

verifySeatingState :: [String] -> Either String ()
verifySeatingState linesSeatingStateFile
  | linesLengthRows /= _nRows_ =
      Left errorNrows
  | any (\line -> length line /= _nSeats_) linesSeatingStateFile =
      Left errorNseats
  | any (\line -> (any (\c -> c /= '*' && c /= '#') line)) linesSeatingStateFile =
      Left errorSeatChar
  | otherwise = Right ()
  where
    linesLengthRows = length linesSeatingStateFile
    errorHeading = "Error in \"verifySeatingState\":\n"
    errorNrows = errorHeading
      ++ "Invalid number of rows in seat map.\n"
      ++ printf "There must be exactly %d rows; Found %d." _nRows_ linesLengthRows
    errorNseats = errorHeading
      ++ "Invalid number of seats in seat map.\n"
      ++ printf "There must be exactly %d seats in each row." _nSeats_
    errorSeatChar = errorHeading
      ++ "Invalid character in seat map.\n"
      ++ "Each seat must be denoted by '#' (available) or '*' (sold)."

readSeatingPricesFromFile :: String -> IO (Either String [Float])
readSeatingPricesFromFile filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Left(errorHeading ++ "File: \"" ++ filePath ++ "\" does not exist.")
    else do
      handle <- openFile filePath ReadMode
      fileContents <- hGetContents' handle
      hClose handle
      let linesOfFile = reverse.dropWhile null.reverse $ lines fileContents -- Remove trailing empty lines.
          verifyLines = verifySeatingPrices linesOfFile
      case verifyLines of
        Left (errStr) -> return $ Left (errorHeading ++ errStr)
        Right () -> return $ Right ((\str -> read str :: Float) <$> linesOfFile)
  where
    errorHeading = "Error in \"readSeatingPricesFromFile\":\n"      

verifySeatingPrices :: [String] -> Either String ()
verifySeatingPrices linesSeatingPricesFile
  | linesLengthRows /= _nRows_ =
      Left errorNrows

    -- Drop leading and trailing spaces, tabs, and commas, then see if the remaining string contains a space, tab, or comma.
  | any (\line -> (any (\c -> c == ' ' || c == '\t' || c == ',')
        (dropWhile (\c -> c == ' ' || c == '\t' || c == ',')
          (reverse
            (dropWhile (\c -> c == ' ' || c == '\t' || c == ',')
              (reverse line))))))
        linesSeatingPricesFile =
      Left errorNseats

    -- See if the String can be read to a Float. A trailing comma will be caught here.
  | any (\line -> (readMaybe line :: Maybe Float) == Nothing) linesSeatingPricesFile =
      Left errorSeatPrice
  | otherwise = Right ()
  where
    linesLengthRows = length linesSeatingPricesFile
    errorHeading = "Error in \"verifySeatingPrices\":\n"
    errorNrows = errorHeading
      ++ "Invalid number of rows in seating prices data.\n"
      ++ printf "There must be exactly %d rows; Found %d." _nRows_ linesLengthRows
    errorNseats = errorHeading
      ++ "Invalid number of prices in seating prices data.\n"
      ++ printf "There must be exactly one number (price) in each row."
    errorSeatPrice = errorHeading
      ++ "Invalid number in seating prices data.\n" 

writeSoldSeatsStateToFile :: AudProps -> SoldSeatsState -> IO ()
writeSoldSeatsStateToFile audProps soldSeatsState = do
  writeFile _SeatingStateFilePath_ $ unlines $ getListOfRowsOfSeatStates audProps soldSeatsState

-- Return True if user types Y, False if N, loop for Y or N otherwise.
readYorN :: String -> IO (Bool)
readYorN questionStringNoQmark = do
  putStr $ questionStringNoQmark ++ " (y/n)? "
  hFlush stdout
  char <- getChar
  case char of
    c | c `elem` ['N', 'n'] -> return False
    c | c `elem` ['Y', 'y'] -> return True
    _ ->  putStrLn "\n\nInvalid selection: please enter y/n."
          >> readYorN questionStringNoQmark  

-- Debugging Tools:

printAudProps :: AudProps -> IO ()
printAudProps audProps = do
    let
      allSeats = getAudSeats audProps
      seatingStateNum = (\line -> show $ 
        (\seat -> (getSeatRow seat, getSeatNum seat, getSnakeNum 'A' seat, getSnakeNum 'B' seat))
        <$> line) <$> allSeats
      mO = getMapSnakeNumOddRowUpToRight2Seat audProps
      mE = getMapSnakeNumEvenRowUpToRight2Seat audProps
    --mapM_ putStrLn seatingStateNum
    --writeFile _TestSeatingPricesOutputFilePath_ $ unlines seatingStateNum
    printMapContents mO
    printMapContents mE

printMapContents :: (Show k, Show v) => Map.Map k v -> IO ()
printMapContents m = do
  let entries = Map.toList m
      formattedEntries = map formatEntry entries
      output = intercalate ", " formattedEntries
  putStrLn output

formatEntry :: (Show k, Show v) => (k, v) -> String
formatEntry (key, value) = show key ++ " -> " ++ show value

debugGetSeatFromSnakeNum :: AudProps -> [Int] -> IO ()
debugGetSeatFromSnakeNum audProps snakeNums = do
  putStrLn "Debugging getSeatFromSnakeNum:"
  let pAORgetSeatFromSnakeNum = getSeatFromSnakeNum audProps 'A'
      pAERgetSeatFromSnakeNum = getSeatFromSnakeNum audProps 'B'
  forM_ snakeNums $ \num -> do
    let resultOR = pAORgetSeatFromSnakeNum num
        resultER = pAERgetSeatFromSnakeNum num
    putStrLn $ "SnakeNumOR: " ++ show num ++ " -> Result: " ++ show resultOR
    putStrLn $ "SnakeNumER: " ++ show num ++ " -> Result: " ++ show resultER

