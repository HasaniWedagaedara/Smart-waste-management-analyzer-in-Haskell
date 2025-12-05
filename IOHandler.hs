module IOHandler where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import DataTypes
import Utils
import System.IO

parseCSVLine :: String -> Maybe WasteRecord
parseCSVLine line = 
  case splitOn ',' line of
    [area, dateStr, wasteTypeStr, weightStr] -> do
      date <- parseDate dateStr
      weight <- readMaybe (trim weightStr)
      return WasteRecord
        { wrArea = trim area
        , wrDate = date
        , wrWasteType = parseWasteType wasteTypeStr
        , wrWeight = weight
        }
    _ -> Nothing
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
    
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(val, "")] -> Just val
      _           -> Nothing

-- | Parse date string (format: YYYY-MM-DD)
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where rest = splitOn delim cs

-- | Load and parse CSV file
loadWasteData :: FilePath -> IO [WasteRecord]
loadWasteData filepath = do
  contents <- readFile filepath
  let linesOfFile = lines contents
      dataLines = if null linesOfFile then [] else tail linesOfFile  -- Skip header
      parsedRecords = mapMaybe parseCSVLine dataLines
  return parsedRecords
  where
    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f xs = [y | x <- xs, Just y <- [f x]]

displayAreaReport :: [Report] -> IO ()
displayAreaReport reports = do
  putStrLn "\n========== WASTE BY AREA =========="
  putStrLn "Area                    | Total Weight (kg)"
  putStrLn "----------------------------------------"
  mapM_ printAreaReport reports
  where
    printAreaReport (AreaSummary area weight) = 
      putStrLn $ padRight 24 area ++ "| " ++ show (roundTo 2 weight)
    printAreaReport _ = return ()
    
    padRight n s = s ++ replicate (n - length s) ' '

displayWasteTypeReport :: [Report] -> IO ()
displayWasteTypeReport reports = do
  putStrLn "\n========== WASTE BY TYPE =========="
  putStrLn "Type                    | Total Weight (kg)"
  putStrLn "----------------------------------------"
  mapM_ printTypeReport reports
  where
    printTypeReport (WasteTypeSummary wtype weight) = 
      putStrLn $ padRight 24 (show wtype) ++ "| " ++ show (roundTo 2 weight)
    printTypeReport _ = return ()
    
    padRight n s = s ++ replicate (n - length s) ' '

displayRecyclingRatio :: Report -> IO ()
displayRecyclingRatio (RecyclingRatio recyclable organic nonRecyclable) = do
  putStrLn "\n========== WASTE COMPOSITION ANALYSIS =========="
  putStrLn $ "[RECYCLABLE] Recyclable materials:   " ++ show recyclable ++ "%"
  putStrLn $ "[ORGANIC]    Organic/Compostable:    " ++ show organic ++ "%"
  putStrLn $ "[LANDFILL]   Non-recyclable waste:   " ++ show nonRecyclable ++ "%"
  putStrLn "\nInterpretation:"
  if recyclable + organic >= 60
    then putStrLn "[OK] Good waste segregation - majority can be diverted from landfill"
    else putStrLn "[WARNING] Consider improving source segregation to reduce landfill waste"
displayRecyclingRatio _ = return ()

displayCollectionPlans :: [CollectionPlan] -> IO ()
displayCollectionPlans plans = do
  putStrLn "\n========== COLLECTION RECOMMENDATIONS =========="
  putStrLn "Area                 | Priority  | Frequency | Special | Reason"
  putStrLn "------------------------------------------------------------------------"
  mapM_ printPlan plans
  where
    printPlan (CollectionPlan area freq priority dominantType specialHandling reason) = 
      putStrLn $ padRight 20 area ++ " | " 
              ++ padRight 9 (show priority) ++ " | " 
              ++ padRight 9 (show freq ++ "/week") ++ " | " 
              ++ padRight 7 (if specialHandling then "Yes" else "No") ++ " | "
              ++ reason
    
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '

displayTopAreas :: Int -> [(String, Double)] -> IO ()
displayTopAreas n topAreas = do
  putStrLn $ "\n========== TOP " ++ show n ++ " WASTE GENERATING AREAS =========="
  putStrLn "Rank | Area                    | Total Weight (kg)"
  putStrLn "------------------------------------------------------"
  mapM_ printRanked (zip [1..] topAreas)
  where
    printRanked (rank, (area, weight)) = 
      putStrLn $ padRight 5 (show rank) ++ "| " 
              ++ padRight 24 area ++ "| " 
              ++ show (roundTo 2 weight)
    
    padRight n s = s ++ replicate (n - length s) ' '

displayStatistics :: String -> Statistics -> IO ()
displayStatistics label stats = do
  putStrLn $ "\n========== STATISTICS: " ++ label ++ " =========="
  putStrLn $ "Mean:   " ++ show (roundTo 2 (statMean stats)) ++ " kg"
  putStrLn $ "Median: " ++ show (roundTo 2 (statMedian stats)) ++ " kg"
  putStrLn $ "Std Dev:" ++ show (roundTo 2 (statStdDev stats)) ++ " kg"
  putStrLn $ "Min:    " ++ show (roundTo 2 (statMin stats)) ++ " kg"
  putStrLn $ "Max:    " ++ show (roundTo 2 (statMax stats)) ++ " kg"
  putStrLn $ "Total:  " ++ show (roundTo 2 (statTotal stats)) ++ " kg"

displayCategoryReport :: [Report] -> IO ()
displayCategoryReport reports = do
  putStrLn "\n========== WASTE BY CATEGORY =========="
  putStrLn "Category                | Total Weight (kg)"
  putStrLn "----------------------------------------"
  mapM_ printCategoryReport reports
  where
    printCategoryReport (CategorySummary category weight) = 
      putStrLn $ padRight 24 (show category) ++ "| " ++ show (roundTo 2 weight)
    printCategoryReport _ = return ()
    
    padRight n s = s ++ replicate (n - length s) ' '

displaySpecialHandling :: [(String, [WasteType])] -> IO ()
displaySpecialHandling specialAreas = do
  putStrLn "\n========== SPECIAL HANDLING REQUIREMENTS =========="
  if null specialAreas || all (null . snd) specialAreas
    then putStrLn "[OK] No special handling required."
    else mapM_ printSpecial (filter (not . null . snd) specialAreas)
  where
    printSpecial (area, types) = do
      putStrLn $ "\n[AREA] " ++ area ++ ":"
      mapM_ (\t -> putStrLn $ "   - " ++ show t) types

displayHazardousWaste :: [(String, Double)] -> IO ()
displayHazardousWaste hazardous = do
  putStrLn "\n========== HAZARDOUS WASTE ANALYSIS =========="
  putStrLn "Area                    | Hazardous Waste (kg)"
  putStrLn "-----------------------------------------------"
  mapM_ printHazardous hazardous
  where
    printHazardous (area, weight) = 
      putStrLn $ padRight 24 area ++ "| " ++ show (roundTo 2 weight)
    
    padRight n s = s ++ replicate (n - length s) ' '

displayEnvironmentalImpact :: EnvironmentalImpact -> IO ()
displayEnvironmentalImpact impact = do
  putStrLn "\n========== ENVIRONMENTAL IMPACT ASSESSMENT =========="
  putStrLn $ "[ORGANIC]    Organic/Compostable:    " ++ show (eiOrganicPercent impact) ++ "%"
  putStrLn $ "[RECYCLABLE] Recyclable/Reusable:    " ++ show (eiRecyclablePercent impact) ++ "%"
  putStrLn $ "[HAZARDOUS]  Hazardous:              " ++ show (eiHazardousPercent impact) ++ "%"
  putStrLn $ "[LANDFILL]   Landfill Required:      " ++ show (eiLandfillPercent impact) ++ "%"
  
  let recs = eiRecommendations impact
  if null recs
    then putStrLn "\n[OK] No specific recommendations."
    else do
      putStrLn "\n[RECOMMENDATIONS]:"
      mapM_ (\r -> putStrLn $ "   * " ++ r) recs

displayPeakPeriods :: [(Day, Double)] -> IO ()
displayPeakPeriods peaks = do
  putStrLn "\n========== TOP 10 PEAK WASTE DAYS =========="
  putStrLn "Date           | Total Weight (kg)"
  putStrLn "-----------------------------------"
  mapM_ printPeak peaks
  where
    printPeak (date, weight) = 
      putStrLn $ show date ++ " | " ++ show (roundTo 2 weight)