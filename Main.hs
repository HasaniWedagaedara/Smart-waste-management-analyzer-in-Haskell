module Main where

import DataTypes
import Processing
import IOHandler
import Utils
import System.IO

main :: IO ()
main = do
  putStrLn "========================================"
  putStrLn " SMART WASTE MANAGEMENT ANALYZER"
  putStrLn " Advanced Waste Classification System"
  putStrLn "========================================"
  putStrLn ""
  
  
  putStrLn "Loading waste data from CSV..."
  records <- loadWasteData "sample_data.csv"
  
  if null records
    then putStrLn "Error: No data loaded. Check your CSV file."
    else do
      putStrLn $ "Successfully loaded " ++ show (length records) ++ " records."
      putStrLn $ "Total waste: " ++ show (roundTo 2 (sumWeights records)) ++ " kg"
      putStrLn $ "Date range: " ++ show (minimum (map wrDate records)) 
                ++ " to " ++ show (maximum (map wrDate records))
      
      
      menuLoop records


menuLoop :: [WasteRecord] -> IO ()
menuLoop records = do
  putStrLn "\n========================================"
  putStrLn "ANALYSIS OPTIONS:"
  putStrLn "1.  Analyze waste by area"
  putStrLn "2.  Analyze waste by type (all 18 types)"
  putStrLn "3.  Analyze by category (Organic/Industrial/etc.)"
  putStrLn "4.  View recycling & organic ratios"
  putStrLn "5.  View top waste-generating areas"
  putStrLn "6.  Generate collection recommendations"
  putStrLn "7.  Identify special handling requirements"
  putStrLn "8.  Analyze hazardous waste distribution"
  putStrLn "9.  Environmental impact assessment"
  putStrLn "10. View area statistics"
  putStrLn "11. Identify peak waste periods"
  putStrLn "12. Run complete analysis"
  putStrLn "13. Exit"
  putStrLn "========================================"
  putStr "Enter your choice (1-13): "
  hFlush stdout
  
  choice <- getLine
  case choice of
    "1" -> do
      let areaReports = analyzeByArea records
      displayAreaReport areaReports
      menuLoop records
      
    "2" -> do
      let typeReports = analyzeByWasteType records
      displayWasteTypeReport typeReports
      menuLoop records
      
    "3" -> do
      let categoryReports = analyzeByCategory records
      displayCategoryReport categoryReports
      menuLoop records
      
    "4" -> do
      let ratio = calculateRecyclingRatio records
      displayRecyclingRatio ratio
      menuLoop records
      
    "5" -> do
      putStr "How many top areas to display? "
      hFlush stdout
      nStr <- getLine
      let n = read nStr :: Int
          topAreas = topWasteAreas n records
      displayTopAreas n topAreas
      menuLoop records
      
    "6" -> do
      let plans = generateCollectionPlans records
      displayCollectionPlans plans
      menuLoop records
      
    "7" -> do
      let specialAreas = areasWithSpecialWaste records
      displaySpecialHandling specialAreas
      menuLoop records
      
    "8" -> do
      let hazardous = analyzeHazardousWaste records
      displayHazardousWaste hazardous
      menuLoop records
      
    "9" -> do
      let impact = assessEnvironmentalImpact records
      displayEnvironmentalImpact impact
      menuLoop records
      
    "10" -> do
      putStr "Enter area name: "
      hFlush stdout
      areaName <- getLine
      let stats = getAreaStatistics areaName records
      displayStatistics areaName stats
      menuLoop records
      
    "11" -> do
      let peaks = identifyPeakPeriods records
      displayPeakPeriods peaks
      menuLoop records
      
    "12" -> do
      runCompleteAnalysis records
      menuLoop records
      
    "13" -> do
      putStrLn "\nThank you for using Smart Waste Management Analyzer!"
      putStrLn "Promoting sustainable waste management through data analytics."
      putStrLn "Goodbye!"
      
    _ -> do
      putStrLn "Invalid choice. Please try again."
      menuLoop records

runCompleteAnalysis :: [WasteRecord] -> IO ()
runCompleteAnalysis records = do
  putStrLn "\n========================================"
  putStrLn " COMPLETE WASTE MANAGEMENT ANALYSIS"
  putStrLn "========================================"
  
  
  putStrLn $ "\n[DATASET SUMMARY]"
  putStrLn $ "Total records analyzed: " ++ show (length records)
  putStrLn $ "Total waste collected: " ++ show (roundTo 2 (sumWeights records)) ++ " kg"
  putStrLn $ "Unique areas: " ++ show (length (groupByKey wrArea records))
  putStrLn $ "Unique waste types: " ++ show (length (groupByKey wrWasteType records))
  

  let areaReports = analyzeByArea records
  displayAreaReport areaReports

  let categoryReports = analyzeByCategory records
  displayCategoryReport categoryReports
  
 
  putStrLn "\n========== TOP 10 WASTE TYPES BY VOLUME =========="
  let typeReports = take 10 $ sortByWeight (analyzeByWasteType records)
  displayWasteTypeReport typeReports
  
  
  let ratio = calculateRecyclingRatio records
  displayRecyclingRatio ratio
  

  let impact = assessEnvironmentalImpact records
  displayEnvironmentalImpact impact
  
  let topAreas = topWasteAreas 5 records
  displayTopAreas 5 topAreas
  
 
  let specialAreas = areasWithSpecialWaste records
  putStrLn "\n[SPECIAL HANDLING AREAS]:"
  displaySpecialHandling specialAreas
  

  let hazardous = analyzeHazardousWaste records
  if null hazardous
    then putStrLn "\n[OK] No hazardous waste detected."
    else displayHazardousWaste hazardous
  

  let plans = generateCollectionPlans records
  displayCollectionPlans plans
  
  putStrLn "\n========================================"
  putStrLn " ANALYSIS COMPLETE"
  putStrLn "========================================"


sortByWeight :: [Report] -> [Report]
sortByWeight reports = sortBy compareWeight reports
  where
    compareWeight (WasteTypeSummary _ w1) (WasteTypeSummary _ w2) = compare w2 w1
    compareWeight _ _ = EQ
    
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy _ [] = []
    sortBy cmp (x:xs) = sortBy cmp lesser ++ [x] ++ sortBy cmp greater
      where
        lesser = [y | y <- xs, cmp y x == LT]
        greater = [y | y <- xs, cmp y x /= LT]