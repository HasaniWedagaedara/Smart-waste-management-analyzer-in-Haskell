module Processing where

import Data.List (sortBy, nub)
import Data.Ord (comparing, Down(..))
import Data.Time (Day, diffDays)
import DataTypes
import Utils


analyzeByArea :: [WasteRecord] -> [Report]
analyzeByArea records = map summarizeArea areaGroups
  where
    areaGroups = groupByKey wrArea records
    summarizeArea group@(r:_) = AreaSummary (wrArea r) (sumWeights group)
    summarizeArea [] = AreaSummary "Unknown" 0.0


analyzeByWasteType :: [WasteRecord] -> [Report]
analyzeByWasteType records = map summarizeType typeGroups
  where
    typeGroups = groupByKey wrWasteType records
    summarizeType group@(r:_) = WasteTypeSummary (wrWasteType r) (sumWeights group)
    summarizeType [] = WasteTypeSummary (UnknownWaste "Empty") 0.0


analyzeByCategory :: [WasteRecord] -> [Report]
analyzeByCategory records = map summarizeCategory categoryGroups
  where
    recordsWithCategory = map (\r -> (categorizeWaste (wrWasteType r), r)) records
    categoryGroups = groupByKey fst recordsWithCategory
    summarizeCategory group@((cat, _):_) = 
      CategorySummary cat (sumWeights (map snd group))
    summarizeCategory [] = CategorySummary MunicipalCategory 0.0


calculateRecyclingRatio :: [WasteRecord] -> Report
calculateRecyclingRatio records = 
  RecyclingRatio recyclablePercent organicPercent nonRecyclablePercent
  where
    totalWeight = sumWeights records
    recyclableWeight = sumWeights (filter (isRecyclable . wrWasteType) records)
    organicWeight = sumWeights (filter (isOrganic . wrWasteType) records)
    nonRecyclableWeight = totalWeight - recyclableWeight - organicWeight
    
    recyclablePercent = percentage recyclableWeight totalWeight
    organicPercent = percentage organicWeight totalWeight
    nonRecyclablePercent = percentage nonRecyclableWeight totalWeight


topWasteAreas :: Int -> [WasteRecord] -> [(String, Double)]
topWasteAreas n records = take n sortedAreas
  where
    areaGroups = groupByKey wrArea records
    areaTotals = map (\g@(r:_) -> (wrArea r, sumWeights g)) areaGroups
    sortedAreas = sortBy (comparing (Down . snd)) areaTotals


generateCollectionPlans :: [WasteRecord] -> [CollectionPlan]
generateCollectionPlans records = map createPlan areaGroups
  where
    areaGroups = groupByKey wrArea records
    createPlan group@(r:_) = 
      let area = wrArea r
          totalWeight = sumWeights group
          weeklyWeight = estimateWeeklyWeight group
          priority = determinePriority weeklyWeight
          frequency = recommendFrequency priority
          dominantType = findDominantType group
          specialHandling = any (requiresSpecialHandling . wrWasteType) group
          reason = reasonForPriority priority weeklyWeight dominantType
      in CollectionPlan area frequency priority dominantType specialHandling reason
    createPlan [] = CollectionPlan "Unknown" 1 Low (UnknownWaste "None") False "No data"


estimateWeeklyWeight :: [WasteRecord] -> Double
estimateWeeklyWeight [] = 0.0
estimateWeeklyWeight records = 
  let totalWeight = sumWeights records
      dates = map wrDate records
      daySpan = if null dates then 1 
                else fromIntegral (diffDays (maximum dates) (minimum dates)) + 1
      weeks = max 1 (daySpan / 7.0)
  in totalWeight / weeks


recommendFrequency :: Priority -> Int
recommendFrequency Critical = 7  -- Daily
recommendFrequency High     = 5  -- 5 times per week
recommendFrequency Medium   = 3  -- 3 times per week
recommendFrequency Low      = 1  -- Once per week


reasonForPriority :: Priority -> Double -> WasteType -> String
reasonForPriority Critical w wt = 
  "Critical: " ++ show (roundTo 1 w) ++ " kg/week - Daily collection required. Dominant: " ++ show wt
reasonForPriority High w wt = 
  "High volume: " ++ show (roundTo 1 w) ++ " kg/week. Main type: " ++ show wt
reasonForPriority Medium w wt = 
  "Moderate: " ++ show (roundTo 1 w) ++ " kg/week. Type: " ++ show wt
reasonForPriority Low w wt = 
  "Low volume: " ++ show (roundTo 1 w) ++ " kg/week. Type: " ++ show wt


filterByDateRange :: Day -> Day -> [WasteRecord] -> [WasteRecord]
filterByDateRange startDate endDate = filter inRange
  where inRange r = wrDate r >= startDate && wrDate r <= endDate


analyzeTrends :: [WasteRecord] -> [(Day, Double)]
analyzeTrends records = map dayTotal dateGroups
  where
    dateGroups = groupByKey wrDate records
    dayTotal group@(r:_) = (wrDate r, sumWeights group)
    dayTotal [] = (read "2024-01-01", 0.0)


getAreaStatistics :: String -> [WasteRecord] -> Statistics
getAreaStatistics area records = calculateStats weights
  where
    areaRecords = filter (\r -> wrArea r == area) records
    weights = map wrWeight areaRecords

areasWithSpecialWaste :: [WasteRecord] -> [(String, [WasteType])]
areasWithSpecialWaste records = map extractSpecial areaGroups
  where
    areaGroups = groupByKey wrArea records
    extractSpecial group@(r:_) = 
      let area = wrArea r
          specialTypes = nub [wrWasteType rec | rec <- group, 
                              requiresSpecialHandling (wrWasteType rec)]
      in (area, specialTypes)
    extractSpecial [] = ("Unknown", [])


analyzeHazardousWaste :: [WasteRecord] -> [(String, Double)]
analyzeHazardousWaste records = 
  sortBy (comparing (Down . snd)) areaHazardous
  where
    hazardousRecords = filter (isHazardous . wrWasteType) records
    areaGroups = groupByKey wrArea hazardousRecords
    areaHazardous = map (\g@(r:_) -> (wrArea r, sumWeights g)) areaGroups


assessEnvironmentalImpact :: [WasteRecord] -> EnvironmentalImpact
assessEnvironmentalImpact records = EnvironmentalImpact
  { eiOrganicPercent = organicPct
  , eiHazardousPercent = hazardousPct
  , eiRecyclablePercent = recyclablePct
  , eiLandfillPercent = landfillPct
  , eiRecommendations = generateRecommendations organicPct hazardousPct recyclablePct
  }
  where
    totalWeight = sumWeights records
    organicWeight = sumWeights (filter (isOrganic . wrWasteType) records)
    hazardousWeight = sumWeights (filter (isHazardous . wrWasteType) records)
    recyclableWeight = sumWeights (filter (isRecyclable . wrWasteType) records)
    landfillWeight = totalWeight - organicWeight - recyclableWeight
    
    organicPct = percentage organicWeight totalWeight
    hazardousPct = percentage hazardousWeight totalWeight
    recyclablePct = percentage recyclableWeight totalWeight
    landfillPct = percentage landfillWeight totalWeight


generateRecommendations :: Double -> Double -> Double -> [String]
generateRecommendations organic hazardous recyclable = concat
  [ if organic > 40 then ["High organic waste - implement composting program"] else []
  , if hazardous > 10 then ["Significant hazardous waste - requires specialized handling"] else []
  , if recyclable < 20 then ["Low recycling rate - enhance segregation at source"] else []
  , if recyclable > 50 then ["Excellent recycling rate - maintain current practices"] else []
  ]


identifyPeakPeriods :: [WasteRecord] -> [(Day, Double)]
identifyPeakPeriods records = take 10 $ sortBy (comparing (Down . snd)) dailyTotals
  where
    dailyTotals = analyzeTrends records


compareWasteTypes :: String -> String -> [WasteRecord] -> (Statistics, Statistics)
compareWasteTypes area1 area2 records = (stats1, stats2)
  where
    stats1 = getAreaStatistics area1 records
    stats2 = getAreaStatistics area2 records