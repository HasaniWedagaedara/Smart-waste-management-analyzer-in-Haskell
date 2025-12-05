module Utils where

import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)
import Data.Char (toLower, isSpace)
import DataTypes


parseWasteType :: String -> WasteType
parseWasteType s = case normalizeString s of
  "biodegradable"           -> Biodegradable
  "bulkywaste"              -> BulkyWaste
  "c&dwaste"                -> CAndDWaste
  "cdwaste"                 -> CAndDWaste
  "canddwaste"              -> CAndDWaste
  "industrialwaste"         -> IndustrialWaste
  "industrialsludgewaste"   -> IndustrialSludgeWaste
  "mesuring"                -> Mesuring
  "measuring"               -> Mesuring
  "msw"                     -> MSW
  "nonbiodegradable"        -> NonBiodegradable
  "polythene&regiform"      -> PolytheneAndRegiform
  "polytheneandregiform"    -> PolytheneAndRegiform
  "sanitarywaste"           -> SanitaryWaste
  "sawdust"                 -> SawDust
  "slaughterhousewaste"     -> SlaughterHouseWaste
  "slaughterwaste"          -> SlaughterHouseWaste
  "soil"                    -> Soil
  "soilwithwaste"           -> SoilWithWaste
  "sortedorganicwaste"      -> SortedOrganicWaste
  "organicwaste"            -> SortedOrganicWaste
  "specialwaste"            -> SpecialWaste
  "wooddebris"              -> WoodDebris
  "woodtrank"               -> WoodTrunk
  "woodtrunk"               -> WoodTrunk
  other                     -> UnknownWaste other
  where
    normalizeString = filter (not . isSpace) . map toLower


categorizeWaste :: WasteType -> WasteCategory
categorizeWaste Biodegradable        = OrganicCategory
categorizeWaste SortedOrganicWaste   = OrganicCategory
categorizeWaste SlaughterHouseWaste  = OrganicCategory
categorizeWaste SawDust              = OrganicCategory
categorizeWaste NonBiodegradable     = InorganicCategory
categorizeWaste PolytheneAndRegiform = InorganicCategory
categorizeWaste IndustrialWaste      = IndustrialCategory
categorizeWaste IndustrialSludgeWaste= IndustrialCategory
categorizeWaste SpecialWaste         = IndustrialCategory
categorizeWaste CAndDWaste           = ConstructionCategory
categorizeWaste Soil                 = ConstructionCategory
categorizeWaste SoilWithWaste        = ConstructionCategory
categorizeWaste WoodDebris           = ConstructionCategory
categorizeWaste WoodTrunk            = ConstructionCategory
categorizeWaste MSW                  = MunicipalCategory
categorizeWaste BulkyWaste           = MunicipalCategory
categorizeWaste SanitaryWaste        = MunicipalCategory
categorizeWaste Mesuring             = MunicipalCategory
categorizeWaste (UnknownWaste _)     = MunicipalCategory


isRecyclable :: WasteType -> Bool
isRecyclable PolytheneAndRegiform = True  -- Can be recycled
isRecyclable WoodDebris           = True  -- Can be reused
isRecyclable WoodTrunk            = True  -- Can be reused
isRecyclable SawDust              = True  -- Can be composted
isRecyclable Soil                 = True  -- Can be reused
isRecyclable _                    = False


isOrganic :: WasteType -> Bool
isOrganic Biodegradable        = True
isOrganic SortedOrganicWaste   = True
isOrganic SlaughterHouseWaste  = True
isOrganic SawDust              = True
isOrganic _                    = False


requiresSpecialHandling :: WasteType -> Bool
requiresSpecialHandling IndustrialWaste       = True
requiresSpecialHandling IndustrialSludgeWaste = True
requiresSpecialHandling SpecialWaste          = True
requiresSpecialHandling SlaughterHouseWaste   = True
requiresSpecialHandling SanitaryWaste         = True
requiresSpecialHandling _                     = False


isHazardous :: WasteType -> Bool
isHazardous IndustrialSludgeWaste = True
isHazardous SpecialWaste          = True
isHazardous SanitaryWaste         = True
isHazardous _                     = False


mean :: [Double] -> Double
mean [] = 0.0
mean xs = sum xs / fromIntegral (length xs)


median :: [Double] -> Double
median [] = 0.0
median xs = sorted !! middle
  where
    sorted = sort xs
    len = length sorted
    middle = len `div` 2


stdDev :: [Double] -> Double
stdDev [] = 0.0
stdDev xs = sqrt (sum squaredDiffs / fromIntegral (length xs))
  where
    avg = mean xs
    squaredDiffs = map (\x -> (x - avg) ^ 2) xs


calculateStats :: [Double] -> Statistics
calculateStats [] = Statistics 0 0 0 0 0 0
calculateStats xs = Statistics
  { statMean   = mean xs
  , statMedian = median xs
  , statStdDev = stdDev xs
  , statMin    = minimum xs
  , statMax    = maximum xs
  , statTotal  = sum xs
  }


groupByKey :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
groupByKey f = groupBy (\x y -> f x == f y) . sortBy (comparing f)


sumWeights :: [WasteRecord] -> Double
sumWeights = foldr ((+) . wrWeight) 0.0


roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * factor)) / factor
  where factor = 10.0 ^ n


determinePriority :: Double -> Priority
determinePriority weight
  | weight > 1000  = Critical
  | weight > 500   = High
  | weight > 100   = Medium
  | otherwise      = Low


findDominantType :: [WasteRecord] -> WasteType
findDominantType [] = UnknownWaste "None"
findDominantType records = fst $ maximum typeCounts
  where
    typeGroups = groupByKey wrWasteType records
    typeCounts = map (\g@(r:_) -> (wrWasteType r, length g)) typeGroups


percentage :: Double -> Double -> Double
percentage part total = if total > 0 
                        then roundTo 2 ((part / total) * 100)
                        else 0.0