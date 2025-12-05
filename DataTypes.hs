module DataTypes where

import Data.Time (Day)

data WasteRecord = WasteRecord
  { wrArea       :: String
  , wrDate       :: Day
  , wrWasteType  :: WasteType
  , wrWeight     :: Double  -- in kg
  } deriving (Show, Eq)

data WasteType 
  = Biodegradable
  | BulkyWaste
  | CAndDWaste              -- Construction & Demolition
  | IndustrialWaste
  | IndustrialSludgeWaste
  | Mesuring               
  | MSW                     -- Municipal Solid Waste
  | NonBiodegradable
  | PolytheneAndRegiform    
  | SanitaryWaste
  | SawDust
  | SlaughterHouseWaste
  | Soil
  | SoilWithWaste
  | SortedOrganicWaste
  | SpecialWaste
  | WoodDebris
  | WoodTrunk
  | UnknownWaste String     
  deriving (Show, Eq, Ord)

data WasteCategory
  = OrganicCategory       -- Biodegradable, Organic, Slaughter, Saw Dust
  | InorganicCategory     -- Non-biodegradable, Polythene
  | IndustrialCategory    -- Industrial waste, Sludge, Special
  | ConstructionCategory  -- C&D, Soil, Wood
  | MunicipalCategory     -- MSW, Bulky, Sanitary
  deriving (Show, Eq, Ord)


data Report
  = AreaSummary String Double            -- Area name, total weight
  | WasteTypeSummary WasteType Double    -- Waste type, total weight
  | CategorySummary WasteCategory Double -- Waste category, total weight
  | DateRangeSummary Day Day Double      -- Start date, end date, total
  | RecyclingRatio Double Double Double  -- Recyclable %, Organic %, Non-recyclable %
  deriving (Show, Eq)


data Priority
  = Critical   -- > 1000 kg/week
  | High       -- 500-1000 kg/week
  | Medium     -- 100-500 kg/week
  | Low        -- < 100 kg/week
  deriving (Show, Eq, Ord, Enum)

data CollectionPlan = CollectionPlan
  { cpArea           :: String
  , cpFrequency      :: Int          -- times per week
  , cpPriority       :: Priority
  , cpDominantType   :: WasteType    -- Most common waste type
  , cpSpecialHandling:: Bool         -- Requires special handling
  , cpReason         :: String
  } deriving (Show, Eq)

data Statistics = Statistics
  { statMean   :: Double
  , statMedian :: Double
  , statStdDev :: Double
  , statMin    :: Double
  , statMax    :: Double
  , statTotal  :: Double
  } deriving (Show, Eq)

data EnvironmentalImpact = EnvironmentalImpact
  { eiOrganicPercent      :: Double
  , eiHazardousPercent    :: Double
  , eiRecyclablePercent   :: Double
  , eiLandfillPercent     :: Double
  , eiRecommendations     :: [String]
  } deriving (Show, Eq)