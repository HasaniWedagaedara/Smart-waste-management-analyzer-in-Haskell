# Smart Waste Management Analyzer

## Group Members
- Kuruwitaarachchi K.A.D.T.T. - EG/2020/4038
- Madhumali W.H. - EG/2020/4050
- Naveendi H.T. - EG/2020/4087
- Dewapura K.K - EG/2020/4338

## Project Title
Smart Waste Management Analyzer in Haskell

## Problem Description
This system analyzes municipal solid waste data from 2012-2018 to optimize waste collection and improve recycling rates. Cities face challenges with overflowing bins, inefficient collection routes, and poor waste segregation. By processing historical waste records, the system identifies patterns, prioritizes collection areas, and provides actionable recommendations for sustainable waste management.

## Instructions to Run
1. Ensure Haskell (GHC/GHCi) is installed on your system
2. Download or clone the project files
3. Navigate to the project directory in terminal
4. Run the program using:
   ```bash
   ghci Main.hs
   > main
   ```
5. Follow the interactive menu (1-13) for different analyses

## Sample Input/Output

### Input CSV Format:
```text
Area,Date,WasteType,Weight
Boralesgamuwa,2015-03-15,MSW,450.5
Dehiwala,2015-03-15,SortedOrganicWaste,320.0
```

### Program Output:
```text
========================================
SMART WASTE MANAGEMENT ANALYZER
========================================
Loading waste data from CSV...
Successfully loaded 15991 records.
Total waste: 556,423,230 kg

ANALYSIS OPTIONS:
1. Analyze waste by area
2. Analyze waste by type
...

Enter your choice (1-13): 1

========== WASTE BY AREA ==========
Boralesgamuwa: 125,450.25 kg
Dehiwala: 98,320.50 kg
...
```

## Functional Programming Concepts Used

### 1. Pure Functions
```haskell
-- Same input always gives same output
sumWeights :: [WasteRecord] -> Double
sumWeights = foldr ((+) . wrWeight) 0.0
```

### 2. Algebraic Data Types
```haskell
data WasteType = Biodegradable | MSW | IndustrialWaste
data Report = AreaSummary String Double | WasteTypeSummary WasteType Double
```

### 3. Higher-Order Functions
```haskell
analyzeByArea = map summarizeArea . groupByKey wrArea
```

### 4. Pattern Matching
```haskell
parseWasteType "biodegradable" = Biodegradable
parseWasteType "msw" = MSW
parseWasteType other = UnknownWaste other
```

### 5. Immutability
All data structures are immutable throughout the processing pipeline.

### 6. Modular Design
Separate modules for data types, processing logic, I/O operations, and utilities ensure maintainability and testability.
