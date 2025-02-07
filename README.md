# ILGA District Crashes Analysis

Analysis of traffic crashes in Illinois General Assembly districts within Chicago, including specific analyses of high-crash corridors.

## Data Sources

- Traffic Crashes Data: [City of Chicago Data Portal](https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if/explore/query/)
- Traffic Fatalities Data: [Vision Zero Chicago Traffic Fatalities](https://data.cityofchicago.org/Transportation/Traffic-Crashes-Vision-Zero-Chicago-Traffic-Fatali/gzaz-isa6)
- Illinois House Districts Shapefiles
- Illinois Senate Districts Shapefiles
- Chicago Ward Boundaries

## Repository Structure

```
ilga_district_crashes/
├── data/
│   ├── raw/              # Original data files
│   │   ├── shapefiles/   # Geographic boundary files
│   │   │   ├── house/    # House district shapefiles
│   │   │   ├── senate/   # Senate district shapefiles
│   │   │   └── wards/    # Ward boundary files
│   │   └── crashes/      # Original crash data
│   └── processed/        # Processed data files
├── R/                    # Analysis scripts
├── output/              # Generated files
│   ├── maps/            # Generated maps
│   └── summaries/       # Summary CSV files
└── README.md
```

## Analysis Scripts

- `utils.R`: Shared functions and constants
- `city_crashes.R`: Citywide crash analysis
- `lakeshore_crashes.R`: Lake Shore Drive corridor analysis
- `archer_crashes.R`: Archer Avenue corridor analysis
- `pulaski_crashes.R`: Pulaski Road corridor analysis
- `lasalle_crashes.R`: LaSalle ramp analysis

## Setup

1. Download the traffic crashes dataset from the [City of Chicago Data Portal](https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if/explore/query/)
2. Download the fatality data from the [Vision Zero Chicago Traffic Fatalities](https://data.cityofchicago.org/Transportation/Traffic-Crashes-Vision-Zero-Chicago-Traffic-Fatali/gzaz-isa6/explore/query/SELECT%0A%20%20%60person_id%60%2C%0A%20%20%60crash_date%60%2C%0A%20%20%60crash_location%60%2C%0A%20%20%60victim%60%2C%0A%20%20%60crash_circumstances%60%2C%0A%20%20%60longitude%60%2C%0A%20%20%60latitude%60%2C%0A%20%20%60geocoded_column%60%0AORDER%20BY%20%60crash_date%60%20DESC%20NULL%20FIRST/page/filter) dataset
   - Required fields for fatality data:
     * person_id
     * crash_date
     * crash_location
     * victim
     * crash_circumstances
     * longitude
     * latitude
   - Export as CSV using the "Export" button in the top right
3. Place both downloaded CSVs in `data/raw/crashes/`
4. Place district shapefiles in appropriate directories:
   - House district files in `data/raw/shapefiles/house/`
   - Senate district files in `data/raw/shapefiles/senate/`
   - Ward files in `data/raw/shapefiles/wards/`
4. Install required R packages:
   ```R
   install.packages(c(
     "dplyr", "readr", "purrr", "geosphere", "ggplot2",
     "ggmap", "magrittr", "leaflet", "sf", "osmdata",
     "leaflet.extras", "lubridate", "tidyr", "knitr"
   ))
   ```

## Running the Analysis

Each script can be run independently to analyze different aspects of the crash data:

1. Run `utils.R` first to load shared functions
2. Run any of the analysis scripts:
   - `city_crashes.R` for citywide analysis
   - `lakeshore_crashes.R` for Lake Shore Drive analysis
   - `archer_crashes.R` for Archer Avenue analysis
   - `pulaski_crashes.R` for Pulaski Road analysis
   - `lasalle_crashes.R` for LaSalle ramp analysis

## Outputs

- Summary CSVs in `output/summaries/`
  - District-level summaries
  - Corridor-specific analyses
  - Crash statistics and economic impact estimates
- Interactive maps in `output/maps/`
  - Fatality visualizations
  - Crash density heatmaps
  - District-level analysis maps

## Data Processing Notes

- Date range: 2019-01-01 to 2023-12-31
- Economic damage estimates based on NSC injury cost estimates
- Spatial analysis uses 750ft buffers for corridor analysis
- All spatial data is transformed to EPSG:4326 for consistency

## Required Data Fields

The traffic crashes CSV should contain the following key fields:
- CRASH_RECORD_ID
- CRASH_DATE
- LONGITUDE
- LATITUDE
- INJURIES_TOTAL
- INJURIES_INCAPACITATING
- FIRST_CRASH_TYPE
- HIT_AND_RUN_I

## License

This project is open source and available under the MIT License.
