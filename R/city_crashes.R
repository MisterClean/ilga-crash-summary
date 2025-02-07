#!/usr/bin/env Rscript

# Source utility functions
source("R/utils.R")

# Load required libraries
load_libraries()

# Read and process crash data
crashes <- load_crash_data("data/raw/crashes/traffic_crashes.csv")
crashes_sf <- convert_to_sf(crashes)

# Read and process fatality data
crash_fatalities <- load_fatality_data("data/raw/crashes/crash_fatalities.csv")
crash_fatalities_sf <- convert_to_sf(crash_fatalities, "Longitude", "Latitude")

# Combine crashes and fatalities
combined_crashes_sf <- bind_rows(crashes_sf, crash_fatalities_sf)

# Load district shapefiles
senate_districts <- load_district_shapefile(
  "data/raw/shapefiles/senate/senate_districts.shp",
  new_name = "senate_district"
)

house_districts <- load_district_shapefile(
  "data/raw/shapefiles/house/house_districts.shp",
  new_name = "house_district"
)

# Spatial join crashes with districts
crashes_with_district <- st_join(combined_crashes_sf, senate_districts, left = FALSE)
crashes_with_district <- st_join(crashes_with_district, house_districts, left = FALSE)

# Calculate economic damages
crashes_with_district <- calculate_economic_damage(crashes_with_district)

# Create district summaries
senate_district_summary <- create_district_summary(crashes_with_district, "senate_district")
house_district_summary <- create_district_summary(crashes_with_district, "house_district")

# Export summaries
export_summary(senate_district_summary, "senate_district_summary.csv")
export_summary(house_district_summary, "house_district_summary.csv")

# Create fatality subset
fatalities <- crashes_with_district %>%
  filter(fatality_count > 0)

# Create district centroids for labels
senate_districts_centroids <- senate_districts %>%
  st_centroid() %>%
  mutate(label = as.character(senate_district))

house_districts_centroids <- house_districts %>%
  st_centroid() %>%
  mutate(label = as.character(house_district))

# Create and export maps
fatality_senate_map <- create_district_map(
  fatalities,
  senate_districts,
  "senate_district",
  senate_districts_centroids
)
export_map(fatality_senate_map, "senate_fatalities_map.html")

fatality_house_map <- create_district_map(
  fatalities,
  house_districts,
  "house_district",
  house_districts_centroids
)
export_map(fatality_house_map, "house_fatalities_map.html")

# Print summary statistics
cat("\nAnalysis Complete\n")
cat("Total Crashes:", sum(crashes_with_district$crash_count, na.rm = TRUE), "\n")
cat("Total Fatalities:", sum(crashes_with_district$fatality_count, na.rm = TRUE), "\n")
cat("Total Economic Damage: $", format(sum(crashes_with_district$estimated_economic_damages, na.rm = TRUE), big.mark = ","), "\n")
