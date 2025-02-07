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

# Transform to meter-based CRS for accurate distance calculations
crashes_with_district <- st_transform(crashes_with_district, 3857)

# Query OSM for Lake Shore Drive
lake_shore_streets <- opq(bbox = CHICAGO_BBOX) %>%
  add_osm_feature(key = 'highway') %>%
  add_osm_feature(key = 'name', value = 'Lake Shore', value_exact = FALSE) %>%
  osmdata_sf() %>%
  pluck("osm_lines") %>%
  {rename(., street_name = name)} %>%
  st_transform(crs = 3857)

# Create buffer around Lake Shore Drive and identify intersecting crashes
buffer <- st_buffer(lake_shore_streets, dist = BUFFER_DISTANCE_M)
intersects <- st_intersects(crashes_with_district, buffer, sparse = FALSE)

# Filter crashes within buffer
crashes_with_district$is_lakeshore_crash <- apply(intersects, 1, any)
lakeshore_crashes <- crashes_with_district %>%
  filter(is_lakeshore_crash)

# Calculate economic damages
lakeshore_crashes <- calculate_economic_damage(lakeshore_crashes)

# Create district summaries for Lake Shore Drive crashes
lakeshore_senate_summary <- create_district_summary(lakeshore_crashes, "senate_district")
lakeshore_house_summary <- create_district_summary(lakeshore_crashes, "house_district")

# Export summaries
export_summary(lakeshore_senate_summary, "lakeshore_senate_summary.csv")
export_summary(lakeshore_house_summary, "lakeshore_house_summary.csv")

# Create fatality subset
lakeshore_fatalities <- lakeshore_crashes %>%
  filter(fatality_count > 0)

# Transform back to WGS84 for mapping
lakeshore_fatalities <- st_transform(lakeshore_fatalities, 4326)
lake_shore_streets <- st_transform(lake_shore_streets, 4326)
buffer <- st_transform(buffer, 4326)

# Create Lake Shore Drive fatality map
lakeshore_fatality_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add the buffer area
  addPolygons(
    data = buffer,
    fillColor = "blue",
    fillOpacity = 0.2,
    color = "blue",
    weight = 2
  ) %>%
  # Add Lake Shore Drive
  addPolylines(
    data = lake_shore_streets,
    color = "red",
    weight = 3
  ) %>%
  # Add fatality markers
  addCircleMarkers(
    data = lakeshore_fatalities,
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    radius = ~fatality_count * 5,
    color = "#FF0000",
    fillOpacity = 0.6,
    popup = ~paste(
      "Date:", CRASH_DATE, "<br>",
      "Fatalities:", fatality_count, "<br>",
      "Type:", FIRST_CRASH_TYPE
    )
  ) %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 12)

# Export map
export_map(lakeshore_fatality_map, "lakeshore_fatalities_map.html")

# Create heatmap of all crashes
lakeshore_heatmap <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = buffer,
    fillColor = "blue",
    fillOpacity = 0.1,
    color = "blue",
    weight = 2
  ) %>%
  addPolylines(
    data = lake_shore_streets,
    color = "red",
    weight = 3
  ) %>%
  addHeatmap(
    data = st_transform(lakeshore_crashes, 4326),
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    intensity = ~1,
    blur = 20,
    max = 0.05,
    radius = 15
  ) %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 12)

# Export heatmap
export_map(lakeshore_heatmap, "lakeshore_crashes_heatmap.html")

# Print summary statistics
cat("\nLake Shore Drive Analysis Complete\n")
cat("Total Crashes:", sum(lakeshore_crashes$crash_count, na.rm = TRUE), "\n")
cat("Total Fatalities:", sum(lakeshore_crashes$fatality_count, na.rm = TRUE), "\n")
cat("Total Economic Damage: $", format(sum(lakeshore_crashes$estimated_economic_damages, na.rm = TRUE), big.mark = ","), "\n")
