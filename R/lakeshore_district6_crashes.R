#!/usr/bin/env Rscript

# Source utility functions (which now loads libraries directly)
source("R/utils.R")

# Read and process crash data
crashes <- load_crash_data("data/raw/crashes/traffic_crashes.csv")
crashes_sf <- convert_to_sf(crashes)

# Read and process fatality data
crash_fatalities <- load_fatality_data("data/raw/crashes/crash_fatalities.csv")
crash_fatalities_sf <- convert_to_sf(crash_fatalities, "Longitude", "Latitude")

# Combine crashes and fatalities
combined_crashes_sf <- bind_rows(crashes_sf, crash_fatalities_sf)

# Load senate district shapefile and filter for district 6
senate_districts <- load_district_shapefile(
  "data/raw/shapefiles/senate/senate_districts.shp",
  new_name = "senate_district"
)
district_6 <- senate_districts %>%
  filter(senate_district == "6")

# Spatial join crashes with district 6
crashes_with_district <- st_join(combined_crashes_sf, district_6, left = FALSE)

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

# Filter crashes within buffer and calculate counts
crashes_with_district$is_lakeshore_crash <- apply(intersects, 1, any)
lakeshore_crashes <- crashes_with_district %>%
  filter(is_lakeshore_crash) %>%
  mutate(
    crash_count = 1,
    fatality_count = ifelse(!is.na(FATALITY_VICTIM) & FATALITY_VICTIM != "", 1, 0)
  )

# Create fatality subset
lakeshore_fatalities <- lakeshore_crashes %>%
  filter(fatality_count > 0)

# Transform back to WGS84 for mapping
lakeshore_fatalities <- st_transform(lakeshore_fatalities, 4326)
lake_shore_streets <- st_transform(lake_shore_streets, 4326)
buffer <- st_transform(buffer, 4326)
district_6 <- st_transform(district_6, 4326)

# Create Lake Shore Drive district 6 fatality map
district6_fatality_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Add district 6
  addPolygons(
    data = district_6,
    fillColor = "#9370DB",  # Medium purple
    fillOpacity = 0.3,
    color = "#4B0082",  # Indigo
    weight = 2,
    label = ~paste("Senate District", senate_district)
  ) %>%
  # Add the buffer area
  addPolygons(
    data = buffer,
    fillColor = "#CCCCCC",  # Light gray
    fillOpacity = 0.2,
    color = "#CCCCCC",  # Light gray
    weight = 2
  ) %>%
  # Add Lake Shore Drive
  addPolylines(
    data = lake_shore_streets,
    color = "#CCCCCC",
    weight = 3
  ) %>%
  # Add clustered fatality circles
  addCircleMarkers(
    data = lakeshore_fatalities,
    lng = ~st_coordinates(geometry)[, 1],
    lat = ~st_coordinates(geometry)[, 2],
    radius = 6,  # Base size for individual points
    color = "red",
    fillColor = "red",
    fillOpacity = 0.6,
    stroke = TRUE,
    weight = 1,
    popup = ~paste(
      "Date:", CRASH_DATE, "<br>",
      "Fatalities:", fatality_count, "<br>",
      "Type:", FIRST_CRASH_TYPE
    ),
    clusterOptions = markerClusterOptions(
      maxClusterRadius = 50 * 0.0003,  # Approximate conversion of 50ft to degrees
      spiderfyOnMaxZoom = TRUE,
      iconCreateFunction = JS("
        function(cluster) {
          var count = cluster.getChildCount();
          var size = Math.min(50, 20 + Math.sqrt(count) * 8);
          return new L.DivIcon({
            html: '<div style=\"background-color: rgba(255, 0, 0, 0.6); width: ' + size + 'px; height: ' + size + 'px; margin-left: -' + (size/2) + 'px; margin-top: -' + (size/2) + 'px; border-radius: 50%; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold; border: 2px solid rgba(255, 0, 0, 0.8);\">' + count + '</div>',
            className: 'marker-cluster-custom',
            iconSize: L.point(size, size)
          });
        }
      ")
    )
  ) %>%
  setView(lng = -87.6298, lat = 41.8781, zoom = 12)

# Export map
export_map(district6_fatality_map, "lakeshore_district6_fatalities_map.html")

# Print summary statistics
cat("\nLake Shore Drive District 6 Analysis Complete\n")
cat("Total Crashes:", sum(lakeshore_crashes$crash_count, na.rm = TRUE), "\n")
cat("Total Fatalities:", sum(lakeshore_crashes$fatality_count, na.rm = TRUE), "\n")
