# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset

install.packages("sf")

#Problem 1

# Problem 1A: Reproduce and explain the engine size andhighway fuel economy
# plot
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds
ggplot(data = vehicles, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(data = vehicles, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Engine Size and Highway Fuel Economy by Drive Type",
    x = "Engine Size (L)",
    y = "Highway MPG",
    color = "Drive Type"
  ) +
  theme_bw()

# there are three fitted lines because vehicles are grouped
# by drive type (4-wheel, front-wheel, and rear-wheel drive).
# each line shows the linear relationship between engine size 
# and highway fuel economy for that drive type 

# Problem 1B: Reproduce/explain the diamond carat and price plot
ggplot(data = gems, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Diamond Carat and Price",
    x = "Carat",
    y = "Price ($)",
    fill = "Drive Type"
  ) +
  theme_bw()

# there is a posivite relationship between fiamond carat
# and price. As carat goes up, diamond price generally increases
# the linear regresion lines shows this trend.
# the bars have equal total heights because position = "fill"
# converts the counts into proportions. The original number 
# of vehicles in each drive type can be different.
# Therefore, equal heights do not mean same sample size.

# Problem 1C: Count retained and excudede records
set.seed(2026)

gems_small <- gems %>%
  slice_sample(n = 5000)
# count number of retained diamonds
nrow(gems_small)
# count number of excluded diamonds
nrow(gems) - nrow(gems_small)

# create the fine histogram
p_fine <- ggplot(data = gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right"
  ) +
  labs(
    title = "Diamond Carat Distribution (Bin Width = 0.01)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

p_fine

# create coarse histogram 
# set the same x-axis range for both histograms 
carat_range <- range(gems_small$carat)
p_coarse <- ggplot(data = gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right"
  ) +
  coord_cartesian(xlim = carat_range) +
  labs(
    title = "Diamond Carat Distribution (Bin Width = 0.5)",
    x = "Carat",
    y = "Count"
  ) +
  theme_bw()

p_coarse

# count diamonds with 0.99 and 1.00 carat
gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  count(carat)

# the narrower bins make it easier to see detailed patterns
# in the diamond carat distribution. tehey show clusters and 
# peaks that are less noticable with wider bins. the wider
# bins combine more values together and hide some detail.

# Problem 2: Load the Chicago Public Library locations
library_locations <- read_csv(file.choose())

# inspect library location data
glimpse(library_locations)

# inspect library coordinates 
head(library_locations$LOCATION)

# separate latitude and longitude 
library_coords <- library_locations %>%
  mutate(LOCATION = gsub("[()]", "", LOCATION)) %>%
  separate(LOCATION,
           into = c("latitude", "longitude"),
           sep = ",\\s*",
           convert = TRUE)

# check separated coords
head(library_coords[, c("latitude", "longitude")])

# check for missing coords
sum(is.na(library_coords$latitude))
sum(is.na(library_coords$longitude))

# load chicago community area boundries
community_areas <- st_read(file.choose())
# inspect chicago community area boundries 
glimpse(community_areas)
# check coords reference system
st_crs(community_areas)
# convert areas to a projected CRS in meters
community_areas_m <- st_transform(community_areas, 26916)
st_crs(community_areas_m)

# convert library locs into spatial points
library_points <- st_as_sf(
  library_coords,
  coords = c("longitude", "latitude"),
  crs = 4326
)
# project library points to meters 
sites_m <- st_transform(library_points, 26916)
st_crs(sites_m)
nrow(sites_m)

# Problem 2: Map all Chicago Public Library locations
p_sites <- ggplot() +
  geom_sf(data = community_areas_m, fill = "gray95",
          color = "gray50") +
  geom_sf(data = sites_m, color = "blue", size = 2) +
  coord_sf(datum = NA) +
  labs(title = "Chicago Public Library Locations",
       subtitle = "All 82 library branches",
       caption = "Source: City of Chicago Data Portal; accessed November 8, 2026.",
       x = "Easting (meters)",
       y = "Northing (meters)") +
  theme_bw()

# Display the map
p_sites

# create 500-meter buffers around lib
buffer_500 <- st_buffer(sites_m, dist = 500)
# combine overlapping buffer areas 
buffer_500_union <- st_union(buffer_500)

# map combined 500-meter buffer areas 
p_buffers <- ggplot() +
  geom_sf(data = community_areas_m, fill = "gray95",
          color = "gray60") +
  geom_sf(data = buffer_500_union, fill = "lightblue",
          color = "blue", alpha = 0.5) +
  geom_sf(data = sites_m, color = "red", size = 1.5) +
  coord_sf(datum = NA) +
  labs(title = "Areas Within 500 Meters of a Chicago Library",
       subtitle = "Overlapping buffer areas are counted once",
       caption = "Source: City of Chicago Data Portal; accessed November 8, 2026.",
       x = "Easting (meters)",
       y = "Northing (meters)") +
  theme_bw()

# Display the buffer map
p_buffers

# create 1000 meter buffers around libs
buffer_1000 <- st_buffer(sites_m, dist = 1000)
# combine them
buffer_1000_union <- st_union(buffer_1000)

# calculate area covered by 500 meter buffers
area_500 <- st_area(buffer_500_union)
# calc area covered by 1000-meter buffers
area_1000 <- st_area(buffer_1000_union)

# convert square meters to kilometers
area_500_km2 <- as.numeric(area_500) / 1000000
area_1000_km2 <- as.numeric(area_1000) / 1000000

# create buffer coverage summary table 
buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = c(area_500_km2, area_1000_km2)
)

buffer_summary

# create unique ids for each library
sites_m$site_id <- seq_len(nrow(sites_m))
# check IDs and names
head(sites_m[, c("site_id", "BRANCH")])

# check number of library records
nrow(library_locations)
nrow(sites_m)

# count missing coords
sum(is.na(library_coords$latitude))
sum(is.na(library_coords$longitude))

# check for dupes 
sum(duplicated(library_coords[, c("latitude", "longitude")]))

# summarize library data cleaning
cleaning_summary <- data.frame(
  records_before = 82,
  records_after = 82,
  missing_latitude = 0,
  missing_longitude = 0,
  duplicate_coordinates = 0,
  records_removed = 0
)

cleaning_summary

# document original data sources
data_sources <- data.frame(
  dataset = c("Chicago Public Library Locations",
              "Chicago Community Area Boundaries"),
  publisher = c("City of Chicago",
                "City of Chicago"),
  source = c(
    "https://data.cityofchicago.org/d/wa2i-tm5d",
    "https://data.cityofchicago.org/d/m39i-3ntz"
  ),
  access_date = c("2026-11-08", "2026-11-08")
)

# save data sources
write_csv(data_sources, "data_sources.csv")

# create cleaned lib locs with unique IDS
cleaned_locations <- library_coords %>%
  mutate(site_id = row_number()) %>%
  select(site_id, BRANCH, ADDRESS, latitude, longitude)

# save cleaned lib locs
write_csv(cleaned_locations, "cleaned_library_locations.csv")

# save projecyed library locs
st_write(sites_m,
         "problem2_spatial_outputs.gpkg",
         layer = "sites_m",
         delete_dsn = TRUE)

# save combined 500-meter buffer
st_write(buffer_500_union,
         "problem2_spatial_outputs.gpkg",
         layer = "buffer_500",
         append = TRUE)

# save combined 1000 meter buffer
st_write(buffer_1000_union,
         "problem2_spatial_outputs.gpkg",
         layer = "buffer_1000",
         append = TRUE)

# save buffer area summary
write_csv(buffer_summary, "buffer_summary.csv")

# verify saved spatial layers
st_layers("problem2_spatial_outputs.gpkg")

# update saved library locs w unique ids
st_write(
  sites_m,
  "problem2_spatial_outputs.gpkg",
  layer = "sites_m",
  delete_layer = TRUE
)

# verify undated spatial layers
st_layers("problem2_spatial_outputs.gpkg")

# Problem 3

# load Chicago Police District boundaries
police_districts <- st_read(file.choose())

# inspect police district boundaries 
glimpse(police_districts)

# convert police districts to a projected CRS in meters
police_districts_m <- st_transform(police_districts, 26916)
# check projected CRS
st_crs(police_districts_m)

# check for repated numbers
police_districts_m %>%
  count(dist_num) %>%
  filter(n > 1)

# combine pieces with the same district number
police_districts_m <- police_districts_m %>%
  group_by(dist_num) %>%
  summarise(
    dist_label = first(dist_label),
    do_union = TRUE
  )

nrow(police_districts_m)

police_districts_m %>%
  count(dist_num) %>%
  filter(n > 1)

# check boundary validity
sum(!st_is_valid(police_districts_m))

# check for missing districts IDs and names
sum(is.na(police_districts_m$dist_num))
sum(is.na(police_districts_m$dist_label))

# find which police dis contains each library
site_matches <- st_join(
  sites_m,
  police_districts_m,
  join = st_within,
  left = TRUE
)

# count how many districts each lib matches
site_match_audit <- site_matches %>%
  st_drop_geometry() %>%
  count(site_id, name = "n_matches")

site_match_audit

# summarize lib-to-district matches
site_match_audit %>%
  count(n_matches, name = "number_of_sites")

# save site match audit
write_csv(site_match_audit, "site_match_audit.csv")

# count libs in each police district
district_counts <- site_matches %>%
  st_drop_geometry() %>%
  group_by(dist_num, dist_label) %>%
  summarise(n_sites = n(), .groups = "drop")

district_counts

# include all police districts, even those with no libraries 
district_counts <- police_districts_m %>%
  st_drop_geometry() %>%
  select(dist_num, dist_label) %>%
  left_join(
    site_matches %>%
      st_drop_geometry() %>%
      group_by(dist_num, dist_label) %>%
      summarise(n_sites = n(), .groups = "drop"),
    by = c("dist_num", "dist_label")
  ) %>%
  mutate(n_sites = replace_na(n_sites, 0)) %>%
  rename(
    district_id = dist_num,
    district_name = dist_label
  )

district_counts

# calc the area of each police district
district_areas <- police_districts_m %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1000000) %>%
  st_drop_geometry() %>%
  select(dist_num, area_km2) %>%
  rename(district_id = dist_num)

district_areas

# combine counts and areas, then calc density
district_counts <- district_counts %>%
  left_join(district_areas, by = "district_id") %>%
  mutate(sites_per_km2 = n_sites / area_km2)

district_counts

# check total lib count
sum(district_counts$n_sites)
# check every district has a positive area
sum(district_counts$area_km2 <= 0)
# check for missing vals
sum(is.na(district_counts))

# attach library counts to district boundaries
district_map <- police_districts_m %>%
  left_join(district_counts, by = c("dist_num" = "district_id"))

# check the map data
glimpse(district_map)

# map number of libraries in each district
p_count <- ggplot(data = district_map) +
  geom_sf(aes(fill = n_sites), color = "white") +
  scale_fill_viridis_c(
    name = "Number of Libraries",
    option = "C"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Chicago Public Libraries by Police District",
    subtitle = "Number of library locations in each district",
    caption = "Source: City of Chicago Data Portal; accessed November 8, 2026. 0 of 82 library locations were left unassigned.",
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

p_count

# map library density by police district
p_density <- ggplot(data = district_map) +
  geom_sf(aes(fill = sites_per_km2), color = "white") +
  scale_fill_viridis_c(
    name = "Libraries per km²",
    option = "C"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Library Density by Chicago Police District",
    subtitle = "Number of libraries per square kilometer",
    caption = "Source: City of Chicago Data Portal; accessed November 8, 2026. 0 of 82 library locations were left unassigned.",
    x = "Easting (meters)",
    y = "Northing (meters)"
  ) +
  theme_bw()

p_density

# save district count and density table 
write_csv(district_counts, "district_counts.csv")

# save the library count map
ggsave("p_count.png", p_count, width = 10, height = 8)

# save the library density map
ggsave("p_density.png", p_density, width = 10, height = 8)

# save police district spatial data
st_write(
  district_map,
  "problem3_spatial_outputs.gpkg",
  layer = "district_map",
  delete_dsn = TRUE
)

# save library locations
st_write(
  sites_m,
  "problem3_spatial_outputs.gpkg",
  layer = "sites_m",
  append = TRUE
)

# add police district data source
data_sources <- bind_rows(
  data_sources,
  data.frame(
    dataset = "Chicago Police District Boundaries",
    publisher = "City of Chicago",
    source = "https://data.cityofchicago.org/d/fthy-xz3r",
    access_date = "2026-11-08"
  )
)

write_csv(data_sources, "data_sources.csv")

# check saved output files
file.exists("site_match_audit.csv")
file.exists("district_counts.csv")
file.exists("data_sources.csv")
file.exists("p_count.png")
file.exists("p_density.png")
file.exists("problem3_spatial_outputs.gpkg")


# save district count and density table
write_csv(district_counts, "district_counts.csv")

# check that it saved
file.exists("district_counts.csv")

ggsave("p_count.png", p_count, width = 10, height = 8)
ggsave("p_density.png", p_density, width = 10, height = 8)



