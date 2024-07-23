rm(list=ls())

# Cleaning (sourced from TidyTuesday)

library(tidyverse)
library(jsonlite)
library(janitor)
library(here)
library(fs)

working_dir <- here::here()
csv_files_dir <- here::here("csv_files")
time_viz_dir <- here::here("time_viz")
location_viz_dir <- here::here("location_viz")
ml_results_dir <- here::here("machine_learning_results")

eclipse_cities_url_2024 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2024.json"
eclipse_cities_url_2023 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json"

eclipse_cities_2024 <- jsonlite::fromJSON(eclipse_cities_url_2024) |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  tidyr::unnest_wider(eclipse, names_sep = "_")

eclipse_total_2024 <- eclipse_cities_2024 |> 
  dplyr::filter(!is.na(eclipse_6))

eclipse_partial_2024 <- eclipse_cities_2024 |> 
  dplyr::filter(is.na(eclipse_6)) |> 
  dplyr::select(-eclipse_6)

eclipse_cities_2023 <- jsonlite::fromJSON(eclipse_cities_url_2023) |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  tidyr::unnest_wider(eclipse, names_sep = "_")

eclipse_annular_2023 <- eclipse_cities_2023 |> 
  dplyr::filter(!is.na(eclipse_6))

eclipse_partial_2023 <- eclipse_cities_2023 |> 
  dplyr::filter(is.na(eclipse_6)) |> 
  dplyr::select(-eclipse_6)

readr::write_csv(
  eclipse_total_2024,
  fs::path(csv_files_dir, "eclipse_total_2024.csv")
)
readr::write_csv(
  eclipse_partial_2024,
  fs::path(csv_files_dir, "eclipse_partial_2024.csv")
)
readr::write_csv(
  eclipse_annular_2023,
  fs::path(csv_files_dir, "eclipse_annular_2023.csv")
)
readr::write_csv(
  eclipse_partial_2023,
  fs::path(csv_files_dir, "eclipse_partial_2023.csv")
)

# Load data

eclipse_total_2024 <- readr::read_csv(fs::path(csv_files_dir, "eclipse_total_2024.csv"))
eclipse_partial_2024 <- readr::read_csv(fs::path(csv_files_dir, "eclipse_partial_2024.csv"))
eclipse_annular_2023 <- readr::read_csv(fs::path(csv_files_dir, "eclipse_annular_2023.csv"))
eclipse_partial_2023 <- readr::read_csv(fs::path(csv_files_dir, "eclipse_partial_2023.csv"))

# Summary statistics

summary(eclipse_total_2024)
summary(eclipse_partial_2024)
summary(eclipse_annular_2023)
summary(eclipse_partial_2023)

# Combining data and adding columns to differentiate

eclipse_annular_2023 <- eclipse_annular_2023 %>% mutate(year = 2023, type = "Annular")
eclipse_partial_2023 <- eclipse_partial_2023 %>% mutate(year = 2023, type = "Partial")
eclipse_total_2024 <- eclipse_total_2024 %>% mutate(year = 2024, type = "Total")
eclipse_partial_2024 <- eclipse_partial_2024 %>% mutate(year = 2024, type = "Partial")

combined_data <- bind_rows(
  eclipse_annular_2023,
  eclipse_partial_2023,
  eclipse_total_2024,
  eclipse_partial_2024
)

combined_data <- combined_data %>%
  mutate(
    eclipse_1 = as.POSIXct(eclipse_1, format = "%H:%M:%S"),
    eclipse_2 = as.POSIXct(eclipse_2, format = "%H:%M:%S"),
    eclipse_3 = as.POSIXct(eclipse_3, format = "%H:%M:%S"),
    eclipse_4 = as.POSIXct(eclipse_4, format = "%H:%M:%S"),
    eclipse_5 = as.POSIXct(eclipse_5, format = "%H:%M:%S"),
    eclipse_6 = ifelse(type == "Partial", NA, as.POSIXct(eclipse_6, format = "%H:%M:%S")),
    duration = ifelse(
      type == "Partial",
      as.numeric(difftime(eclipse_5, eclipse_1, units = "mins")),
      as.numeric(difftime(eclipse_6, eclipse_1, units = "mins"))
    )
  )

eclipse_annular_2023 <- combined_data %>%
  filter(type == "Annular")
eclipse_partial_2023 <- combined_data %>%
  filter(type == "Partial", year == 2023)
eclipse_total_2024 <- combined_data %>%
  filter(type == "Total")
eclipse_partial_2024 <- combined_data %>%
  filter(type == "Partial", year == 2023)

# Visualizing Latitude and Longitude

library(ggplot2)
library(maps)

lat_plot <- ggplot(combined_data, aes(x = lat, fill = interaction(type, year))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Latitudes for Eclipses 2023-2024", x = "Latitude", y = "Count", fill = "Eclipse Type and Year")

lon_plot <- ggplot(combined_data, aes(x = lon, fill = interaction(type, year))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Longitudes for Eclipses 2023-2024", x = "Longitude", y = "Count", fill = "Eclipse Type and Year")

ggsave(filename = fs::path(location_viz_dir, "latitude_distribution.png"), plot = lat_plot, width = 10, height = 6)
ggsave(filename = fs::path(location_viz_dir, "longitude_distribution.png"), plot = lon_plot, width = 10, height = 6)

# Analyzing timing for partial eclipses

partial_data <- combined_data %>%
  filter(type == "Partial")

time_labels <- function(x) format(x, "%H:%M:%S")

partials_first_contact <- ggplot(partial_data, aes(x = eclipse_1, fill = as.factor(year))) +
  geom_histogram(binwidth = 3600, position = "dodge") +
  scale_x_datetime(labels = time_labels) +
  theme_minimal() +
  labs(title = "Distribution of Partial Eclipse Start Times 2023-2024", x = "Start Time", y = "Count", fill = "Year")

partials_first_50_percent <- ggplot(partial_data, aes(x = eclipse_2, fill = as.factor(year))) +
  geom_histogram(binwidth = 3600, position = "dodge") +
  scale_x_datetime(labels = time_labels) +
  theme_minimal() +
  labs(title = "Distribution of Partial Eclipse First Half Times 2023-2024", x = "Maximum Time", y = "Count", fill = "Year")

partials_100_percent <- ggplot(partial_data, aes(x = eclipse_3, fill = as.factor(year))) +
  geom_histogram(binwidth = 3600, position = "dodge") +
  scale_x_datetime(labels = time_labels) +
  theme_minimal() +
  labs(title = "Distribution of Partial Eclipse Maximum Times 2023-2024", x = "End Time", y = "Count", fill = "Year")

partials_last_50_percent <- ggplot(partial_data, aes(x = eclipse_4, fill = as.factor(year))) +
  geom_histogram(binwidth = 3600, position = "dodge") +
  scale_x_datetime(labels = time_labels) +
  theme_minimal() +
  labs(title = "Distribution of Partial Eclipse Last Half Times 2023-2024", x = "Maximum Time", y = "Count", fill = "Year")

partials_last_contact <- ggplot(partial_data, aes(x = eclipse_5, fill = as.factor(year))) +
  geom_histogram(binwidth = 3600, position = "dodge") +
  scale_x_datetime(labels = time_labels) +
  theme_minimal() +
  labs(title = "Distribution of Partial Eclipse End Times 2023-2024", x = "Start Time", y = "Count", fill = "Year")

# Saving partial eclipse timing plots

ggsave(filename = fs::path(time_viz_dir, "partial_eclipses_start_times_histogram.png"), plot = partials_first_contact, width = 10, height = 6)
ggsave(filename = fs::path(time_viz_dir, "partial_eclipses_first_half_times_histogram.png"), plot = partials_first_50_percent, width = 10, height = 6)
ggsave(filename = fs::path(time_viz_dir, "partial_eclipses_maximum_times_histogram.png"), plot = partials_100_percent, width = 10, height = 6)
ggsave(filename = fs::path(time_viz_dir, "partial_eclipses_last_half_times_histogram.png"), plot = partials_last_50_percent, width = 10, height = 6)
ggsave(filename = fs::path(time_viz_dir, "partial_eclipses_end_times_histogram.png"), plot = partials_last_contact, width = 10, height = 6)

# Analyzing eclipse durations for annular and total eclipses

annular_total_data <- combined_data %>%
  filter(type %in% c("Annular", "Total"))

duration_hist_plot <- ggplot(annular_total_data, aes(x = duration, fill = interaction(type, year))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Eclipse Durations 2023-2024", x = "Duration (minutes)", y = "Count", fill = "Eclipse Type and Year")

duration_box_plot <- ggplot(annular_total_data, aes(x = interaction(type, year), y = duration, fill = interaction(type, year))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Eclipse Durations 2023-2024", x = "Eclipse Type and Year", y = "Duration (minutes)", fill = "Eclipse Type and Year")

# Saving eclipse duration plots

ggsave(filename = fs::path(time_viz_dir, "annular_total_eclipse_durations_histogram.png"), plot = duration_hist_plot, width = 10, height = 6)
ggsave(filename = fs::path(time_viz_dir, "annular_total_eclipse_durations_boxplot.png"), plot = duration_box_plot, width = 10, height = 6)

# Plotting eclipse location and duration maps

us_map <- map_data("state")

annular_2023_map <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(map_id = region), fill = "white", color = "black") +
  geom_point(data = eclipse_annular_2023, aes(x = lon, y = lat, color = duration), size = 0.5) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Geographical Distribution of Duration for Annular Eclipse 2023", x = "Longitude", y = "Latitude")

total_2024_map <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(map_id = region), fill = "white", color = "black") +
  geom_point(data = eclipse_total_2024, aes(x = lon, y = lat, color = duration), size = 0.5) +
  scale_color_gradient(low = "yellow", high = "blue") +
  labs(title = "Geographical Distribution of Duration for Total Eclipse 2024", x = "Longitude", y = "Latitude")

partial_2023_map <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(map_id = region), fill = "white", color = "black") +
  geom_point(data = eclipse_partial_2023, aes(x = lon, y = lat, color = duration), size = 0.1) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Geographical Distribution of Duration for Partial Eclipse 2023", x = "Longitude", y = "Latitude") + 
  coord_map(xlim = c(-176, -50), ylim = c(17.95, 71.25))

partial_2024_map <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(map_id = region), fill = "white", color = "black") +
  geom_point(data = eclipse_partial_2024, aes(x = lon, y = lat, color = duration), size = 0.1) +
  scale_color_gradient(low = "yellow", high = "blue") +
  labs(title = "Geographical Distribution of Duration for Partial Eclipse 2024", x = "Longitude", y = "Latitude") +
  coord_map(xlim = c(-176, -50), ylim = c(17.95, 71.25))

# Saving eclipse location and duration maps

ggsave(filename = file.path(location_viz_dir, "annular_eclipse_2023_map.png"), plot = annular_2023_map, width = 10, height = 7)
ggsave(filename = file.path(location_viz_dir, "total_eclipse_2024_map.png"), plot = total_2024_map, width = 10, height = 7)
ggsave(filename = file.path(location_viz_dir, "partial_eclipse_2023_map.png"), plot = partial_2023_map, width = 10, height = 7)
ggsave(filename = file.path(location_viz_dir, "partial_eclipse_2024_map.png"), plot = partial_2024_map, width = 10, height = 7)

# Predictive Modeling with Annular 2023

model <- lm(duration ~ lon + lat, data = eclipse_annular_2023)
summary(model)

model_output <- capture.output(summary(model))
model_filepath <- "machine_learning_results/regression_model_output.txt"
writeLines(model_output, model_filepath)
cat("Predictive model output has been written to", model_filepath, "\n")


png(filename = "machine_learning_results/regression_annular_23.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model)
dev.off()

# K-Means Clustering with Partial 2023

library(cluster)
kmeans_result <- kmeans(eclipse_partial_2023[, c("lon", "lat")], centers = 4)
eclipse_partial_2023$cluster <- kmeans_result$cluster

kmeans_partial_23 <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(x = long, y = lat, map_id = region), fill = "white", color = "black") +
  geom_point(data = eclipse_partial_2023, aes(x = lon, y = lat, color = as.factor(cluster))) +
  labs(title = "Clusters of Partial Eclipse 2023 Durations", x = "Longitude", y = "Latitude", color = "Cluster") +
  coord_fixed(xlim = c(-176, -50), ylim = c(17.95, 71.25)) +
  theme_minimal()

ggsave(filename = file.path(ml_results_dir, "kmeans_partial_eclipse_2024_map.png"), plot = kmeans_partial_23, width = 10, height = 7)

# Isolation Forest for anomaly detection with Partial 2024

library(isotree)
iso_forest <- isolation.forest(eclipse_partial_2024[, c("lon", "lat", "duration")])
eclipse_partial_2024$anomaly_score <- predict(iso_forest, eclipse_partial_2024[, c("lon", "lat", "duration")])

filtered_anomalies_partial_24 <- subset(eclipse_partial_2024, anomaly_score > 0.6)

isolation_forest_partial_24 <- ggplot() +
  geom_map(data = us_map, map = us_map, aes(x = long, y = lat, map_id = region), fill = "white", color = "black") +
  geom_point(data = filtered_anomalies_partial_24, aes(x = lon, y = lat, color = anomaly_score)) +
  labs(title = "Anomaly Detection in Partial Eclipse 2024 Durations", x = "Longitude", y = "Latitude", color = "Anomaly Score") +
  coord_map(xlim = c(-176, -50), ylim = c(17.95, 71.25)) +
  theme_minimal()

ggsave(filename = file.path(ml_results_dir, "isolation_forest_partial_2024_map.png"), plot = isolation_forest_partial_24, width = 10, height = 7)

# Spatial Autocorrelation with Total 2024

library(spdep)

coords <- as.matrix(eclipse_total_2024[, c("lon", "lat")])
knn <- knearneigh(coords, k=4)
spatial_weights <- nb2listw(knn2nb(knn))
moran_test <- moran.test(eclipse_total_2024$duration, listw = spatial_weights)
print(moran_test)

moran_test_output <- capture.output(moran_test)
moran_filepath <- "machine_learning_results/moran_i_test_output.txt"
writeLines(moran_test_output, moran_filepath)
cat("Moran's I test output has been written to", moran_filepath, "\n")
