library(tensorflow)
library(NonlinearBSS)
library(ggplot2)
library(kernelshap)
library(dplyr)
library(xtable)
library(viridis)
library(sp)
library(sf)
library(gridExtra)

var_names <- c("mean_O3", "mean_NO2", "mean_PM10", "rh", "ssr", "t2m", "tp", "windspeed", "co", "nh3", "no", "pm25", "so2", "voc")

load("EEA_sub_aux.RData")

coordinates_val_latlon <- SpatialPoints(cbind(EEA_sub_aux$Longitude, EEA_sub_aux$Latitude),
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))

# Transform to UTM Zone 32N (best for Northern Italy)
coordinates_val_utm <- spTransform(coordinates_val_latlon, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m"))

# Add X and Y columns to your dataframe
EEA_sub_aux$X <- coordinates(coordinates_val_utm)[,1]
EEA_sub_aux$Y <- coordinates(coordinates_val_utm)[,2]

# Check which stations have all missing values in windspeed (and other meteorological variables)
missing_windspeed_stations <- EEA_sub_aux %>%
  group_by(AirQualityStation) %>%
  summarize(all_missing = all(is.na(windspeed))) %>%
  filter(all_missing) %>%
  pull(AirQualityStation)

# Drop those stations from the dataset
EEA_sub_aux <- EEA_sub_aux %>%
  filter(!AirQualityStation %in% missing_windspeed_stations)

# iVAE model
data <- EEA_sub_aux[, var_names]
coords_time <- as.matrix(EEA_sub_aux[, c("X", "Y", "time_numeric")])
n_s <- length(unique(EEA_sub_aux$AirQualityStation))
n_t <- length(unique(EEA_sub_aux$time_numeric))
head(EEA_sub_aux)
# Start by selecting the number of latent components


seed <- 18082025
ivae_radial3 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 3,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial3, "env_ind_ivae_radial3_v2", file = "models/env_ind_ivae_radial3_v2.RData")
ivae_radial3 <- load_with_tf("models/env_ind_ivae_radial3_v2.RData")
elbo3 <- ivae_radial3$elbo

ivae_radial4 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 4,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial4, "env_ind_ivae_radial4_v2", file = "models/env_ind_ivae_radial4_v2.RData")
ivae_radial4 <- load_with_tf("models/env_ind_ivae_radial4_v2.RData")
elbo4 <- ivae_radial4$elbo

ivae_radial5 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 5,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial5, "env_ind_ivae_radial5_v2", file = "models/env_ind_ivae_radial5_v2.RData")
ivae_radial5 <- load_with_tf("models/env_ind_ivae_radial5_v2.RData")
elbo5 <- ivae_radial5$elbo

ivae_radial6 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 6,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial6, "env_ind_ivae_radial6_v2", file = "models/env_ind_ivae_radial6_v2.RData")
ivae_radial6 <- load_with_tf("models/env_ind_ivae_radial6_v2.RData")
elbo6 <- ivae_radial6$elbo

ivae_radial7 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 7,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial7, "env_ind_ivae_radial7_v2", file = "models/env_ind_ivae_radial7_v2.RData")
ivae_radial7 <- load_with_tf("models/env_ind_ivae_radial7_v2.RData")
elbo7 <- ivae_radial7$elbo

ivae_radial8 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 8,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial8, "env_ind_ivae_radial8_v2", file = "models/env_ind_ivae_radial8_v2.RData")
ivae_radial8 <- load_with_tf("models/env_ind_ivae_radial8_v2.RData")
elbo8 <- ivae_radial8$elbo

ivae_radial9 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 9,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial9, "env_ind_ivae_radial9_v2", file = "models/env_ind_ivae_radial9_v2.RData")
ivae_radial9 <- load_with_tf("models/env_ind_ivae_radial9_v2.RData")
elbo9 <- ivae_radial9$elbo

ivae_radial10 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 10,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial10, "env_ind_ivae_radial10_v2", file = "models/env_ind_ivae_radial10_v2.RData")
ivae_radial10 <- load_with_tf("models/env_ind_ivae_radial10_v2.RData")
elbo10 <- ivae_radial10$elbo

ivae_radial11 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 11,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial11, "env_ind_ivae_radial11_v2", file = "models/env_ind_ivae_radial11_v2.RData")
ivae_radial11 <- load_with_tf("models/env_ind_ivae_radial11_v2.RData")
elbo11 <- ivae_radial11$elbo

ivae_radial12 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 12,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial12, "env_ind_ivae_radial12_v2", file = "models/env_ind_ivae_radial12_v2.RData")
ivae_radial12 <- load_with_tf("models/env_ind_ivae_radial12_v2.RData")
elbo12 <- ivae_radial12$elbo

ivae_radial13 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 13,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial13, "env_ind_ivae_radial13_v2", file = "models/env_ind_ivae_radial13_v2.RData")
ivae_radial13 <- load_with_tf("models/env_ind_ivae_radial13_v2.RData")
elbo13 <- ivae_radial13$elbo

ivae_radial14 <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 14,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 30,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial14, "env_ind_ivae_radial14_v2", file = "models/env_ind_ivae_radial14_v2.RData")
ivae_radial14 <- load_with_tf("models/env_ind_ivae_radial14_v2.RData")
elbo14 <- ivae_radial14$elbo

elbos <- c(
    elbo4 = elbo4,
    elbo5 = elbo5,
    elbo6 = elbo6,
    elbo7 = elbo7,
    elbo8 = elbo8,
    elbo9 = elbo9,
    elbo10 = elbo10,
    elbo11 = elbo11,
    elbo12 = elbo12,
    elbo13 = elbo13,
    elbo14 = elbo14
)

elbos_df <- data.frame(
    latent_dim = 4:14,
    elbo = unname(elbos)
)
paics <- elbos_df$elbo - elbos_df$latent_dim
elbos_df[which(max(paics) == paics), ]
# Plot the ELBO values
ggplot(elbos_df, aes(x = latent_dim, y = elbo)) +
    geom_line() +
    geom_point() +
    labs(title = "", x = "Latent Dimension", y = "ELBO") +
    # Make all latent dimension tick marks visible
    scale_x_continuous(breaks = 4:15) +
    # Make the dot at 10 larger and red
    geom_point(data = elbos_df[elbos_df$latent_dim == 10, ], aes(x = latent_dim, y = elbo), color = "red", size = 2) +
    theme_minimal()
# Based on analysis, P = 10.

# Fit the final iVAE model for 60 epochs.
ivae_radial10_final <- iVAE_radial_spatio_temporal(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 10,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    aux_hidden_units = c(128, 128, 128),
    epochs = 60,
    get_elbo = TRUE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivae_radial10_final, "env_ind_ivae_radial10_final_v2", file = "models/env_ind_ivae_radial10_final_v2.RData")
ivae_radial10_final <- load_with_tf("models/env_ind_ivae_radial10_final_v2.RData")
# Shapley values for ICs

X <- as.data.frame(ivae_radial10_final$IC)
n <- nrow(X)
set.seed(18082025)
n_sample <- 10000
sample_inds <- sample(1:n, n_sample)
X <- X[sample_inds, ]
bg_X <- X[1:500, ]
explainer2 <- kernelshap(ivae_radial10_final, X, bg_X = bg_X, pred_fun = function(object, X) {
  pred <- predict(object, newdata = as.matrix(X), IC_to_data = TRUE)
  return(pred)
})
save(explainer2, file = "models/decoder_shap_env_ind_v3.RData")
load("models/decoder_shap_env_ind_v3.RData")
explainer2$baseline
shap_vars2 <- data.frame(matrix(NA, ncol = 10, nrow = 14))
rownames(shap_vars2) <- colnames(data)
colnames(shap_vars2) <- sapply(1:10, function(i) paste0("IC", i))
i <- 1
for (l in explainer2$S) {
  shap_vars2[i, ] <- apply(l, 2, function(x) mean(abs(x)))
  i <- i + 1
}
shap_vars_scaled2 <- sweep(shap_vars2, 1, rowSums(shap_vars2), "/")
shap_vars_scaled2
avgs <- colMeans(shap_vars_scaled2)
col_ord <- order(-avgs)
shap_vars_scaled2 <- shap_vars_scaled2[, col_ord]
colnames(shap_vars_scaled2) <- paste0("IC", 1:10)
# Format values: bold if > 0.2
df_fmt <- as.data.frame(
  lapply(shap_vars_scaled2, function(col) {
    if (is.numeric(col)) {
      sapply(col, function(x) {
        if (x > 0.2) {
          sprintf("\\textbf{%.3f}", x)
        } else {
          sprintf("%.3f", x)
        }
      })
    } else {
      col
    }
  })
)

# Convert to xtable
rownames(df_fmt) <- rownames(shap_vars_scaled2)
tab <- xtable(df_fmt, digits = 3)
print(tab, sanitize.text.function = identity, include.rownames = TRUE)
xtable(data.frame(t(colMeans(shap_vars_scaled2))), digits = 3)

# Plot the temporal behavior of four most important ICs for a specific station (e.g., station 8):

ic_data <- ivae_radial10_final$IC
temp_inds <- which(EEA_sub_aux$AirQualityStation == unique(EEA_sub_aux$AirQualityStation)[8])
ring_coords <- EEA_sub_aux[temp_inds, c("Longitude", "Latitude")][1, ]
ic_data_temp <- as.data.frame(ic_data[temp_inds, ])
ic_data_temp <- ic_data_temp[, col_ord]
names(ic_data_temp) <- paste0("IC", 1:10)
ic_data_temp$time <- EEA_sub_aux$time[temp_inds]
# IC1
t1 <- ggplot(ic_data_temp, aes(x = time, y = -IC1)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC1") +
    theme_minimal()

# IC2
t2 <- ggplot(ic_data_temp, aes(x = time, y = -IC2)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC2") +
    theme_minimal()

# IC3
t3 <- ggplot(ic_data_temp, aes(x = time, y = -IC3)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC3") +
    theme_minimal()

# IC4
t4 <- ggplot(ic_data_temp, aes(x = time, y = IC4)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC4") +
    theme_minimal()


# Plot spatial behaviors of the ICs over summer 2023 and winter 2022-2023:

ic_data <- ic_data[, col_ord]
colnames(ic_data) <- paste0("IC", 1:10)
EEA_IC_df <- cbind(EEA_sub_aux, ic_data)
ICs_summer2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2023-06-01" & EEA_IC_df$time < "2023-09-01"), ]
ICs_winter2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2022-12-01" & EEA_IC_df$time < "2023-03-01"), ]

# Calculate the mean IC values for each station during summer and winter:
spat_coords_df <- unique(EEA_IC_df[, c("AirQualityStation", "Longitude", "Latitude")])
means_summer2023_ic1 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC1[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic1 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC1[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic2 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC2[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic2 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC2[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic3 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC3[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic3 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC3[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic4 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC4[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic4 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC4[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})

winter_means_df <- data.frame(spat_coords_df, means_winter2023_ic1, means_winter2023_ic2, means_winter2023_ic3, means_winter2023_ic4)
summer_means_df <- data.frame(spat_coords_df, means_summer2023_ic1, means_summer2023_ic2, means_summer2023_ic3, means_summer2023_ic4)
colnames(winter_means_df)[(ncol(winter_means_df) - 3):ncol(winter_means_df)] <- c("IC1", "IC2", "IC3", "IC4")
colnames(summer_means_df)[(ncol(summer_means_df) - 3):ncol(summer_means_df)] <- c("IC1", "IC2", "IC3", "IC4")


# Load Italian administrative boundaries to plot the spatial distribution of ICs in Northern Italy:
url <- "https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2023/Limiti01012023_g.zip"
tmp <- tempfile(fileext = ".zip")
download.file(url, tmp, mode = "wb")
unzip_dir <- tempfile()
unzip(tmp, exdir = unzip_dir)

shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
ripgeo_shp <- shp_files[grepl("RipGeo01012023_g", shp_files)][1]
ripgeo_sf <- st_read(ripgeo_shp, quiet = TRUE)
north_sf <- ripgeo_sf %>% filter(DEN_RIP %in% c("Nord-ovest", "Nord-est"))
north_dissolved <- north_sf %>% st_union() %>% st_as_sf()

# Create sf objects for the EEA stations with IC values for summer and winter:
EEA_points_summer <- st_as_sf(summer_means_df, coords = c("Longitude", "Latitude"), crs = 4326)
EEA_points_winter <- st_as_sf(winter_means_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Function to plot the spatial distribution of an IC over the map of Northern Italy, with options for color limits, legend, and ring around a specific station:
plot_north_map <- function(df, variable, limits = NULL, hide_legend = FALSE, 
                           change_sign = FALSE, legend_title = NULL,
                           color_scale = "C", ring_coords = NULL) {
  df_var <- df %>% filter(!is.na(.data[[variable]]))
  if (change_sign) {
    df_var[[variable]] <- -df_var[[variable]]
    if (!is.null(limits)) {
        limits <- rev(-limits)
    }
  }
  plot <- ggplot() +
    geom_sf(data = north_dissolved, fill = NA, color = "black", size = 0.3) +
    geom_sf(data = st_as_sf(df_var, coords = c("Longitude", "Latitude"), crs = 4326),
            aes(color = .data[[variable]]), size = 2) +
    scale_color_viridis_c(option = color_scale, na.value = "grey50", limits = limits) +
    labs(color = variable) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
  if (hide_legend) {
    plot <- plot + guides(color = "none")
  }
  if (!is.null(legend_title)) {
    plot <- plot + labs(color = legend_title)
  }
  if (!is.null(ring_coords)) {
    plot <- plot + geom_sf(data = st_as_sf(ring_coords, coords = c("Longitude", "Latitude"), crs = 4326), color = "darkgreen", size = 3, shape = 1, stroke = 2)
  }
  return(plot)
}

# Determine common color limits across summer and winter for each IC to ensure comparability:
s1_limits <- range(c(winter_means_df$IC1, summer_means_df$IC1))
s2_limits <- range(c(winter_means_df$IC2, summer_means_df$IC2))
s3_limits <- range(c(winter_means_df$IC3, summer_means_df$IC3))
s4_limits <- range(c(winter_means_df$IC4, summer_means_df$IC4))

# Plot the spatial distribution of the ICs for summer and winter side by side, with the same color limits for comparability:
s1_summer <- plot_north_map(EEA_points_summer, "IC1", s1_limits, change_sign = TRUE, ring_coords = ring_coords)
s2_summer <- plot_north_map(EEA_points_summer, "IC2", s2_limits, change_sign = TRUE, ring_coords = ring_coords)
s3_summer <- plot_north_map(EEA_points_summer, "IC3", s3_limits, change_sign = TRUE, ring_coords = ring_coords)
s4_summer <- plot_north_map(EEA_points_summer, "IC4", s4_limits, ring_coords = ring_coords)

s1_winter <- plot_north_map(EEA_points_winter, "IC1", s1_limits, TRUE, change_sign = TRUE, ring_coords = ring_coords)
s2_winter <- plot_north_map(EEA_points_winter, "IC2", s2_limits, TRUE, change_sign = TRUE, ring_coords = ring_coords)
s3_winter <- plot_north_map(EEA_points_winter, "IC3", s3_limits, TRUE, change_sign = TRUE, ring_coords = ring_coords)
s4_winter <- plot_north_map(EEA_points_winter, "IC4", s4_limits, TRUE, ring_coords = ring_coords)

# Temporal and spatial side by side for each IC
grid.arrange(t1, s1_winter, s1_summer, t2, s2_winter, s2_summer, t3, s3_winter, s3_summer, t4, s4_winter, s4_summer,ncol = 3, widths = c(0.9, 0.77, 1))


# Fit final iVAEar model for 60 epochs:
seed <- 22042026
ivaear_radial10_final <- iVAEar_radial(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 10,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365.25,
    error_dist_sigma = 0.01,
    aux_hidden_units = c(128, 128, 128),
    ar_order = 2,
    n_s = n_s,
    epochs = 60,
    get_elbo = FALSE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivaear_radial10_final, "env_ind_ivaear1_radial10_final_v2", file = "models/env_ind_ivaear1_radial10_final_v2.RData")
ivaear1_radial10_final <- load_with_tf("models/env_ind_ivaear1_radial10_final_v2.RData")

X <- as.data.frame(ivaear1_radial10_final$IC)
n <- nrow(X)
set.seed(18082025)
n_sample <- 10000
sample_inds <- sample(1:n, n_sample)
X <- X[sample_inds, ]
bg_X <- X[1:500, ]
explainer2 <- kernelshap(ivaear1_radial10_final, X, bg_X = bg_X, pred_fun = function(object, X) {
  pred <- predict(object, newdata = as.matrix(X), IC_to_data = TRUE)
  return(pred)
})
save(explainer2, file = "models/decoder_shap_env_ind2_v4.RData")
load("models/decoder_shap_env_ind2_v4.RData")
explainer2$baseline
shap_vars2 <- data.frame(matrix(NA, ncol = 10, nrow = 14))
rownames(shap_vars2) <- colnames(data)
colnames(shap_vars2) <- sapply(1:10, function(i) paste0("IC", i))
i <- 1
for (l in explainer2$S) {
  shap_vars2[i, ] <- apply(l, 2, function(x) mean(abs(x)))
  i <- i + 1
}
shap_vars_scaled2 <- sweep(shap_vars2, 1, rowSums(shap_vars2), "/")
shap_vars_scaled2
colMeans(shap_vars_scaled2)
col_ord <- order(-colMeans(shap_vars_scaled2))
shap_vars_scaled2 <- shap_vars_scaled2[, col_ord]

# Convert to xtable
df_fmt <- as.data.frame(
  lapply(shap_vars_scaled2, function(col) { 
    if (is.numeric(col)) {
      sapply(col, function(x) {
        if (x > 0.1) {
          sprintf("\\textbf{%.3f}", x)
        } else {
          sprintf("%.3f", x)
        }
      })
    } else {
      col
    }
  })
)

rownames(df_fmt) <- rownames(shap_vars_scaled2)
tab <- xtable(df_fmt, digits = 3)
print(tab, sanitize.text.function = identity, include.rownames = TRUE)
xtable(data.frame(t(colMeans(shap_vars_scaled2))), digits = 3)

EEA_sub_aux_IC <- cbind(EEA_sub_aux, ivaear1_radial10_final$IC[, col_ord])
colnames(EEA_sub_aux_IC) <- c(colnames(EEA_sub_aux), paste0("IC", 1:10))

ic_data <- ivaear1_radial10_final$IC
temp_inds <- which(EEA_sub_aux$AirQualityStation == unique(EEA_sub_aux$AirQualityStation)[8])
ic_data_temp <- as.data.frame(ic_data[temp_inds, ])
ic_data_temp <- ic_data_temp[, col_ord]
names(ic_data_temp) <- paste0("IC", 1:10)
ic_data_temp$time <- EEA_sub_aux$time[temp_inds]
# IC1
t1 <- ggplot(ic_data_temp, aes(x = time, y = -IC1)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC1") +
    theme_minimal()

# IC2
t2 <- ggplot(ic_data_temp, aes(x = time, y = IC2)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC2") +
    theme_minimal()

# IC3
t3 <- ggplot(ic_data_temp, aes(x = time, y = IC3)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC3") +
    theme_minimal()

# IC4
t4 <- ggplot(ic_data_temp, aes(x = time, y = IC4)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC4") +
    theme_minimal()


# Plot spatial behaviors of the ICs over summer 2023 and winter 2022-2023:

ic_data <- ic_data[, col_ord]
colnames(ic_data) <- paste0("IC", 1:10)
EEA_IC_df <- cbind(EEA_sub_aux, ic_data)
ICs_summer2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2023-06-01" & EEA_IC_df$time < "2023-09-01"), ]
ICs_winter2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2022-12-01" & EEA_IC_df$time < "2023-03-01"), ]

# Calculate the mean IC values for each station during summer and winter:
spat_coords_df <- unique(EEA_IC_df[, c("AirQualityStation", "Longitude", "Latitude")])
means_summer2023_ic1 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC1[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic1 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC1[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic2 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC2[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic2 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC2[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic3 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC3[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic3 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC3[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_winter2023_ic4 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_winter2023$IC4[ICs_winter2023$AirQualityStation == station], na.rm = TRUE)
})
means_summer2023_ic4 <- sapply(spat_coords_df$AirQualityStation, function(station) {
    mean(ICs_summer2023$IC4[ICs_summer2023$AirQualityStation == station], na.rm = TRUE)
})

winter_means_df <- data.frame(spat_coords_df, means_winter2023_ic1, means_winter2023_ic2, means_winter2023_ic3, means_winter2023_ic4)
summer_means_df <- data.frame(spat_coords_df, means_summer2023_ic1, means_summer2023_ic2, means_summer2023_ic3, means_summer2023_ic4)
colnames(winter_means_df)[(ncol(winter_means_df) - 3):ncol(winter_means_df)] <- c("IC1", "IC2", "IC3", "IC4")
colnames(summer_means_df)[(ncol(summer_means_df) - 3):ncol(summer_means_df)] <- c("IC1", "IC2", "IC3", "IC4")

EEA_points_summer <- st_as_sf(summer_means_df, coords = c("Longitude", "Latitude"), crs = 4326)
EEA_points_winter <- st_as_sf(winter_means_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Determine common color limits across summer and winter for each IC to ensure comparability:
s1_limits <- range(c(winter_means_df$IC1, summer_means_df$IC1))
s2_limits <- range(c(winter_means_df$IC2, summer_means_df$IC2))
s3_limits <- range(c(winter_means_df$IC3, summer_means_df$IC3))
s4_limits <- range(c(winter_means_df$IC4, summer_means_df$IC4))

# Plot the spatial distribution of the ICs for summer and winter side by side, with the same color limits for comparability:
s1_summer <- plot_north_map(EEA_points_summer, "IC1", s1_limits, change_sign = TRUE, ring_coords = ring_coords)
s2_summer <- plot_north_map(EEA_points_summer, "IC2", s2_limits, ring_coords = ring_coords)
s3_summer <- plot_north_map(EEA_points_summer, "IC3", s3_limits, ring_coords = ring_coords)
s4_summer <- plot_north_map(EEA_points_summer, "IC4", s4_limits, ring_coords = ring_coords)

s1_winter <- plot_north_map(EEA_points_winter, "IC1", s1_limits, TRUE, change_sign = TRUE, ring_coords = ring_coords)
s2_winter <- plot_north_map(EEA_points_winter, "IC2", s2_limits, TRUE, ring_coords = ring_coords)
s3_winter <- plot_north_map(EEA_points_winter, "IC3", s3_limits, TRUE, ring_coords = ring_coords)
s4_winter <- plot_north_map(EEA_points_winter, "IC4", s4_limits, TRUE, ring_coords = ring_coords)

grid.arrange(t1, s1_winter, s1_summer, t2, s2_winter, s2_summer, t3, s3_winter, s3_summer, t4, s4_winter, s4_summer, ncol = 3, widths = c(0.9, 0.77, 1))

# Predictions of the latent components, iVAEar

max_time <- max(coords_time[, 3])
ar_order <- 2
time_interval <- 365
future_coords_time <- coords_time[which(coords_time[, 3] > (max_time - time_interval)), ]
future_coords_time[, 3] <- future_coords_time[, 3] + time_interval
last_coords_time <- coords_time[which(coords_time[, 3] %in% ((max_time - (ar_order - 1)):max_time)), ]

# Note that model has to be trained in the same session for the predict function to work.
pred_ICs_2024_obj <- predict_coords_to_IC_ar(ivaear1_radial10_final, last_coords_time[, 1:2], last_coords_time[, 3], NULL, future_coords_time[, 1:2], future_coords_time[, 3], NULL, get_var = TRUE)
pred_ICs_2024 <- pred_ICs_2024_obj$preds
var_ICs_2024 <- pred_ICs_2024_obj$vars

# Visualize the predicted latent components
pred_ICs_2024_ord <- pred_ICs_2024[, col_ord]
var_ICs_2024_ord <- var_ICs_2024[, col_ord]

pred_ICs_2024 <- cbind(future_coords_time, as.data.frame(pred_ICs_2024_ord), as.data.frame(var_ICs_2024_ord))
colnames(pred_ICs_2024) <- c("X", "Y", "time_numeric", paste0("IC", 1:10), paste0("var_IC", 1:10))
head(pred_ICs_2024)

save(pred_ICs_2024, file = "pred_ICs_2024_v2.RData")
load("pred_ICs_2024_v2.RData")
str(pred_ICs_2024)
min(unique(pred_ICs_2024$time_numeric))
as.Date(max(unique(pred_ICs_2024$time_numeric)), origin = "1970-01-01")

# Visualize the predicted temporal behavior of the ICs for station 8, and the spatial distribution of the ICs on 2024-07-01, with uncertainty intervals.
station_coords <- EEA_sub_aux[EEA_sub_aux$AirQualityStation %in% unique(EEA_sub_aux$AirQualityStation)[8], c("X", "Y")][1,]
temp_inds <- which(future_coords_time[, 1] == as.numeric(station_coords[1]) & future_coords_time[, 2] == as.numeric(station_coords[2]))
ic_data_temp <- as.data.frame(pred_ICs_2024[temp_inds, ])
ic_data_temp$time <- as.Date(ic_data_temp$time_numeric)
snapshot_time <- as.Date("2024-07-01")
# IC1
t1 <- ggplot(ic_data_temp, aes(x = time, y = -IC1)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC1") +
    # display variance as shaded area
    geom_ribbon(aes(ymin = -IC1 - 1.645 * sqrt(var_IC1), ymax = -IC1 + 1.645 * sqrt(var_IC1)), alpha = 0.2) +
    theme_minimal()

# IC2
t2 <- ggplot(ic_data_temp, aes(x = time, y = IC2)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC2") +
    geom_ribbon(aes(ymin = IC2 - 1.96 * sqrt(var_IC2), ymax = IC2 + 1.96 * sqrt(var_IC2)), alpha = 0.2) +
    theme_minimal()

# IC3
t3 <- ggplot(ic_data_temp, aes(x = time, y = IC3)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC3") +
    geom_ribbon(aes(ymin = IC3 - 1.96 * sqrt(var_IC3), ymax = IC3 + 1.96 * sqrt(var_IC3)), alpha = 0.2) +
    theme_minimal()

# IC4
t4 <- ggplot(ic_data_temp, aes(x = time, y = IC4)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC4") +
    geom_ribbon(aes(ymin = IC4 - 1.96 * sqrt(var_IC4), ymax = IC4 + 1.96 * sqrt(var_IC4)), alpha = 0.2) +
    theme_minimal()

spat_inds <- which(as.Date(future_coords_time[, 3]) == "2024-07-01")
ic_data_spat <- as.data.frame(pred_ICs_2024[spat_inds, ])
#names(ic_data_spat) <- paste0("IC", 1:10)
sp_coords <- coordinates_val_utm_back <- SpatialPoints(
  coords = future_coords_time[spat_inds, 1:2],
  proj4string = CRS("+proj=utm +zone=32 +datum=WGS84 +units=m")
)
coordinates_val_latlon_back <- spTransform(
  sp_coords,
  CRS("+proj=longlat +datum=WGS84")
)

ic_data_spat$Longitude <- coordinates(coordinates_val_latlon_back)[, 1]
ic_data_spat$Latitude <- coordinates(coordinates_val_latlon_back)[, 2]

# Calculate confidence intervals for the predicted IC values
ic_data_spat$IC1_upper <- ic_data_spat$IC1 + 1.96 * sqrt(ic_data_spat$var_IC1)
ic_data_spat$IC1_lower <- ic_data_spat$IC1 - 1.96 * sqrt(ic_data_spat$var_IC1)
ic_data_spat$IC2_upper <- ic_data_spat$IC2 + 1.96 * sqrt(ic_data_spat$var_IC2)
ic_data_spat$IC2_lower <- ic_data_spat$IC2 - 1.96 * sqrt(ic_data_spat$var_IC2)
ic_data_spat$IC3_upper <- ic_data_spat$IC3 + 1.96 * sqrt(ic_data_spat$var_IC3)
ic_data_spat$IC3_lower <- ic_data_spat$IC3 - 1.96 * sqrt(ic_data_spat$var_IC3)
ic_data_spat$IC4_upper <- ic_data_spat$IC4 + 1.96 * sqrt(ic_data_spat$var_IC4)
ic_data_spat$IC4_lower <- ic_data_spat$IC4 - 1.96 * sqrt(ic_data_spat$var_IC4)

s1 <- plot_north_map(ic_data_spat, "IC1", change_sign = TRUE, ring_coords = ring_coords)

s2 <- plot_north_map(ic_data_spat, "IC2", ring_coords = ring_coords)

s3 <- plot_north_map(ic_data_spat, "IC3", ring_coords = ring_coords)

s4 <- plot_north_map(ic_data_spat, "IC4", ring_coords = ring_coords)


ic_data_spat$IC1_width <- ic_data_spat$IC1_upper - ic_data_spat$IC1_lower
ic_data_spat$IC2_width <- ic_data_spat$IC2_upper - ic_data_spat$IC2_lower
ic_data_spat$IC3_width <- ic_data_spat$IC3_upper - ic_data_spat$IC3_lower
ic_data_spat$IC4_width <- ic_data_spat$IC4_upper - ic_data_spat$IC4_lower

s1_width <- plot_north_map(ic_data_spat, "IC1_width", legend_title = "CI width",
                            color_scale = "D",
                           limits = c(0, max(ic_data_spat$IC1_width)))
s2_width <- plot_north_map(ic_data_spat, "IC2_width", legend_title = "CI width",
                           color_scale = "D",
                           limits = c(0, max(ic_data_spat$IC2_width))
                           )
s3_width <- plot_north_map(ic_data_spat, "IC3_width", legend_title = "CI width",
                            color_scale = "D",
                           limits = c(0, max(ic_data_spat$IC3_width))
                           )
s4_width <- plot_north_map(ic_data_spat, "IC4_width", legend_title = "CI width",
                            color_scale = "D",
                           limits = c(0, max(ic_data_spat$IC4_width))
                           )

#grid.arrange(t1, s1, s1_upper, s1_lower, t2, s2, s2_upper, s2_lower, t3, s3, s3_upper, s3_lower, ncol = 4, widths = c(0.5, 0.7, 0.7, 0.91))
grid.arrange(t1, s1, s1_width, t2, s2, s2_width, t3, s3, s3_width, t4, s4, s4_width, ncol = 3, widths = c(0.5, 0.7, 0.7))
