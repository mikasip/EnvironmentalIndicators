library(ECoST)
library(tensorflow)
library(NonlinearBSS)
library(gstat)
library(sp)
library(spacetime)
library(sf)
library(covatest)
library(dplyr)
library(rdist)
library(ggplot2)
library(kernelshap)
library(xtable)
library(viridis)
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
save_with_tf(ivae_radial3, "env_ind_ivae_radial3_v2", file = "env_ind_ivae_radial3_v2.RData")
ivae_radial3 <- load_with_tf("env_ind_ivae_radial3_v2.RData")
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
save_with_tf(ivae_radial4, "env_ind_ivae_radial4_v2", file = "env_ind_ivae_radial4_v2.RData")
ivae_radial4 <- load_with_tf("env_ind_ivae_radial4_v2.RData")
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
save_with_tf(ivae_radial5, "env_ind_ivae_radial5_v2", file = "env_ind_ivae_radial5_v2.RData")
ivae_radial5 <- load_with_tf("env_ind_ivae_radial5_v2.RData")
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
save_with_tf(ivae_radial6, "env_ind_ivae_radial6_v2", file = "env_ind_ivae_radial6_v2.RData")
ivae_radial6 <- load_with_tf("env_ind_ivae_radial6_v2.RData")
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
save_with_tf(ivae_radial7, "env_ind_ivae_radial7_v2", file = "env_ind_ivae_radial7_v2.RData")
ivae_radial7 <- load_with_tf("env_ind_ivae_radial7_v2.RData")
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
save_with_tf(ivae_radial8, "env_ind_ivae_radial8_v2", file = "env_ind_ivae_radial8_v2.RData")
ivae_radial8 <- load_with_tf("env_ind_ivae_radial8_v2.RData")
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
save_with_tf(ivae_radial9, "env_ind_ivae_radial9_v2", file = "env_ind_ivae_radial9_v2.RData")
ivae_radial9 <- load_with_tf("env_ind_ivae_radial9_v2.RData")
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
save_with_tf(ivae_radial10, "env_ind_ivae_radial10_v2", file = "env_ind_ivae_radial10_v2.RData")
ivae_radial10 <- load_with_tf("env_ind_ivae_radial10_v2.RData")
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
save_with_tf(ivae_radial11, "env_ind_ivae_radial11_v2", file = "env_ind_ivae_radial11_v2.RData")
ivae_radial11 <- load_with_tf("env_ind_ivae_radial11_v2.RData")
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
save_with_tf(ivae_radial12, "env_ind_ivae_radial12_v2", file = "env_ind_ivae_radial12_v2.RData")
ivae_radial12 <- load_with_tf("env_ind_ivae_radial12_v2.RData")
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
save_with_tf(ivae_radial13, "env_ind_ivae_radial13_v2", file = "env_ind_ivae_radial13_v2.RData")
ivae_radial13 <- load_with_tf("env_ind_ivae_radial13_v2.RData")
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
save_with_tf(ivae_radial14, "env_ind_ivae_radial14_v2", file = "env_ind_ivae_radial14_v2.RData")
ivae_radial14 <- load_with_tf("env_ind_ivae_radial14_v2.RData")
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
save_with_tf(ivae_radial10_final, "env_ind_ivae_radial10_final", file = "env_ind_ivae_radial10_final.RData")
ivae_radial10_final <- load_with_tf("env_ind_ivae_radial10_final.RData")
# Shapley values for ICs

X <- as.data.frame(ivae_radial10_final$IC)
n <- nrow(X)
set.seed(18082025)
n_sample <- 100000
sample_inds <- sample(1:n, n_sample)
X <- X[sample_inds, ]
bg_X <- X[1:500, ]
explainer2 <- kernelshap(ivae_radial10_final, X, bg_X = bg_X, pred_fun = function(object, X) {
  pred <- predict(object, newdata = as.matrix(X), IC_to_data = TRUE)
  return(pred)
})
save(explainer2, file = "decoder_shap_env_ind_v2.RData")
load("decoder_shap_env_ind_v2.RData")
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

# Plot the temporal behavior of ICs 6, 8, 9 and 10 at first station.

ic_data <- ivae_radial10_final$IC
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
t4 <- ggplot(ic_data_temp, aes(x = time, y = -IC4)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC4") +
    theme_minimal()


# Plot spatial behaviors of the ICs over summer 2023 and winter 2022-2023:

ic_data <- ic_data[, col_ord]
colnames(ic_data) <- paste0("IC", 1:10)
EEA_IC_df <- cbind(EEA_sub_aux, ic_data)
ICs_summer2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2023-06-01" & EEA_IC_df$time < "2023-09-01"), ]
ICs_winter2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2022-12-01" & EEA_IC_df$time < "2023-03-01"), ]

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

winter_means_df <- data.frame(spat_coords_df, means_winter2023_ic1, means_winter2023_ic2, means_winter2023_ic3)
summer_means_df <- data.frame(spat_coords_df, means_summer2023_ic1, means_summer2023_ic2, means_summer2023_ic3)
colnames(winter_means_df)[(ncol(winter_means_df) - 2):ncol(winter_means_df)] <- c("IC1", "IC2", "IC3")
colnames(summer_means_df)[(ncol(summer_means_df) - 2):ncol(summer_means_df)] <- c("IC1", "IC2", "IC3")

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

EEA_points_summer <- st_as_sf(summer_means_df, coords = c("Longitude", "Latitude"), crs = 4326)
EEA_points_winter <- st_as_sf(winter_means_df, coords = c("Longitude", "Latitude"), crs = 4326)

plot_north_map <- function(df, variable, limits = NULL, hide_legend = FALSE, change_sign = FALSE) {
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
    scale_color_viridis_c(option = "C", na.value = "grey50", limits = limits) +
    labs(color = variable) +
    coord_sf(expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold")
    )
  if (hide_legend) {
    plot <- plot + guides(color = "none")
  }
  return(plot)
}

s1_limits <- range(c(winter_means_df$IC1, summer_means_df$IC1))
s2_limits <- range(c(winter_means_df$IC2, summer_means_df$IC2))
s3_limits <- range(c(winter_means_df$IC3, summer_means_df$IC3))

s1_summer <- plot_north_map(EEA_points_summer, "IC1", s1_limits, change_sign = TRUE)
s2_summer <- plot_north_map(EEA_points_summer, "IC2", s2_limits)
s3_summer <- plot_north_map(EEA_points_summer, "IC3", s3_limits)

s1_winter <- plot_north_map(EEA_points_winter, "IC1", s1_limits, TRUE, change_sign = TRUE)
s2_winter <- plot_north_map(EEA_points_winter, "IC2", s2_limits, TRUE)
s3_winter <- plot_north_map(EEA_points_winter, "IC3", s3_limits, TRUE)

# Temporal and spatial side by side for each IC
grid.arrange(t1, s1_winter, s1_summer, t2, s2_winter, s2_summer, t3, s3_winter, s3_summer, ncol = 3, widths = c(0.9, 0.77, 1))


# Fit final iVAEar model for 60 epochs:
ivaear_radial10_final <- iVAEar_radial(
    as.matrix(data), 
    as.matrix(coords_time[, 1:2]), as.matrix(coords_time[, 3]),
    latent_dim = 10,
    spatial_basis = c(2, 9, 17, 37),
    temporal_basis = c(9, 17, 37),
    seasonal_period = 365,
    aux_hidden_units = c(128, 128, 128),
    ar_order = 2,
    n_s = n_s,
    epochs = 60,
    get_elbo = FALSE,
    batch_size = 64,
    seed = seed
)
save_with_tf(ivaear_radial10_final, "env_ind_ivaear1_radial10_final_v2", file = "env_ind_ivaear1_radial10_final_v2.RData")
ivaear1_radial10_final <- load_with_tf("env_ind_ivaear1_radial10_final_v2.RData")

library(kernelshap)
library(xtable)
X <- as.data.frame(ivaear1_radial10_final$IC)
n <- nrow(X)
set.seed(18082025)
n_sample <- 100000
sample_inds <- sample(1:n, n_sample)
X <- X[sample_inds, ]
bg_X <- X[1:500, ]
explainer2 <- kernelshap(ivaear1_radial10_final, X, bg_X = bg_X, pred_fun = function(object, X) {
  pred <- predict(object, newdata = as.matrix(X), IC_to_data = TRUE)
  return(pred)
})
save(explainer2, file = "decoder_shap_env_ind2_v2.RData")
load("decoder_shap_env_ind2_v2.RData")
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
rownames(df_fmt) <- rownames(shap_vars_scaled2)
tab <- xtable(df_fmt, digits = 3)
print(tab, sanitize.text.function = identity, include.rownames = TRUE)
xtable(data.frame(t(colMeans(shap_vars_scaled2))), digits = 3)

EEA_sub_aux_IC <- cbind(EEA_sub_aux, ivaear1_radial10_final$IC[, col_ord])
colnames(EEA_sub_aux_IC) <- c(colnames(EEA_sub_aux), paste0("IC", 1:10))
save(EEA_sub_aux_IC, file = "EEA_sub_aux_IC.RData")
str(EEA_sub_aux_IC)

ic_data <- ivaear1_radial10_final$IC
temp_inds <- which(EEA_sub_aux$AirQualityStation == unique(EEA_sub_aux$AirQualityStation)[8])
ic_data_temp <- as.data.frame(ic_data[temp_inds, ])
ic_data_temp <- ic_data_temp[, col_ord]
names(ic_data_temp) <- paste0("IC", 1:10)
ic_data_temp$time <- EEA_sub_aux$time[temp_inds]
# IC1
t1 <- ggplot(ic_data_temp, aes(x = time, y = IC1)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC1") +
    theme_minimal()

# IC2
t2 <- ggplot(ic_data_temp, aes(x = time, y = IC2)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC2") +
    theme_minimal()

# IC3
t3 <- ggplot(ic_data_temp, aes(x = time, y = -IC3)) +
    geom_line() +
    labs(title = "", x = "Time", y = "IC3") +
    theme_minimal()


# Plot spatial behaviors of the ICs over summer 2023 and winter 2022-2023:

ic_data <- ic_data[, col_ord]
colnames(ic_data) <- paste0("IC", 1:10)
EEA_IC_df <- cbind(EEA_sub_aux, ic_data)
ICs_summer2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2023-06-01" & EEA_IC_df$time < "2023-09-01"), ]
ICs_winter2023 <- EEA_IC_df[which(EEA_IC_df$time >= "2022-12-01" & EEA_IC_df$time < "2023-03-01"), ]

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

winter_means_df <- data.frame(spat_coords_df, means_winter2023_ic1, means_winter2023_ic2, means_winter2023_ic3)
summer_means_df <- data.frame(spat_coords_df, means_summer2023_ic1, means_summer2023_ic2, means_summer2023_ic3)
colnames(winter_means_df)[(ncol(winter_means_df) - 2):ncol(winter_means_df)] <- c("IC1", "IC2", "IC3")
colnames(summer_means_df)[(ncol(summer_means_df) - 2):ncol(summer_means_df)] <- c("IC1", "IC2", "IC3")

EEA_points_summer <- st_as_sf(summer_means_df, coords = c("Longitude", "Latitude"), crs = 4326)
EEA_points_winter <- st_as_sf(winter_means_df, coords = c("Longitude", "Latitude"), crs = 4326)

s1_limits <- range(c(winter_means_df$IC1, summer_means_df$IC1))
s2_limits <- range(c(winter_means_df$IC2, summer_means_df$IC2))
s3_limits <- range(c(winter_means_df$IC3, summer_means_df$IC3))

s1_summer <- plot_north_map(EEA_points_summer, "IC1", s1_limits)
s2_summer <- plot_north_map(EEA_points_summer, "IC2", s2_limits)
s3_summer <- plot_north_map(EEA_points_summer, "IC3", s3_limits, change_sign = TRUE)

s1_winter <- plot_north_map(EEA_points_winter, "IC1", s1_limits, TRUE)
s2_winter <- plot_north_map(EEA_points_winter, "IC2", s2_limits, TRUE)
s3_winter <- plot_north_map(EEA_points_winter, "IC3", s3_limits, TRUE, change_sign = TRUE)

grid.arrange(t1, s1_winter, s1_summer, t2, s2_winter, s2_summer, t3, s3_winter, s3_summer, ncol = 3, widths = c(0.9, 0.77, 1))

# Predictions of the latent components, iVAEar

max_time <- max(coords_time[, 3])
ar_order <- 2
future_coords_time <- coords_time[which(coords_time[, 3] > (max_time - 365)), ]
future_coords_time[, 3] <- future_coords_time[, 3] + 365
last_coords_time <- coords_time[which(coords_time[, 3] %in% ((max_time - (ar_order - 1)):max_time)), ]
pred_ICs_2024 <- predict_coords_to_IC_ar(ivaear1_radial10_final, last_coords_time[, 1:2], last_coords_time[, 3], NULL, future_coords_time[, 1:2], future_coords_time[, 3], NULL)
pred_ICs_2024 <- pred_ICs_2024$preds
#pred_obs_time_aux <- predict(ivaear1_radial10_final, st_ar_test_time, IC_to_data = TRUE)

# Visualize the predicted latent components
pred_ICs_2024_ord <- pred_ICs_2024[, col_ord]

pred_ICs_2024 <- cbind(future_coords_time, as.data.frame(pred_ICs_2024_ord))
colnames(pred_ICs_2024) <- c("X", "Y", "time_numeric", paste0("IC", 1:10))
head(pred_ICs_2024)

save(pred_ICs_2024, file = "pred_ICs_2024.RData")
load("pred_ICs_2024.RData")
str(pred_ICs_2024)
min(unique(pred_ICs_2024$time_numeric))
as.Date(max(unique(pred_ICs_2024$time_numeric)), origin = "1970-01-01")

station_coords <- EEA_sub_aux[EEA_sub_aux$AirQualityStation %in% unique(EEA_sub_aux$AirQualityStation)[8], c("X", "Y")][1,]
temp_inds <- which(future_coords_time[, 1] == as.numeric(station_coords[1]) & future_coords_time[, 2] == as.numeric(station_coords[2]))
ic_data_temp <- as.data.frame(pred_ICs_2024_ord[temp_inds, ])
names(ic_data_temp) <- paste0("IC", 1:10)
ic_data_temp$time <- as.Date(future_coords_time[temp_inds, 3])
snapshot_time <- as.Date("2024-07-01")
# IC1
t1 <- ggplot(ic_data_temp, aes(x = time, y = IC1)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC1") +
    theme_minimal()

# IC2
t2 <- ggplot(ic_data_temp, aes(x = time, y = IC2)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC2") +
    theme_minimal()

# IC3
t3 <- ggplot(ic_data_temp, aes(x = time, y = -IC3)) +
    geom_line() +
    geom_vline(xintercept = snapshot_time, linetype = "dashed", color = "red") +
    labs(title = "", x = "Time", y = "IC3") +
    theme_minimal()

spat_inds <- which(as.Date(future_coords_time[, 3]) == "2024-07-01")
ic_data_spat <- as.data.frame(pred_ICs_2024_ord[spat_inds, ])
names(ic_data_spat) <- paste0("IC", 1:10)
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
s1 <- plot_north_map(ic_data_spat, "IC1", NULL)

s2 <- plot_north_map(ic_data_spat, "IC2", NULL)

s3 <- plot_north_map(ic_data_spat, "IC3", NULL, change_sign = TRUE)

grid.arrange(t1, s1, t2, s2, t3, s3, ncol = 2)