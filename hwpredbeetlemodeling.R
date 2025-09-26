# HWA Predatory Beetle - Abiotic and Biotic Factors Affecting Species Presence/Survivorship

# load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(sp)
library(sf)
library(terra)

data_dir <- "/Volumes/cmjone25"

##survey sites 2022-24 data path
survey_path <- file.path(data_dir, "2022-24_StSurveyAllSites.csv")

surveys <-read_csv(survey_path)
##plotting into spatial points
survey_points <- vect(surveys, geom = c("Lon", "Lat"), crs("epsg:4326")) 
states <- vect(file.path(data_dir, "Data/Vector/USA/us_lower_48_states.gpkg"))
nc <- states[states$STATE_NAME == "North Carolina",]
plot(nc)
plot(survey_points, add = T)
sp <- crop(survey_points, nc)
glimpse(surveys)

##raster data - abiotic factors data path
tmax2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2022.nc"))
survey_points_p <- terra::project(survey_points, tmax2022)
buffers <- buffer(survey_points_p, 5000)
tmax2022c <- crop(tmax2022, buffers)
tmax_mean2022 <- mean(tmax2022c)
surveys$tmax2022 <- extract(tmax_mean2022, survey_points_p)
plot(tmax_mean2022)
plot(survey_points_p, add = T)
write.csv(surveys, "surveys_wtmax2022.csv")
surveys <- read.csv("surveys_wtmax2022.csv")

tmax2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2023.nc"))

tmin2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2022.nc"))

tmin2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2023.nc"))

precip2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/precip/daymet_v4_daily_na_prcp_2022.nc"))

precip2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/precip/daymet_v4_daily_na_prcp_2023.nc"))

###need tmax2024, tmin2024, and precip2024##

elevation <- rast(file.path("/Volumes/cmjone25/Data/Raster/USA/elevation", "dem_1s.tif"))
hemlockbiomass <- rast(file.path("/Volumes/cmjone25/Data/Original/bigmap fs tree species biomass/BIGMAP_AGB_2018_SPCD0261_EASTERN_HEMLOCK/Hosted_AGB_0261_2018_EASTERN_HEMLOCK_06062023072438.tif"))
survey_points_p <- terra::project(survey_points, hemlockbiomass)
buffers <- buffer(survey_points_p, 5000)

#reprojection of data
tmax2022 <- project(tmax2022, hemlockbiomass)

surveys$tmax2022 <- extract(tmax2022, survey_points)


