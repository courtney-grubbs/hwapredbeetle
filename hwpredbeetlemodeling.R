# HWA Predatory Beetle - Abiotic and Biotic Factors Affecting Species Presence/Survivorship

#data setup -------

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

##maxmium temperature##
tmax2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2022.nc"))
survey_points_p <- terra::project(survey_points, tmax2022)
buffers <- buffer(survey_points_p, 5000)
tmax2022c <- crop(tmax2022, buffers)
tmax_mean2022 <- mean(tmax2022c)
surveys$tmax2022 <- extract(tmax_mean2022, survey_points_p)
plot(tmax_mean2022)
plot(survey_points_p, add = T)

####saving work as i go to build columns in surveys (dataframe)
write.csv(surveys, "surveys_wtmax2022.csv")
surveys <- read.csv("surveys_wtmax2022.csv")

tmax2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2023.nc"))
survey_points_p <- terra::project(survey_points, tmax2023)
buffers <- buffer(survey_points_p, 5000)
tmax2023c <- crop(tmax2023, buffers)
tmax_mean2023 <- mean(tmax2023c)
surveys$tmax2023 <- extract(tmax_mean2023, survey_points_p)
plot(tmax_mean2023)
plot(survey_points_p, add = T)

##minimum temperature##
tmin2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2022.nc"))
survey_points_p <- terra::project(survey_points, tmin2022)
buffers <- buffer(survey_points_p, 5000)
tmin2022c <- crop(tmin2022, buffers)
tmin_mean2022 <- mean(tmin2022c)
surveys$tmin2022 <- extract(tmin_mean2022, survey_points_p)
plot(tmin_mean2022)
plot(survey_points_p, add = T)

tmin2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2023.nc"))
survey_points_p <- terra::project(survey_points, tmin2023)
buffers <- buffer(survey_points_p, 5000)
tmin2023c <- crop(tmin2023, buffers)
tmin_mean2023 <- mean(tmin2023c)
surveys$tmin2023 <- extract(tmin_mean2023, survey_points_p)
plot(tmin_mean2023)
plot(survey_points_p, add = T)

##preciptation##
precip2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/precip/daymet_v4_daily_na_prcp_2022.nc"))
survey_points_p <- terra::project(survey_points, precip2022)
buffers <- buffer(survey_points_p, 5000)
precip2022c <- crop(precip2022, buffers)
precip_mean2022 <- mean(precip2022c)
surveys$precip2022 <- extract(precip_mean2022, survey_points_p)
plot(precip_mean2022)
plot(survey_points_p, add = T)

precip2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/precip/daymet_v4_daily_na_prcp_2023.nc"))
survey_points_p <- terra::project(survey_points, precip2023)
buffers <- buffer(survey_points_p, 5000)
precip2023c <- crop(precip2023, buffers)
precip_mean2023 <- mean(precip2023c)
surveys$precip2023 <- extract(precip_mean2023, survey_points_p)
plot(precip_mean2023)
plot(survey_points_p, add = T)

####saving work as i go to build columns in surveys (dataframe)
write.csv(surveys, "surveys_wtempandprecip22_23.csv")
surveys <- read.csv("surveys_wtempandprecip22_23.csv")

##NEED tmax2024, tmin2024, and precip2024

elevation <- rast(file.path("/Volumes/cmjone25/Data/Raster/USA/elevation", "dem_1s.tif"))
survey_points_p <- terra::project(survey_points, elevation)
buffers <- buffer(survey_points_p, 5000)
elevationc <- crop(elevation, buffers)
surveys$elevation <- extract(elevation, survey_points_p)
plot(elevation)
plot(survey_points_p, add = T)

elevationc <- mask(elevationc, buffers) ## original crop showed the whole US
plot(elevationc)
plot(survey_points_p, add = T)

####saving work as i go to build columns in surveys (dataframe)
write.csv(surveys, "surveys_wtp22_23_e.csv")
surveys <- read.csv("surveys_wtp22_23_e.csv") #with temp22-23 + elevation

#hemlockbiomass
hemlockbiomass <- rast(file.path("/Volumes/cmjone25/Data/Original/bigmap fs tree species biomass/BIGMAP_AGB_2018_SPCD0261_EASTERN_HEMLOCK/Hosted_AGB_0261_2018_EASTERN_HEMLOCK_06062023072438.tif"))
survey_points_p <- terra::project(survey_points, hemlockbiomass)
buffers <- buffer(survey_points_p, 5000)
hemlockbiomassc <- crop(hemlockbiomass, buffers)
surveys$hemlockbiomass <- extract(hemlockbiomassc, survey_points_p)
plot(hemlockbiomassc)
plot(survey_points_p, add = T)

####saving work as i go to build columns in surveys (dataframe)
write.csv(surveys, "surveys_wtp22_23_ehb.csv")
surveys <- read.csv("surveys_wtp22_23_ehb.csv") #with temp22-23 + elevation + hemlockbiomass

##data analysis ####

#updating columns
surveys$X2022.Survey[surveys$X2022.Survey == "."] <- NA
surveys$X2022.Survey <- as.numeric(surveys$X2022.Survey)

surveys$X2023.Survey[surveys$X2023.Survey == "."] <- NA
surveys$X2023.Survey <- as.numeric(surveys$X2023.Survey)

surveys$X2024.Survey[surveys$X2024.Survey == "."] <- NA
surveys$X2024.Survey <- as.numeric(surveys$X2024.Survey)

#generalized linear modeling for presence vs variable
tmax22model <- glm(X2022.Survey ~ tmax2022.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmax22model) #negative effect, p value = 0.216

tmin22model <- glm(X2022.Survey ~ tmin2022.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmin22model) #negative effect, p value = 0.123

precip22model <- glm(X2022.Survey ~ precip2022.mean,
                     data = surveys,
                     family = binomial,
                     na.action = na.omit)
summary(precip22model) #positive effect, p value = 0.290

elevation22model <- glm(X2022.Survey ~ elevation.elevation,
                        data = surveys,
                        family = binomial,
                        na.action = na.omit)
summary(elevation22model) #p value = 0.654

#renaming column bc that one was way too long
names(surveys)[names(surveys) == "hemlockbiomass.Hosted_AGB_0261_2018_EASTERN_HEMLOCK_06062023072438"] <- "hemlock.biomass"

hbiomass22model <- glm(X2022.Survey ~ hemlock.biomass,
                       data = surveys,
                       family = binomial,
                       na.action = na.omit)
summary(hbiomass22model) #p = 0.2667

write.csv(surveys, "surveys_updated.csv")
surveys <- read.csv("surveys_updated.csv")

tmax23model <- glm(X2023.Survey ~ tmax2023.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmax23model) #negative effect, p value = 0.191

tmin23model <- glm(X2023.Survey ~ tmin2023.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmin23model) #negative effect, p value = 0.663

precip23model <- glm(X2023.Survey ~ precip2023.mean,
                     data = surveys,
                     family = binomial,
                     na.action = na.omit)
summary(precip23model) #positive effect, p value = 0.552

tmax22model <- glm(X2022.Survey ~ tmax2022.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmax22model) #negative effect, p value = 0.216

tmin22model <- glm(X2022.Survey ~ tmin2022.mean,
                   data = surveys,
                   family = binomial,
                   na.action = na.omit)
summary(tmin22model) #negative effect, p value = 0.123

#count released effect?
surveys$Count[surveys$Count == "."] <- NA
surveys$Count <- as.numeric(surveys$Count)

count22model <- glm(X2022.Survey ~ Count,
                    data = surveys,
                    family = binomial,
                    na.action = na.omit)
summary(count22model) #p value = 0.949 #may not be used correctly

write.csv(surveys, "surveys_updated.csv")
surveys <- read.csv("surveys_updated.csv")
