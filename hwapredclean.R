# Courtney Grubbs - Fall 2025 - I-GROUP 
# Identifying abiotic and biotic variables affecting the presence/survivorship 
##of the biological control agent Sasajiscymnus tsugae

# load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(sp)
library(sf)
library(terra)
library(lubridate)
library(factoextra)
library(geosphere)
library(tidyverse)

data_dir <- "/Volumes/cmjone25"

##survey sites 2022-24 data path ####
survey_path <- file.path(data_dir, "2022-24_StSurveyAllSites.csv")
surveys <-read_csv(survey_path)

##plotting into spatial points ####
survey_points <- vect(surveys, geom = c("Lon", "Lat"), crs("epsg:4326")) 
states <- vect(file.path(data_dir, "Data/Vector/USA/us_lower_48_states.gpkg"))
nc <- states[states$STATE_NAME == "North Carolina",]
plot(nc)
plot(survey_points, add = T)
sp <- crop(survey_points, nc)

#precipitation 2022-2023 ####
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

##elevation ####
elevation <- rast(file.path("/Volumes/cmjone25/Data/Raster/USA/elevation", "dem_1s.tif"))
survey_points_p <- terra::project(survey_points, elevation)
buffers <- buffer(survey_points_p, 5000)
elevationc <- crop(elevation, buffers)
surveys$elevation <- extract(elevationc, survey_points_p)
plot(elevationc)
plot(survey_points_p, add = T)

##hemlockbiomass ####
hemlockbiomass <- rast(file.path("/Volumes/cmjone25/Data/Original/bigmap fs tree species biomass/BIGMAP_AGB_2018_SPCD0261_EASTERN_HEMLOCK/Hosted_AGB_0261_2018_EASTERN_HEMLOCK_06062023072438.tif"))
survey_points_p <- terra::project(survey_points, hemlockbiomass)
buffers <- buffer(survey_points_p, 5000)
hemlockbiomassc <- crop(hemlockbiomass, buffers)
surveys$hemlockbiomass <- extract(hemlockbiomassc, survey_points_p)
plot(hemlockbiomassc)
plot(survey_points_p, add = T)

#renaming column bc that one was way too long
colnames(surveys)[28] <- "hemlock.biomass"

#save checkpoint####
write.csv(surveys, "surveys_clean.csv")
surveys <- read.csv("surveys_clean.csv")

#load tmin from 2005-2024 (take mean number of days) #####
tmin2005 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2005.nc"))
survey_points_p <- terra::project(survey_points, tmin2005)
buffers <- buffer(survey_points_p, 5000)
tmin2005c <- crop(tmin2005, buffers)
tmin_mean2005 <- mean(tmin2005c)
surveys$tmin2005 <- extract(tmin_mean2005, survey_points_p)

tmin2006 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2006.nc"))
survey_points_p <- terra::project(survey_points, tmin2006)
buffers <- buffer(survey_points_p, 5000)
tmin2006c <- crop(tmin2006, buffers)
tmin_mean2006 <- mean(tmin2006c)
surveys$tmin2006 <- extract(tmin_mean2006, survey_points_p)

tmin2007 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2007.nc"))
survey_points_p <- terra::project(survey_points, tmin2007)
buffers <- buffer(survey_points_p, 5000)
tmin2007c <- crop(tmin2007, buffers)
tmin_mean2007 <- mean(tmin2007c)
surveys$tmin2007 <- extract(tmin_mean2007, survey_points_p)

tmin2008 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2008.nc"))
survey_points_p <- terra::project(survey_points, tmin2008)
buffers <- buffer(survey_points_p, 5000)
tmin2008c <- crop(tmin2008, buffers)
tmin_mean2008 <- mean(tmin2008c)
surveys$tmin2008 <- extract(tmin_mean2008, survey_points_p)

tmin2009 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2009.nc"))
survey_points_p <- terra::project(survey_points, tmin2009)
buffers <- buffer(survey_points_p, 5000)
tmin2009c <- crop(tmin2009, buffers)
tmin_mean2009 <- mean(tmin2009c)
surveys$tmin2009 <- extract(tmin_mean2009, survey_points_p)

tmin2010 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2010.nc"))
survey_points_p <- terra::project(survey_points, tmin2010)
buffers <- buffer(survey_points_p, 5000)
tmin2010c <- crop(tmin2010, buffers)
tmin_mean2010 <- mean(tmin2010c)
surveys$tmin2010 <- extract(tmin_mean2010, survey_points_p)

tmin2011 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2011.nc"))
survey_points_p <- terra::project(survey_points, tmin2011)
buffers <- buffer(survey_points_p, 5000)
tmin2011c <- crop(tmin2011, buffers)
tmin_mean2011 <- mean(tmin2011c)
surveys$tmin2011 <- extract(tmin_mean2011, survey_points_p)

tmin2012 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2012.nc"))
survey_points_p <- terra::project(survey_points, tmin2012)
buffers <- buffer(survey_points_p, 5000)
tmin2012c <- crop(tmin2012, buffers)
tmin_mean2012 <- mean(tmin2012c)
surveys$tmin2012 <- extract(tmin_mean2012, survey_points_p)

tmin2013 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2013.nc"))
survey_points_p <- terra::project(survey_points, tmin2013)
buffers <- buffer(survey_points_p, 5000)
tmin2013c <- crop(tmin2013, buffers)
tmin_mean2013 <- mean(tmin2013c)
surveys$tmin2013 <- extract(tmin_mean2013, survey_points_p)

tmin2014 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2014.nc"))
survey_points_p <- terra::project(survey_points, tmin2014)
buffers <- buffer(survey_points_p, 5000)
tmin2014c <- crop(tmin2014, buffers)
tmin_mean2014 <- mean(tmin2014c)
surveys$tmin2014 <- extract(tmin_mean2014, survey_points_p)

tmin2015 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2015.nc"))
survey_points_p <- terra::project(survey_points, tmin2015)
buffers <- buffer(survey_points_p, 5000)
tmin2015c <- crop(tmin2015, buffers)
tmin_mean2015 <- mean(tmin2015c)
surveys$tmin2015 <- extract(tmin_mean2015, survey_points_p)

tmin2016 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2016.nc"))
survey_points_p <- terra::project(survey_points, tmin2016)
buffers <- buffer(survey_points_p, 5000)
tmin2016c <- crop(tmin2016, buffers)
tmin_mean2016 <- mean(tmin2016c)
surveys$tmin2016 <- extract(tmin_mean2016, survey_points_p)

tmin2017 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2017.nc"))
survey_points_p <- terra::project(survey_points, tmin2017)
buffers <- buffer(survey_points_p, 5000)
tmin2017c <- crop(tmin2017, buffers)
tmin_mean2017 <- mean(tmin2017c)
surveys$tmin2017 <- extract(tmin_mean2017, survey_points_p)

tmin2018 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2018.nc"))
survey_points_p <- terra::project(survey_points, tmin2018)
buffers <- buffer(survey_points_p, 5000)
tmin2018c <- crop(tmin2018, buffers)
tmin_mean2018 <- mean(tmin2018c)
surveys$tmin2018 <- extract(tmin_mean2018, survey_points_p)

tmin2019 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2019.nc"))
survey_points_p <- terra::project(survey_points, tmin2019)
buffers <- buffer(survey_points_p, 5000)
tmin2019c <- crop(tmin2019, buffers)
tmin_mean2019 <- mean(tmin2019c)
surveys$tmin2019 <- extract(tmin_mean2019, survey_points_p)

tmin2020 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2020.nc"))
survey_points_p <- terra::project(survey_points, tmin2020)
buffers <- buffer(survey_points_p, 5000)
tmin2020c <- crop(tmin2020, buffers)
tmin_mean2020 <- mean(tmin2020c)
surveys$tmin2020 <- extract(tmin_mean2020, survey_points_p)

tmin2021 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2021.nc"))
survey_points_p <- terra::project(survey_points, tmin2021)
buffers <- buffer(survey_points_p, 5000)
tmin2021c <- crop(tmin2021, buffers)
tmin_mean2021 <- mean(tmin2021c)
surveys$tmin2021 <- extract(tmin_mean2021, survey_points_p)

tmin2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2022.nc"))
survey_points_p <- terra::project(survey_points, tmin2022)
buffers <- buffer(survey_points_p, 5000)
tmin2022c <- crop(tmin2022, buffers)
tmin_mean2022 <- mean(tmin2022c)
surveys$tmin2022 <- extract(tmin_mean2022, survey_points_p)

tmin2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2023.nc"))
survey_points_p <- terra::project(survey_points, tmin2023)
buffers <- buffer(survey_points_p, 5000)
tmin2023c <- crop(tmin2023, buffers)
tmin_mean2023 <- mean(tmin2023c)
surveys$tmin2023 <- extract(tmin_mean2023, survey_points_p)

tmin2024 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2024.nc"))
survey_points_p <- terra::project(survey_points, tmin2024)
buffers <- buffer(survey_points_p, 5000)
tmin2024c <- crop(tmin2024, buffers)
tmin_mean2024 <- mean(tmin2024c)
surveys$tmin2024 <- extract(tmin_mean2024, survey_points_p)

#save checkpoint####
write.csv(surveys, "surveys_clean.csv")
surveys <- read.csv("surveys_clean.csv")

#overwintering time period October 1.day 274 to March 1. day 60 ####
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)

tminw05_06 <- c(tmin2005c[[274:365]], tmin2006c[[1:60]]) 
tmin05_06r <- terra::classify(tminw05_06, rclmat, right = FALSE) 
tmin05_06r_sum <- app(tmin05_06r, fun="sum")

tminw06_07 <- c(tmin2006c[[274:365]], tmin2007c[[1:60]]) 
tmin06_07r <- terra::classify(tminw06_07, rclmat, right = FALSE) 
tmin06_07r_sum <- app(tmin06_07r, fun="sum")

tminw07_08 <- c(tmin2008c[[274:365]], tmin2008c[[1:60]]) 
tmin07_08r <- terra::classify(tminw07_08, rclmat, right = FALSE) 
tmin07_08r_sum <- app(tmin07_08r, fun="sum")

tminw08_09 <- c(tmin2008c[[274:365]], tmin2009c[[1:60]]) 
tmin08_09r <- terra::classify(tminw08_09, rclmat, right = FALSE) 
tmin08_09r_sum <- app(tmin08_09r, fun="sum")

tminw09_10 <- c(tmin2009c[[274:365]], tmin2010c[[1:60]]) 
tmin09_10r <- terra::classify(tminw09_10, rclmat, right = FALSE) 
tmin09_10r_sum <- app(tmin09_10r, fun="sum")

tminw10_11 <- c(tmin2010c[[274:365]], tmin2011c[[1:60]]) 
tmin10_11r <- terra::classify(tminw10_11, rclmat, right = FALSE) 
tmin10_11r_sum <- app(tmin10_11r, fun="sum")

tminw11_12 <- c(tmin2011c[[274:365]], tmin2012c[[1:60]]) 
tmin11_12r <- terra::classify(tminw11_12, rclmat, right = FALSE) 
tmin11_12r_sum <- app(tmin11_12r, fun="sum")

tminw12_13 <- c(tmin2012c[[274:365]], tmin2013c[[1:60]]) 
tmin12_13r <- terra::classify(tminw12_13, rclmat, right = FALSE) 
tmin12_13r_sum <- app(tmin12_13r, fun="sum")

tminw13_14 <- c(tmin2013c[[274:365]], tmin2014c[[1:60]]) 
tmin13_14r <- terra::classify(tminw13_14, rclmat, right = FALSE) 
tmin13_14r_sum <- app(tmin13_14r, fun="sum")

tminw14_15 <- c(tmin2014c[[274:365]], tmin2015c[[1:60]]) 
tmin14_15r <- terra::classify(tminw14_15, rclmat, right = FALSE) 
tmin14_15r_sum <- app(tmin14_15r, fun="sum")

tminw15_16 <- c(tmin2015c[[274:365]], tmin2016c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin15_16r <- terra::classify(tminw15_16, rclmat, right = FALSE) 
tmin15_16r_sum <- app(tmin15_16r, fun="sum")

tminw16_17 <- c(tmin2016c[[274:365]], tmin2017c[[1:60]]) 
tmin16_17r <- terra::classify(tminw16_17, rclmat, right = FALSE) 
tmin16_17r_sum <- app(tmin16_17r, fun="sum")

tminw17_18 <- c(tmin2017c[[274:365]], tmin2018c[[1:60]]) 
tmin17_18r <- terra::classify(tminw17_18, rclmat, right = FALSE) 
tmin17_18r_sum <- app(tmin17_18r, fun="sum")

tminw18_19 <- c(tmin2018c[[274:365]], tmin2019c[[1:60]]) 
tmin18_19r <- terra::classify(tminw18_19, rclmat, right = FALSE) 
tmin18_19r_sum <- app(tmin18_19r, fun="sum")

tminw19_20 <- c(tmin2019c[[274:365]], tmin2020c[[1:60]]) 
tmin19_20r <- terra::classify(tminw19_20, rclmat, right = FALSE) 
tmin19_20r_sum <- app(tmin19_20r, fun="sum")

tminw20_21 <- c(tmin2020c[[274:365]], tmin2021c[[1:60]]) 
tmin20_21r <- terra::classify(tminw20_21, rclmat, right = FALSE) 
tmin20_21r_sum <- app(tmin20_21r, fun="sum")

tminw21_22 <- c(tmin2021c[[274:365]], tmin2022c[[1:60]]) 
tmin21_22r <- terra::classify(tminw21_22, rclmat, right = FALSE) 
tmin21_22r_sum <- app(tmin21_22r, fun="sum")

tminw22_23 <- c(tmin2022c[[274:365]], tmin2023c[[1:60]]) 
tmin22_23r <- terra::classify(tminw22_23, rclmat, right = FALSE) 
tmin22_23r_sum <- app(tmin22_23r, fun="sum")

tminw23_24 <- c(tmin2023c[[274:365]], tmin2024c[[1:60]]) 
tmin23_24r <- terra::classify(tminw23_24, rclmat, right = FALSE) 
tmin23_24r_sum <- app(tmin23_24r, fun="sum")

all_yearsbelowneg5c <- c(tmin05_06r_sum, tmin06_07r_sum, tmin07_08r_sum, 
                tmin08_09r_sum, tmin09_10r_sum, tmin10_11r_sum,
                tmin11_12r_sum, tmin12_13r_sum, tmin13_14r_sum,
                tmin14_15r_sum, tmin15_16r_sum, tmin16_17r_sum,
                tmin17_18r_sum, tmin18_19r_sum, tmin19_20r_sum,
                tmin20_21r_sum, tmin21_22r_sum, tmin22_23r_sum, 
                tmin23_24r_sum) ###stacked days below -5

#mean below -5°C ####
mean_days_belowneg5c <- app(all_yearsbelowneg5c, fun="mean")
sd_days_belowneg5c <- app(all_yearsbelowneg5c, fun="sd")
max_days_belowneg5c <- app(all_yearsbelowneg5c, fun="max")
min_days_belowneg5c <- app(all_yearsbelowneg5c, fun="min")

survey_points_proj <- project(survey_points, crs(mean_days_belowneg5c))
meanbelowneg5_vals <- terra::extract(mean_days_belowneg5c, survey_points_proj)
surveys <- cbind(surveys, meanbelowneg5_vals)
colnames(surveys)[colnames(surveys) == "mean"] <- "all_years_mean_days_belowneg5"

survey_points_proj <- project(survey_points, crs(sd_days_belowneg5c))
sdbelowneg5_vals <- terra::extract(sd_days_belowneg5c, survey_points_proj)
surveys <- cbind(surveys, sdbelowneg5_vals)
colnames(surveys)[colnames(surveys) == "sd"] <- "all_years_sd_days_belowneg5"

#save checkpoint####
write.csv(surveys, "surveys_clean.csv")
surveys <- read.csv("surveys_clean.csv")

#load tmax from 2005-2024 ####
tmax2005 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2005.nc"))
survey_points_p <- terra::project(survey_points, tmax2005)
buffers <- buffer(survey_points_p, 5000)
tmax2005c <- crop(tmax2005, buffers)
tmax_mean2005 <- mean(tmax2005c)
surveys$tmax2005 <- extract(tmax_mean2005, survey_points_p)

tmax2006 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2006.nc"))
survey_points_p <- terra::project(survey_points, tmax2006)
buffers <- buffer(survey_points_p, 5000)
tmax2006c <- crop(tmax2006, buffers)
tmax_mean2006 <- mean(tmax2006c)
surveys$tmax2006 <- extract(tmax_mean2006, survey_points_p)

tmax2007 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2007.nc"))
survey_points_p <- terra::project(survey_points, tmax2007)
buffers <- buffer(survey_points_p, 5000)
tmax2007c <- crop(tmax2007, buffers)
tmax_mean2007 <- mean(tmax2007c)
surveys$tmax2007 <- extract(tmax_mean2007, survey_points_p)

tmax2008 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2008.nc"))
survey_points_p <- terra::project(survey_points, tmax2008)
buffers <- buffer(survey_points_p, 5000)
tmax2008c <- crop(tmax2008, buffers)
tmax_mean2008 <- mean(tmax2008c)
surveys$tmax2008 <- extract(tmax_mean2008, survey_points_p)

tmax2009 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2009.nc"))
survey_points_p <- terra::project(survey_points, tmax2009)
buffers <- buffer(survey_points_p, 5000)
tmax2009c <- crop(tmax2009, buffers)
tmax_mean2009 <- mean(tmax2009c)
surveys$tmax2009 <- extract(tmax_mean2009, survey_points_p)

tmax2010 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2010.nc"))
survey_points_p <- terra::project(survey_points, tmax2010)
buffers <- buffer(survey_points_p, 5000)
tmax2010c <- crop(tmax2010, buffers)
tmax_mean2010 <- mean(tmax2010c)
surveys$tmax2010 <- extract(tmax_mean2010, survey_points_p)

tmax2011 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2011.nc"))
survey_points_p <- terra::project(survey_points, tmax2011)
buffers <- buffer(survey_points_p, 5000)
tmax2011c <- crop(tmax2011, buffers)
tmax_mean2011 <- mean(tmax2011c)
surveys$tmax2011 <- extract(tmax_mean2011, survey_points_p)

tmax2012 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2012.nc"))
survey_points_p <- terra::project(survey_points, tmax2012)
buffers <- buffer(survey_points_p, 5000)
tmax2012c <- crop(tmax2012, buffers)
tmax_mean2012 <- mean(tmax2012c)
surveys$tmax2012 <- extract(tmax_mean2012, survey_points_p)

tmax2013 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2013.nc"))
survey_points_p <- terra::project(survey_points, tmax2013)
buffers <- buffer(survey_points_p, 5000)
tmax2013c <- crop(tmax2013, buffers)
tmax_mean2013 <- mean(tmax2013c)
surveys$tmax2013 <- extract(tmax_mean2013, survey_points_p)

tmax2014 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2014.nc"))
survey_points_p <- terra::project(survey_points, tmax2014)
buffers <- buffer(survey_points_p, 5000)
tmax2014c <- crop(tmax2014, buffers)
tmax_mean2014 <- mean(tmax2014c)
surveys$tmax2014 <- extract(tmax_mean2014, survey_points_p)

tmax2015 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2015.nc"))
survey_points_p <- terra::project(survey_points, tmax2015)
buffers <- buffer(survey_points_p, 5000)
tmax2015c <- crop(tmax2015, buffers)
tmax_mean2015 <- mean(tmax2015c)
surveys$tmax2015 <- extract(tmax_mean2015, survey_points_p)

tmax2016 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2016.nc"))
survey_points_p <- terra::project(survey_points, tmax2016)
buffers <- buffer(survey_points_p, 5000)
tmax2016c <- crop(tmax2016, buffers)
tmax_mean2016 <- mean(tmax2016c)
surveys$tmax2016 <- extract(tmax_mean2016, survey_points_p)

tmax2017 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2017.nc"))
survey_points_p <- terra::project(survey_points, tmax2017)
buffers <- buffer(survey_points_p, 5000)
tmax2017c <- crop(tmax2017, buffers)
tmax_mean2017 <- mean(tmax2017c)
surveys$tmax2017 <- extract(tmax_mean2017, survey_points_p)

tmax2018 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2018.nc"))
survey_points_p <- terra::project(survey_points, tmax2018)
buffers <- buffer(survey_points_p, 5000)
tmax2018c <- crop(tmax2018, buffers)
tmax_mean2018 <- mean(tmax2018c)
surveys$tmax2018 <- extract(tmax_mean2018, survey_points_p)

tmax2019 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2019.nc"))
survey_points_p <- terra::project(survey_points, tmax2019)
buffers <- buffer(survey_points_p, 5000)
tmax2019c <- crop(tmax2019, buffers)
tmax_mean2019 <- mean(tmax2019c)
surveys$tmax2019 <- extract(tmax_mean2019, survey_points_p)

tmax2020 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2020.nc"))
survey_points_p <- terra::project(survey_points, tmax2020)
buffers <- buffer(survey_points_p, 5000)
tmax2020c <- crop(tmax2020, buffers)
tmax_mean2020 <- mean(tmax2020c)
surveys$tmax2020 <- extract(tmax_mean2020, survey_points_p)

tmax2021 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2021.nc"))
survey_points_p <- terra::project(survey_points, tmax2021)
buffers <- buffer(survey_points_p, 5000)
tmax2021c <- crop(tmax2021, buffers)
tmax_mean2021 <- mean(tmax2021c)
surveys$tmax2021 <- extract(tmax_mean2021, survey_points_p)

tmax2022 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2022.nc"))
survey_points_p <- terra::project(survey_points, tmax2022)
buffers <- buffer(survey_points_p, 5000)
tmax2022c <- crop(tmax2022, buffers)
tmax_mean2022 <- mean(tmax2022c)
surveys$tmax2022 <- extract(tmax_mean2022, survey_points_p)

tmax2023 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2023.nc"))
survey_points_p <- terra::project(survey_points, tmax2023)
buffers <- buffer(survey_points_p, 5000)
tmax2023c <- crop(tmax2023, buffers)
tmax_mean2023 <- mean(tmax2023c)
surveys$tmax2023 <- extract(tmax_mean2023, survey_points_p)

tmax2024 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmax/daymet_v4_daily_na_tmax_2024.nc"))
survey_points_p <- terra::project(survey_points, tmax2024)
buffers <- buffer(survey_points_p, 5000)
tmax2024c <- crop(tmax2024, buffers)
tmax_mean2024 <- mean(tmax2024c)
surveys$tmax2024 <- extract(tmax_mean2024, survey_points_p)

#t max temps greater or equal to 30 reclassification ####
rcl2 <- c(-Inf, 30, 0, 30, Inf, 1)
rclmat2 <- matrix(rcl2, ncol = 3, byrow = TRUE)

tmax05ge30 <- terra::classify(tmax2005c, rclmat2, right = TRUE)
tmax05ge30_sum <- app(tmax05ge30, fun = "sum")
t05daysover30 <- extract(tmax05ge30_sum, survey_points_p)

tmax06ge30 <- terra::classify(tmax2006c, rclmat2, right = TRUE)
tmax06ge30_sum <- app(tmax06ge30, fun = "sum")
t06daysover30 <- extract(tmax06ge30_sum, survey_points_p)

tmax07ge30 <- terra::classify(tmax2007c, rclmat2, right = TRUE)
tmax07ge30_sum <- app(tmax07ge30, fun = "sum")
t07daysover30 <- extract(tmax07ge30_sum, survey_points_p)

tmax08ge30 <- terra::classify(tmax2008c, rclmat2, right = TRUE)
tmax08ge30_sum <- app(tmax08ge30, fun = "sum")
t08daysover30 <- extract(tmax08ge30_sum, survey_points_p)

tmax09ge30 <- terra::classify(tmax2009c, rclmat2, right = TRUE)
tmax09ge30_sum <- app(tmax09ge30, fun = "sum")
t09daysover30 <- extract(tmax09ge30_sum, survey_points_p)

tmax10ge30 <- terra::classify(tmax2010c, rclmat2, right = TRUE)
tmax10ge30_sum <- app(tmax10ge30, fun = "sum")
t10daysover30 <- extract(tmax10ge30_sum, survey_points_p)

tmax11ge30 <- terra::classify(tmax2011c, rclmat2, right = TRUE)
tmax11ge30_sum <- app(tmax11ge30, fun = "sum")
t11daysover30 <- extract(tmax11ge30_sum, survey_points_p)

tmax12ge30 <- terra::classify(tmax2012c, rclmat2, right = TRUE)
tmax12ge30_sum <- app(tmax12ge30, fun = "sum")
t12daysover30 <- extract(tmax12ge30_sum, survey_points_p)

tmax13ge30 <- terra::classify(tmax2013c, rclmat2, right = TRUE)
tmax13ge30_sum <- app(tmax13ge30, fun = "sum")
t13daysover30 <- extract(tmax13ge30_sum, survey_points_p)

tmax14ge30 <- terra::classify(tmax2014c, rclmat2, right = TRUE)
tmax14ge30_sum <- app(tmax14ge30, fun = "sum")
t14daysover30 <- extract(tmax14ge30_sum, survey_points_p)

tmax15ge30 <- terra::classify(tmax2015c, rclmat2, right = TRUE)
tmax15ge30_sum <- app(tmax15ge30, fun = "sum")
t15daysover30 <- extract(tmax15ge30_sum, survey_points_p)

tmax16ge30 <- terra::classify(tmax2016c, rclmat2, right = TRUE)
tmax16ge30_sum <- app(tmax16ge30, fun = "sum")
t16daysover30 <- extract(tmax16ge30_sum, survey_points_p)

tmax17ge30 <- terra::classify(tmax2017c, rclmat2, right = TRUE)
tmax17ge30_sum <- app(tmax17ge30, fun = "sum")
t17daysover30 <- extract(tmax17ge30_sum, survey_points_p)

tmax18ge30 <- terra::classify(tmax2018c, rclmat2, right = TRUE)
tmax18ge30_sum <- app(tmax18ge30, fun = "sum")
t18daysover30 <- extract(tmax18ge30_sum, survey_points_p)

tmax19ge30 <- terra::classify(tmax2019c, rclmat2, right = TRUE)
tmax19ge30_sum <- app(tmax19ge30, fun = "sum")
t19daysover30 <- extract(tmax19ge30_sum, survey_points_p)

tmax20ge30 <- terra::classify(tmax2020c, rclmat2, right = TRUE)
tmax20ge30_sum <- app(tmax20ge30, fun = "sum")
t20daysover30 <- extract(tmax20ge30_sum, survey_points_p)

tmax21ge30 <- terra::classify(tmax2021c, rclmat2, right = TRUE)
tmax21ge30_sum <- app(tmax21ge30, fun = "sum")
t21daysover30 <- extract(tmax21ge30_sum, survey_points_p)

tmax22ge30 <- terra::classify(tmax2022c, rclmat2, right = TRUE)
tmax22ge30_sum <- app(tmax22ge30, fun = "sum")
t22daysover30 <- extract(tmax22ge30_sum, survey_points_p)

tmax23ge30 <- terra::classify(tmax2023c, rclmat2, right = TRUE)
tmax23ge30_sum <- app(tmax23ge30, fun = "sum")
t23daysover30 <- extract(tmax23ge30_sum, survey_points_p)

tmax24ge30 <- terra::classify(tmax2024c, rclmat2, right = TRUE)
tmax24ge30_sum <- app(tmax24ge30, fun = "sum")
t24daysover30 <- extract(tmax24ge30_sum, survey_points_p)

all_yearsover30c <- c(tmax05ge30_sum, tmax06ge30_sum, tmax07ge30_sum, 
                      tmax08ge30_sum, tmax09ge30_sum, tmax10ge30_sum,
                      tmax11ge30_sum, tmax12ge30_sum, tmax13ge30_sum,
                      tmax14ge30_sum, tmax15ge30_sum, tmax16ge30_sum,
                      tmax17ge30_sum, tmax18ge30_sum, tmax19ge30_sum,
                      tmax20ge30_sum, tmax21ge30_sum, tmax22ge30_sum, 
                      tmax23ge30_sum, tmax24ge30_sum)

#mean over 30°C ####
mean_days_over30c <- app(all_yearsover30c, fun="mean")
sd_days_over30c <- app(all_yearsover30c, fun="sd")
max_days_over30c <- app(all_yearsover30c, fun="max")
min_days_over30c <- app(all_yearsover30c, fun="min")

survey_points_proj <- project(survey_points, crs(mean_days_over30c))
meanover30_vals <- terra::extract(mean_days_over30c, survey_points_proj)
surveys <- cbind(surveys, meanover30_vals)
colnames(surveys)[colnames(surveys) == "mean"] <- "all_years_mean_days_over30"

survey_points_proj <- project(survey_points, crs(sd_days_over30c))
sdover30_vals <- terra::extract(sd_days_over30c, survey_points_proj)
surveys <- cbind(surveys, sdover30_vals)
colnames(surveys)[colnames(surveys) == "sd"] <- "all_years_sd_days_over30"

write.csv(surveys, "surveys_clean.csv")
surveys <- read.csv("surveys_clean.csv")

#distance ####
##site to site proximity as variable####
survey_points_proj <- project(survey_points, crs(nc))
buffers_10km <- buffer(survey_points_proj, width = 10000)
buffers_5km <- buffer(survey_points_proj, width = 5000)

plot(nc, col = "gray90", border = "white")
plot(survey_points, add = TRUE, col = "black", pch = 19, cex = 0.5)
#10km buffer plot
plot(buffers_10km, add = TRUE, border = "green", lwd = 1)
#5km buffer plot
plot(buffers_5km, add = TRUE, border = "blue", lwd = 1)

#precense points
presence_subset <- surveys[surveys$ST.Status == "1", ]
presence_points <- vect(presence_subset, geom = c("Lon", "Lat"), crs("epsg:4326")) 
plot(presence_points, add = TRUE, col = "yellow", pch = 19, cex = 0.5)

#10km overlap
overlap10km_matrix <- relate(survey_points_proj, buffers_10km, relation = "intersects")
overlap10km_count <- rowSums(overlap10km_matrix) - 1
surveys$overlap_count10km <- overlap10km_count

glm_overlap10km <- glm(ST.Status ~ overlap10km_count, data = surveys, family = binomial)
summary(glm_overlap10km) #negative effect, p value = 0.49701

#5km overlap
overlap5km_matrix <- relate(survey_points_proj, buffers_5km, "intersects")
overlap5km_count <- rowSums(overlap5km_matrix) - 1
surveys$overlap_count5km <- overlap5km_count

glm_overlap5km <- glm(ST.Status ~ overlap5km_count, data = surveys, family = binomial)
summary(glm_overlap5km) #negative effect, p value = 0.0621

ncol_surveys <- ncol(surveys)
for (i in seq(1:10)) {
  buffer <- buffer(survey_points_proj, width = i * 1000)
  surveys[, ncol_surveys + i] <- rowSums(relate(survey_points_proj, buffer, "intersects"))
  summary(glm(ST.Status ~ surveys[, ncol_surveys + i], surveys, family = binomial))
}

#save checkpoint####
write.csv(surveys, "surveys_clean.csv")
surveys <- read.csv("surveys_clean.csv")

#box plots ####
means_below5_plot <- ggplot(surveys, aes(x = factor(ST.Status), y = all_years_mean_days_belowneg5)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, color = "red", size = 3) +
  xlab("Species Presence (0=absent,1=present)") +
  ylab("Mean Days Below -5°C") +
  theme_minimal()
ggsave(ggsave("meandaysbelow5_presence.png", plot = means_belowneg5_plot, width = 6, height = 4, dpi = 300))

means_over30_plot <- ggplot(surveys, aes(x = factor(ST.Status), y = all_years_mean_days_over30)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, color = "red", size = 3) +
  xlab("Species Presence (0=absent,1=present)") +
  ylab("Mean Greater or Equal to 30°C") +
  theme_minimal()
ggsave(ggsave("meandaysover30_presence.png", plot = means_over30_plot, width = 6, height = 4, dpi = 300))
plot(means_over30_plot)


#normalize means first ####

#k means clustering ####
fviz_nbclust(surveys[,c("all_years_mean_days_over30", "precip2022.mean", "all_years_mean_days_belowneg5", "elevation.elevation", "overlap_count5km")], kmeans, method = "wss")
km <- kmeans(surveys[,c("all_years_mean_days_over30", "precip2022.mean", "all_years_mean_days_belowneg5", "elevation.elevation", "overlap_count5km")], centers = 4, nstart = 25)
km

surveys$cluster <- km$cluster