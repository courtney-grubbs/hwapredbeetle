# HWA Predatory Beetle - Abiotic and Biotic Factors Affecting Species Presence/Survivorship

#data setup ----

# load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(sp)
library(sf)
library(terra)
library(lubridate)
library(factoextra)

data_dir <- "/Volumes/cmjone25"

##survey sites 2022-24 data path
survey_path <- file.path(data_dir, "2022-24_StSurveyAllSites.csv")

surveys <-read_csv(survey_path)

#most up to date df
surveys <- read.csv("surveys_updated.csv")

##plotting into spatial points
survey_points <- vect(surveys, geom = c("Lon", "Lat"), crs("epsg:4326")) 
states <- vect(file.path(data_dir, "Data/Vector/USA/us_lower_48_states.gpkg"))
nc <- states[states$STATE_NAME == "North Carolina",]
plot(nc)
plot(survey_points, add = T)
sp <- crop(survey_points, nc)

##raster data - abiotic factors data path

##maxmium temperature####
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

##minimum temperature####
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

tmin2024 <- rast(file.path("/Volumes/cmjone25/Data/Original/Daymet/tmin/daymet_v4_daily_na_tmin_2024.nc"))
survey_points_p <- terra::project(survey_points, tmin2024)
buffers <- buffer(survey_points_p, 5000)
tmin2024c <- crop(tmin2024, buffers)
tmin_mean2024 <- mean(tmin2024c)
surveys$tmin2024 <- extract(tmin_mean2024, survey_points_p)
plot(tmin_mean2024)
plot(survey_points_p, add = T)

##preciptation####
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
write.csv(surveys, "surveys_updated.csv")
surveys <- read.csv("surveys_updated.csv")

##NEED tmax2024 and precip2024

##elevation####
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

##hemlockbiomass####
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

#data analysis ------

#updating columns
surveys$X2022.Survey[surveys$X2022.Survey == "."] <- NA
surveys$X2022.Survey <- as.numeric(surveys$X2022.Survey)

surveys$X2023.Survey[surveys$X2023.Survey == "."] <- NA
surveys$X2023.Survey <- as.numeric(surveys$X2023.Survey)

surveys$X2024.Survey[surveys$X2024.Survey == "."] <- NA
surveys$X2024.Survey <- as.numeric(surveys$X2024.Survey)

###generalized linear modeling for presence vs variable####
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

elevationmodel <- glm(ST.Status ~ elevation.elevation,
                        data = surveys,
                        family = binomial,
                        na.action = na.omit)
summary(elevationmodel) #p value = 0.30472

#renaming column bc that one was way too long
names(surveys)[names(surveys) == "hemlockbiomass.Hosted_AGB_0261_2018_EASTERN_HEMLOCK_06062023072438"] <- "hemlock.biomass"

hbiomassmodel <- glm(ST.Status ~ hemlock.biomass,
                       data = surveys,
                       family = binomial,
                       na.action = na.omit)
summary(hbiomassmodel) #p value = 0.97993

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

#count released effect
surveys$Count[surveys$Count == "."] <- NA
surveys$Count <- as.numeric(surveys$Count)

count22model <- glm(X2022.Survey ~ Count,
                    data = surveys,
                    family = binomial,
                    na.action = na.omit)
summary(count22model) #p value = 0.949 #may not be used correctly

write.csv(surveys, "surveys_updated.csv")
surveys <- read.csv("surveys_updated.csv")

##kmeans####
fviz_nbclust(surveys[,c("tmax2022.mean", "tmin2022.mean", "elevation.elevation")], kmeans, method = "wss")
km <- kmeans(surveys[,c("tmax2022.mean", "tmin2022.mean", "elevation.elevation")], centers = 4, nstart = 25)
km

##overwintering time period October 1.day 274 to March 1. day 60####
tminw22_23 <- c(tmin2022c[[274:365]], tmin2023c[[1:60]]) #overwinter period Oct 22 to Mar 23
rcl <- c(-Inf, -5, 1, -5, Inf, 0) #reclassifies values: <-5 to 1 and >/= to -5 to 0
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin22_23r <- terra::classify(tminw22_23, rclmat, right = FALSE) #classifying for temp
tmin22_23r_sum <- app(tmin22_23r, fun="sum") #run where fun = mean 
plot(tmin22_23r_sum)

#project w22_23 to survey points
survey_points_w <- terra::project(survey_points, tminw22_23)
buffers <- buffer(survey_points_w, 5000)
w22_23c <- crop(tminw22_23, buffers)
surveys$w22_23 <- extract(w22_23c, survey_points_w)

#extract temperature time series for a single cell
survey_tvals <- tminw22_23[1, 1, ]   #first row, first column across all layers
survey_tvals <- as.numeric(survey_tvals) #convert to numeric vector

#run-length encode
r <- rle(survey_tvals < -5)

#consecutive runs below -5
r$values[r$values == TRUE]  #TRUE blocks correspond to cold spells
r$lengths[r$values == TRUE] #number of consecutive days below -5 for each block

#maximum consecutive cold spell length
max_consec <- max(r$lengths[r$values == TRUE])

#count prolonged cold period ≥ 16 days
long_cold_spells <- r$lengths[r$values]
sum(long_cold_spells >= 16)

#defining function for sum of cold spells
cold_spell_count <- function(x) {
  r <- rle(x < -5)
  sum(r$lengths[r$values] >= 16)
} # sum = 0

write.csv(surveys, "surveys_w22_23.csv") #surveys with all winter 22/23 temps

surveys <- read.csv("surveys_updated.csv")
#load tmin from 2005-2024 (take mean number of days)#####
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

write.csv(surveys, "surveys_alltminmeans.csv") #surveys with all 2005-2024 tmin means

##all_yearsc < c(tmin22_23r_sum, tmin23_24r_sum, etc.) stacked days below -5 ####

tminw05_06 <- c(tmin2005c[[274:365]], tmin2006c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin05_06r <- terra::classify(tminw05_06, rclmat, right = FALSE) 
tmin05_06r_sum <- app(tmin05_06r, fun="sum")

tminw06_07 <- c(tmin2006c[[274:365]], tmin2007c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin06_07r <- terra::classify(tminw06_07, rclmat, right = FALSE) 
tmin06_07r_sum <- app(tmin06_07r, fun="sum")

tminw07_08 <- c(tmin2008c[[274:365]], tmin2008c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin07_08r <- terra::classify(tminw07_08, rclmat, right = FALSE) 
tmin07_08r_sum <- app(tmin07_08r, fun="sum")

tminw08_09 <- c(tmin2008c[[274:365]], tmin2009c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin08_09r <- terra::classify(tminw08_09, rclmat, right = FALSE) 
tmin08_09r_sum <- app(tmin08_09r, fun="sum")

tminw09_10 <- c(tmin2009c[[274:365]], tmin2010c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin09_10r <- terra::classify(tminw09_10, rclmat, right = FALSE) 
tmin09_10r_sum <- app(tmin09_10r, fun="sum")

tminw10_11 <- c(tmin2010c[[274:365]], tmin2011c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin10_11r <- terra::classify(tminw10_11, rclmat, right = FALSE) 
tmin10_11r_sum <- app(tmin10_11r, fun="sum")

tminw11_12 <- c(tmin2011c[[274:365]], tmin2012c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin11_12r <- terra::classify(tminw11_12, rclmat, right = FALSE) 
tmin11_12r_sum <- app(tmin11_12r, fun="sum")

tminw12_13 <- c(tmin2012c[[274:365]], tmin2013c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin12_13r <- terra::classify(tminw12_13, rclmat, right = FALSE) 
tmin12_13r_sum <- app(tmin12_13r, fun="sum")

tminw13_14 <- c(tmin2013c[[274:365]], tmin2014c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin13_14r <- terra::classify(tminw13_14, rclmat, right = FALSE) 
tmin13_14r_sum <- app(tmin13_14r, fun="sum")

tminw14_15 <- c(tmin2014c[[274:365]], tmin2015c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin14_15r <- terra::classify(tminw14_15, rclmat, right = FALSE) 
tmin14_15r_sum <- app(tmin14_15r, fun="sum")

tminw15_16 <- c(tmin2015c[[274:365]], tmin2016c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin15_16r <- terra::classify(tminw15_16, rclmat, right = FALSE) 
tmin15_16r_sum <- app(tmin15_16r, fun="sum")

tminw16_17 <- c(tmin2016c[[274:365]], tmin2017c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin16_17r <- terra::classify(tminw16_17, rclmat, right = FALSE) 
tmin16_17r_sum <- app(tmin16_17r, fun="sum")

tminw17_18 <- c(tmin2017c[[274:365]], tmin2018c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin17_18r <- terra::classify(tminw17_18, rclmat, right = FALSE) 
tmin17_18r_sum <- app(tmin17_18r, fun="sum")

tminw18_19 <- c(tmin2018c[[274:365]], tmin2019c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin18_19r <- terra::classify(tminw18_19, rclmat, right = FALSE) 
tmin18_19r_sum <- app(tmin18_19r, fun="sum")

tminw19_20 <- c(tmin2019c[[274:365]], tmin2020c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin19_20r <- terra::classify(tminw19_20, rclmat, right = FALSE) 
tmin19_20r_sum <- app(tmin19_20r, fun="sum")

tminw20_21 <- c(tmin2020c[[274:365]], tmin2021c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin20_21r <- terra::classify(tminw20_21, rclmat, right = FALSE) 
tmin20_21r_sum <- app(tmin20_21r, fun="sum")

tminw21_22 <- c(tmin2021c[[274:365]], tmin2022c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin21_22r <- terra::classify(tminw21_22, rclmat, right = FALSE) 
tmin21_22r_sum <- app(tmin21_22r, fun="sum")

tminw23_24 <- c(tmin2023c[[274:365]], tmin2024c[[1:60]]) 
rcl <- c(-Inf, -5, 1, -5, Inf, 0) 
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)
tmin23_24r <- terra::classify(tminw23_24, rclmat, right = FALSE) 
tmin23_24r_sum <- app(tmin23_24r, fun="sum")

all_yearsc <- c(tmin05_06r_sum, tmin06_07r_sum, tmin07_08r_sum, 
               tmin08_09r_sum, tmin09_10r_sum, tmin10_11r_sum,
               tmin11_12r_sum, tmin12_13r_sum, tmin13_14r_sum,
               tmin14_15r_sum, tmin15_16r_sum, tmin16_17r_sum,
               tmin17_18r_sum, tmin18_19r_sum, tmin19_20r_sum,
               tmin20_21r_sum, tmin21_22r_sum, tmin22_23r_sum, 
               tmin23_24r_sum) ###stacked days below -5

#mean_days_below5c <- app(all_yearsc, fun "mean") + fun = sd ####
mean_days_below5c <- app(all_yearsc, fun="mean")
sd_days_below5c <- app(all_yearsc, fun="sd")

#run against survey presence ####
survey_points <- vect(surveys, geom = c("Lon", "Lat"), crs = crs(all_yearsc))


