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
library(geosphere)

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

##NEED precip2024

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
max(r$lengths[r$values == TRUE])

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

write.csv(surveys, "surveys_alltminmeans.csv") #surveys with all 2019-2024 tmin means

##load tmax from 2005-2024 (take mean number of days) - trying to loop#####
surveys <- read.csv("surveys_alltminmeans.csv")

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

write.csv(surveys, "surveys_alltempmeans.csv")
surveys <- read_csv("surveys_alltempmeans.csv")

##t max temps greater or equal to 30 reclassification
rcl <- c(-Inf, 30, 0, 30, Inf, 1)
rclmat <- matrix(rcl, ncol = 3, byrow = TRUE)

tmax05ge30 <- terra::classify(tmax2005c, rclmat, right = TRUE)
tmax05ge30_sum <- app(tmax05ge30, fun = "sum")
t05daysover30 <- extract(tmax05ge30_sum, survey_points_p)

tmax06ge30 <- terra::classify(tmax2006c, rclmat, right = TRUE)
tmax06ge30_sum <- app(tmax06ge30, fun = "sum")
t06daysover30 <- extract(tmax06ge30_sum, survey_points_p)

tmax07ge30 <- terra::classify(tmax2007c, rclmat, right = TRUE)
tmax07ge30_sum <- app(tmax07ge30, fun = "sum")
t07daysover30 <- extract(tmax07ge30_sum, survey_points_p)

tmax08ge30 <- terra::classify(tmax2008c, rclmat, right = TRUE)
tmax08ge30_sum <- app(tmax08ge30, fun = "sum")
t08daysover30 <- extract(tmax08ge30_sum, survey_points_p)

tmax09ge30 <- terra::classify(tmax2009c, rclmat, right = TRUE)
tmax09ge30_sum <- app(tmax09ge30, fun = "sum")
t09daysover30 <- extract(tmax09ge30_sum, survey_points_p)

tmax10ge30 <- terra::classify(tmax2010c, rclmat, right = TRUE)
tmax10ge30_sum <- app(tmax10ge30, fun = "sum")
t10daysover30 <- extract(tmax10ge30_sum, survey_points_p)

tmax11ge30 <- terra::classify(tmax2011c, rclmat, right = TRUE)
tmax11ge30_sum <- app(tmax11ge30, fun = "sum")
t11daysover30 <- extract(tmax11ge30_sum, survey_points_p)

tmax12ge30 <- terra::classify(tmax2012c, rclmat, right = TRUE)
tmax12ge30_sum <- app(tmax12ge30, fun = "sum")
t12daysover30 <- extract(tmax12ge30_sum, survey_points_p)

tmax13ge30 <- terra::classify(tmax2013c, rclmat, right = TRUE)
tmax13ge30_sum <- app(tmax13ge30, fun = "sum")
t13daysover30 <- extract(tmax13ge30_sum, survey_points_p)

tmax14ge30 <- terra::classify(tmax2014c, rclmat, right = TRUE)
tmax14ge30_sum <- app(tmax14ge30, fun = "sum")
t14daysover30 <- extract(tmax14ge30_sum, survey_points_p)

tmax15ge30 <- terra::classify(tmax2015c, rclmat, right = TRUE)
tmax15ge30_sum <- app(tmax15ge30, fun = "sum")
t15daysover30 <- extract(tmax15ge30_sum, survey_points_p)

tmax16ge30 <- terra::classify(tmax2016c, rclmat, right = TRUE)
tmax16ge30_sum <- app(tmax16ge30, fun = "sum")
t16daysover30 <- extract(tmax16ge30_sum, survey_points_p)

tmax17ge30 <- terra::classify(tmax2017c, rclmat, right = TRUE)
tmax17ge30_sum <- app(tmax17ge30, fun = "sum")
t17daysover30 <- extract(tmax17ge30_sum, survey_points_p)

tmax18ge30 <- terra::classify(tmax2018c, rclmat, right = TRUE)
tmax18ge30_sum <- app(tmax18ge30, fun = "sum")
t18daysover30 <- extract(tmax18ge30_sum, survey_points_p)

tmax19ge30 <- terra::classify(tmax2019c, rclmat, right = TRUE)
tmax19ge30_sum <- app(tmax19ge30, fun = "sum")
t19daysover30 <- extract(tmax19ge30_sum, survey_points_p)

tmax20ge30 <- terra::classify(tmax2020c, rclmat, right = TRUE)
tmax20ge30_sum <- app(tmax20ge30, fun = "sum")
t20daysover30 <- extract(tmax20ge30_sum, survey_points_p)

tmax21ge30 <- terra::classify(tmax2021c, rclmat, right = TRUE)
tmax21ge30_sum <- app(tmax21ge30, fun = "sum")
t21daysover30 <- extract(tmax21ge30_sum, survey_points_p)

tmax22ge30 <- terra::classify(tmax2022c, rclmat, right = TRUE)
tmax22ge30_sum <- app(tmax22ge30, fun = "sum")
t22daysover30 <- extract(tmax22ge30_sum, survey_points_p)

tmax23ge30 <- terra::classify(tmax2023c, rclmat, right = TRUE)
tmax23ge30_sum <- app(tmax23ge30, fun = "sum")
t23daysover30 <- extract(tmax23ge30_sum, survey_points_p)

tmax24ge30 <- terra::classify(tmax2024c, rclmat, right = TRUE)
tmax24ge30_sum <- app(tmax24ge30, fun = "sum")
t24daysover30 <- extract(tmax24ge30_sum, survey_points_p)


all_yearsover30c <- c(tmax05ge30_sum, tmax06ge30_sum, tmax07ge30_sum, 
                tmax08ge30_sum, tmax09ge30_sum, tmax10ge30_sum,
                tmax11ge30_sum, tmax12ge30_sum, tmax13ge30_sum,
                tmax14ge30_sum, tmax15ge30_sum, tmax16ge30_sum,
                tmax17ge30_sum, tmax18ge30_sum, tmax19ge30_sum,
                tmax20ge30_sum, tmax21ge30_sum, tmax22ge30_sum, 
                tmax23ge30_sum, tmax24ge30_sum)

mean_days_over30c <- app(all_yearsover30c, fun="mean")
sd_days_over30c <- app(all_yearsover30c, fun="sd")

survey_points_proj <- project(survey_points, crs(mean_days_over30c))
meanover30_vals <- terra::extract(mean_days_over30c, survey_points_proj)
surveys_with_meanover30 <- cbind(surveys, meanover30_vals)
colnames(surveys_with_meanover30)[colnames(surveys_with_meanover30) == "mean"] <- "all_years_mean_days_over30"

mean_over30_model <- glm(ST.Status ~ all_years_mean_days_over30, 
                         data = surveys_with_meanover30, 
                         family = binomial)
summary(mean_over30_model) #negative effect, p value = 0.222

boxplot(all_years_mean_days_over30 ~ ST.Status, data = surveys_with_meanover30,
        xlab = "Species Presence (0=absent,1=present)",
        ylab = "Mean Greater or Equal to 30°C")

means_over30_plot <- ggplot(surveys_with_meanover30, aes(x = factor(ST.Status), y = all_years_mean_days_over30)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, color = "red", size = 3) +
  xlab("Species Presence (0=absent,1=present)") +
  ylab("Mean Greater or Equal to 30°C") +
  theme_minimal()
ggsave(ggsave("meandaysover30_presence.png", plot = means_over30_plot, width = 6, height = 4, dpi = 300))
plot(means_over30_plot)

write.csv(surveys_with_meanover30, "surveys_with_meanover30.csv")

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

#mean_days_below-5c <- app(all_yearsc, fun "mean") + fun = sd ####
mean_days_below5c <- app(all_yearsc, fun="mean")
sd_days_below5c <- app(all_yearsc, fun="sd")
max_days_below5c <- app(all_yearsc, fun="max") #test min too and add to surveys to run St.Status

survey_points_proj <- project(survey_points, crs(mean_days_below5c))
mean_vals <- terra::extract(mean_days_below5c, survey_points_proj)
surveys_with_mean <- cbind(surveys, mean_vals)
colnames(surveys_with_mean)[colnames(surveys_with_mean) == "mean"] <- "all_years_mean_days_belowneg5"

boxplot(all_years_mean_days_belowneg5 ~ ST.Status, data = surveys_with_mean,
        xlab = "Species Presence (0=absent,1=present)",
        ylab = "Mean Days Below -5°C")

means_below5_plot <- ggplot(surveys_with_mean, aes(x = factor(ST.Status), y = all_years_mean_days_belowneg5)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 4, color = "red", size = 3) +
  xlab("Species Presence (0=absent,1=present)") +
  ylab("Mean Days Below -5°C") +
  theme_minimal()
ggsave(ggsave("meandaysbelow5_presence.png", plot = means_below5_plot, width = 6, height = 4, dpi = 300))

mean_below5_model <- glm(ST.Status ~ all_years_mean_days_belowneg5, 
                 data = surveys_with_mean, 
                 family = binomial)
summary(mean_below5_model) #p val = 0.346

#number of consec days below -5 all

test <- function(x) {
  r <- rle(x < -5)
  #consecutive runs below -5
  r$lengths[r$values == TRUE] #number of consecutive days below -5 for each block
  print(r$lengths[r$values == TRUE])
  max(r$lengths[r$values == TRUE])
}

tmin20_21r_consecdays <- app(tmin20_21r, fun=test)

surveys_alltminmeans <- read_csv("surveys_alltminmeans.csv")

##site to site proximity as variable####
survey_points_proj <- project(survey_points, crs(nc))
buffers_10km <- buffer(survey_points_proj, width = 10000)
buffers_5km <- buffer(survey_points_proj, width = 5000)
buffers_1km <- buffer(survey_points_proj, width = 1000)

plot(nc, col = "gray90", border = "white")
plot(survey_points, add = TRUE, col = "black", pch = 19, cex = 0.5)
#10km buffer plot
plot(buffers_10km, add = TRUE, border = "green", lwd = 1)
#5km buffer plot
plot(buffers_5km, add = TRUE, border = "blue", lwd = 1)
#1km buffer plot
plot(buffers_1km, add = TRUE, border = "red", lwd = 1)
#precense points
presence_subset <- surveys[surveys$ST.Status == "1", ]
presence_points <- vect(presence_subset, geom = c("Lon", "Lat"), crs("epsg:4326")) 
plot(presence_points, add = TRUE, col = "yellow", pch = 19, cex = 0.5)

#10km overlap
overlap10km_matrix <- relate(survey_points_proj, buffers_10km, relation = "intersects")
overlap10km_count <- rowSums(overlap10km_matrix) - 1
surveys$overlap_count10km <- overlap10km_count
surveys$overlap10km_binary <- ifelse(overlap10km_count > 0, 1, 0) #count may be more effective than binary
glm_overlap10km <- glm(ST.Status ~ overlap10km_count, data = surveys, family = binomial)
summary(glm_overlap10km) #negative effect, p value = 0.49701

#5km overlap
overlap5km_matrix <- relate(survey_points_proj, buffers_5km, "intersects")
overlap5km_count <- rowSums(overlap5km_matrix) - 1
surveys$overlap_count5km <- overlap5km_count
surveys$overlap5km_binary <- ifelse(overlap5km_count > 0, 1, 0)
glm_overlap5km <- glm(ST.Status ~ overlap5km_count, data = surveys, family = binomial)
summary(glm_overlap5km) #negative effect, p value = 0.0621

#1km overlap
overlap1km_matrix <- relate(survey_points_proj, buffers_1km, "intersects")
overlap1km_count <- rowSums(overlap1km_matrix) - 1
surveys$overlap_count1km <- overlap1km_count
surveys$overlap1km_binary <- ifelse(overlap1km_count > 0, 1, 0) 
glm_overlap1km <- glm(ST.Status ~ overlap1km_count, data = surveys, family = binomial)
summary(glm_overlap1km) #positive effect, p value = 0.837

write.csv(surveys, "surveys_distances.csv")

surveys <- read.csv("surveys_updated.csv")

dist_matrix <- distance(survey_points_proj)
dist_df <- as.data.frame(as.table(as.matrix(dist_matrix)))
dist_df <- subset(dist_df, as.numeric(Var1) < as.numeric(Var2))
names(dist_df) <- c("site1", "site2", "distance_m")
dist_10km_sites <- subset(dist_df, distance_m <= 10000)
dist_5km_sites <- subset(dist_df, distance_m <= 5000)

dist_matrix <- as.matrix(dist_matrix)
rownames(dist_matrix) <- surveys$X
colnames(dist_matrix) <- surveys$X

##unique pairs
sites_prox10km <- unique(c(dist_10km_sites$site1, dist_10km_sites$site2))
surveys$sites_prox10km <- ifelse(surveys$X %in% sites_prox10km, 1, 0)
glm_dist10km <- glm(ST.Status ~ sites_prox10km, data = surveys, family = binomial)
summary(glm_dist10km) #negative effect, p value = 0.485

sites_prox5km <- unique(c(dist_5km_sites$site1, dist_5km_sites$site2))
surveys$sites_prox5km <- ifelse(surveys$X %in% sites_prox5km, 1, 0)
glm_dist5km <- glm(ST.Status ~ sites_prox5km, data = surveys, family = binomial)
summary(glm_dist5km) ###wrong showing same values as 10km

surveys$prox_5km <- apply(dist_matrix < 5000, 1, any)
surveys$prox_10km <- apply(dist_matrix < 10000, 1, any)

table(surveys$sites_prox10km, surveys$sites_prox5km)

#trying dist matrix with geosphere
sitecoords <- as.matrix(surveys[, c("Lon", "Lat")])
geodist_matrix <- distm(sitecoords, sitecoords, fun = distHaversine)
geodist_matrix[lower.tri(dist_matrix, diag = TRUE)] <- NA
sites_prx5km_g <- which(dist_matrix <= 5000, arr.ind = TRUE)

##need to combine columns into one "surveys"

##kmeans again
fviz_nbclust(surveys_with_meanover30[,c("all_years_mean_days_over30", "precip2022.mean", "elevation.elevation")], kmeans, method = "wss")
km <- kmeans(surveys_with_meanover30[,c("all_years_mean_days_over30", "precip2022.mean", "elevation.elevation")], centers = 4, nstart = 25)
km

surveys_with_meanover30$cluster <- km$cluster

boxplot(cluster ~ ST.Status, data = surveys_with_meanover30,
        xlab = "Species Presence (0=absent,1=present)",
        ylab = "cluster")

table(surveys_with_meanover30$ST.Status, surveys_with_meanover30$cluster)
