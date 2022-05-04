# GEO 5915/9915
# University of Oslo, May 2022
# Olav Skarpaas, Lasse Torben Keetz, Eirik Aasmo Finne
### This script prepares and writes environmental and species observation data at a GLOBAL scale for the DM tutorial data lab ###
### Each group should choose a model species (see further down) and run both at a regional and global scale. Distribute (no pun intended) the model runs between participants


path <- "C:/Users/Your/working/directory" ## Edit to your own path!
setwd(path)
dir.create(file.path(path, "Data"), showWarnings = FALSE) # if not existing create subdirectory called Data
# Install & load packages
library(raster)
library(rgbif)
library(plyr)
library(rgdal)
library(downloader)

# Predictor variable data: download WorldClim data, extract global temperature and precipitation data
env <- getData("worldclim",var="bio", res=10)
temp <- env[[1]]*0.1; names(temp) <- "temp"        # 1. value is mean annual temperature. Scale values to Celsius (Saved as *10 to reduce file size)
prec <- env[[12]]; names(prec) <- "prec"           # 12. value is total annual precipitation (https://www.worldclim.org/data/bioclim.html)
predictor_maps <- stack(temp,prec)                 # raster stack of current mean annual temperature and total annual precipitation
plot(predictor_maps)

#Load climate scenario data for 2041-2060 (CMIP6, RCP8.5, CanESM) 

###download from shared files in Google Drive:
url<-"https://www.googleapis.com/drive/v3/files/1BlraSYwgebAhcOfragYfLW_Ov42TIoE3?alt=media&key=AIzaSyD_bqOnzJ8OfrUdMmQhVKC8jTYkxylrrZU"
dest<-"Data/cmip6_10m.tif"
download(url,dest,mode="wb")
cmip6 <- stack(dest)

#Alternatively, download directly from WorldClim: https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/10m/wc2.1_10m_bioc_CanESM5_ssp585_2041-2060.zip


cmip6.temp <- cmip6[[1]]; names(cmip6.temp) <- "temp"    # projected mean annual temperature
cmip6.prec <- cmip6[[12]]; names(cmip6.prec) <- "prec"   # projected total annual precipitation
scenario_maps <- stack(cmip6.temp,cmip6.prec)            # raster stack of projected mean annual temperature and total annual precipitation
plot(scenario_maps)


# Response variable data: download GBIF presence data
species <- "Letharia vulpina" ## Choose a species by latin name (https://www.gbif.org/)
# Generalist species that occupy a wide range of environments are often more tricky to model than specialist!
n <- 1000 #number of occurences to include
occ <- occ_search(scientificName=species,limit=n)


# This is a "quick and dirty" approach (for training purposes).
# For scientific studies, use asynchronous downloading of GBIF data, e.g. as described by Anders Finstad:
# https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
# Asynchronous downloading allows larger data sets, and citation of the download with a single doi. 

# Plot observations on predictor maps
plot(temp,main=paste("Temperature (annual mean) and","GBIF occurrences of ",species), )
occ_points <- data.frame(x=occ$data$decimalLongitude,y=occ$data$decimalLatitude)
points(occ_points)


# Rasterize occurrence data
occ_ras <- rasterize(occ_points,predictor_maps,fun='count')
occ_ras                     # some cells have multiple obs
occ_ras <- occ_ras/occ_ras  # convert to presence
occ_points <- rasterToPoints(occ_ras,spatial=TRUE)


# Generate random pseudo-absences
abs_ras <- temp-temp                          # creating dummy raster with zeros
abs_ras[occ_ras==1] <- NA                     # setting cells with presences to NA
abs_points <- sampleRandom(abs_ras,size=nrow(occ_points),sp=TRUE) # sampling cells without presences at random (same number as presence cells)


# Plot rasterized presences and absences on predictor map
plot(temp,main=paste("Temperature (annual mean) and","GBIF occurrences of ",species))
points(occ_points)
points(abs_points,pch="+",col="red")


# Build model training data set (data set for regression): combine presences, absences, and temperatures at these locations
presence_data <- data.frame(occ_points,temp=extract(temp,occ_points),prec=extract(prec,occ_points))
absence_data <- data.frame(abs_points,temp=extract(temp,abs_points),prec=extract(prec,abs_points))
training_data <- rbind(presence_data,absence_data)
training_data <- rename(training_data,c("layer"="presence"))
head(training_data)
tail(training_data)



# Save training data and predictor map for glm
path <- getwd()
save(training_data,file=paste(path,"/Data/DM_tutorial_training_data",sep=""))
writeRaster(predictor_maps,file=paste(path,"/Data/DM_tutorial_predictor_maps",sep=""),overwrite=TRUE)
writeRaster(scenario_maps,file=paste(path,"/Data/DM_tutorial_scenario_maps",sep=""),overwrite=TRUE)




# Save training data and predictor maps for MIAmaxent
training_data_PA <- training_data
training_data_PA$presence[training_data_PA$presence==0] <- NA
write.table(training_data_PA,file=paste(path,"/Data/DM_tutorial_training_data_PA.csv",sep=""),sep=",")
training_data_PO <- training_data
training_data_PO <- training_data_PO[training_data_PO$presence==1,]
write.table(training_data_PO,file=paste(path,"/Data/DM_tutorial_training_data_PO.csv",sep=""),sep=",")
writeRaster(temp,file=paste(path,"/Data/temp.asc",sep=""),format="ascii",overwrite=TRUE)
writeRaster(prec,file=paste(path,"/Data/prec.asc",sep=""),format="ascii",overwrite=TRUE)

