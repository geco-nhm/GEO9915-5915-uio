# GEO 5915/9915
# University of Oslo, May 2022
# Olav Skarpaas, Lasse Torben Keetz, Eirik Aasmo Finne

# DM tutorial
#############

# In this tutorial we will run quickly through the steps in distribution modelling
# from data to model fitting, evaluation and map predictions.
# WARNING! These exerceises neither demonstrates all aspects of good modelling practice,
# nor great models - it is just a quick and simple start.

# Preparations:

# Download and install R: https://cran.r-project.org/
# Download and install R studio: https://www.rstudio.com/

# Open and run (or source) the file 'Dataprep_global.r' or 'Dataprep_regional.r'- if this should fail get
# the data from the shared workshop folder
# Download and install packages:
# install.packages(c("raster","fields","rgdal", "dismo", "ecospat"))


# Exercise 1: a (too) simple glm of model species based on temperature ----

# Load packages
library(raster) # for raster data tools
library(fields) # for tim.colors and other color palettes
library(rgdal)
library(dismo)
library(ecospat)

# Load training data
setwd("C:/Users/Your/working/directory") ##Edit to your own path to working dir
load("Data/DM_tutorial_training_data") ### remember to change the name if necessary to use the pre-made output files

# View top and bottom of data
head(training_data)
tail(training_data)

# Load temperature map
temp <- raster("Data/DM_tutorial_predictor_maps",layer="temp")

#species <- "Letharia vulpina" ### If needed, state your model species again
# Plot temperature map and observations
plot(temp,main=paste("Temperature (annual mean) and","GBIF occurrences of",species))
presence_points <- training_data[training_data$presence==1, c("x","y")]
points(presence_points)                    # Presence points from GBIF (see Dataprep.r)
absence_points <- training_data[training_data$presence==0, c("x","y")]
points(absence_points,pch="+",col="red")   # Random absence points (see Dataprep.r)


# Fit a first regression model to training data using GLM
m1 <- glm(presence~temp,family=binomial,data=training_data)

# Inspect model
summary(m1)

# Make map of predictions
p1 <- predict(temp,m1,type="response")
plot(p1,col=tim.colors(64))

# Put plots of observations and predictions on the same page,
# with headers and different colors for predicted probabilities of presence
par(mfrow=c(1,2),mar=c(2,2,4,5)+0.1)
plot(temp,main="Temperature")
points(presence_points)
points(absence_points,pch="+",col="red")
plot(p1,col=tim.colors(64),main=paste("Probability of presence - model 1"))

### Discussion
# Interpret and discuss the model and predictions. Happy?



# Exercise 2: a slightly better glm? by improving temperature response ----


# A new model, with a second order term for temperature
m2 <- glm(presence~temp+I(temp^2),family=binomial,data=training_data)
# Inspect model 2
summary(m2)
# Plot predictions below model 1
p2 <- predict(temp,m2,type="response")


par(mfrow=c(1,2))
plot(p1,col=tim.colors(64),main=paste("Probability of presence - model 1"))
plot(p2,col=tim.colors(64),main=paste("Probability of presence - model 2"))
### Discussion
# Compare model 1 and 2 (maps and model summaries), discuss differences/improvements



# Exercise 3: Adding precipitation as a predictor ----

# A third model, with second order terms for both temperature and precipitation
m3 <- glm(presence~temp+I(temp^2)+prec+I(prec^2),family=binomial,data=training_data)

# Inspect model 3 and compare to model 1 and 2
summary(m3)

# Load temperature and precipitation maps
predictor_maps <- stack("Data/DM_tutorial_predictor_maps")

# Plot predictions and compare to model 1 and 2
p3 <- predict(predictor_maps,m3,type="response")
par(mfrow=c(1,3))
plot(p1,col=tim.colors(64),main=paste("Probability of presence - model 1"))
plot(p2,col=tim.colors(64),main=paste("Probability of presence - model 2"))
plot(p3,col=tim.colors(64),main=paste("Probability of presence - model 3"))

### Discussion
# Compare model 3 to model 1 and 2 (maps and model summaries), discuss differences/improvements



# Exercise 4: model validation ----

# Get evaluation data: ideally independent, here using training data (see Discussion below for exercise with subset validation)
evaluation_data <- training_data

# Evaluation of the three models with ROC plot and AUC
ev_pres <- evaluation_data[evaluation_data$presence==1,]  # Selecting independent evaluation data presences with predictors as evaluation presence data set
ev_abs <- evaluation_data[evaluation_data$presence==0,]   # Selecting independent evaluation data absences with predictors as validation absence data set
ev1 <- evaluate(p=ev_pres,a=ev_abs,model=m1)              # Compute ROC and AUC for model 1
ev2 <- evaluate(p=ev_pres,a=ev_abs,model=m2)              # Compute ROC and AUC for model 2
ev3 <- evaluate(p=ev_pres,a=ev_abs,model=m3)              # Compute ROC and AUC for model 3
par(mfrow=(c(1,3)))
plot(ev1,"ROC");mtext("Model 1",line=3)                   # Plot ROC model 1
plot(ev2,"ROC");mtext("Model 2",line=3)                   # Plot ROC model 2
plot(ev3,"ROC");mtext("Model 3",line=3)                   # Plot ROC model 3


###Evaluation with Continious Boyce Index (CBI)

training_data_naomit<-training_data[complete.cases(training_data),]##remove any rows with NA data, to work with ecospat.boyce

##Extract model output for different habitat suitabilities, both presences and background sites
### Better to use the raster as background, but that is slower to compute..

#Compute model prediction of habitat suitability for both presences and background data (here we will use our pseudo-absences)
pred1<-predict(m1, training_data_naomit["temp"], type="response") 
pred2<-predict(m2, training_data_naomit["temp"], type="response")
pred3<-predict(m3, training_data_naomit[,c("temp", "prec")], type="response")

obs1<-pred1[which(training_data_naomit$presence==1)] ## extract the habitat suitability for the sites with presences
obs2<-pred2[which(training_data_naomit$presence==1)]
obs3<-pred3[which(training_data_naomit$presence==1)]

boyce1<-ecospat.boyce(fit=pred1, obs1, nclass = 0, window.w = "default", res=100, PEplot=TRUE)
legend("bottomright", legend =paste("Spearman cor", boyce1$Spearman.cor), title = "Model 1", box.lty = 0, bg="transparent")
boyce2<-ecospat.boyce(fit=pred2, obs2, nclass = 0, window.w = "default", res=100, PEplot=TRUE)
legend("bottomright", legend =paste("Spearman cor", boyce2$Spearman.cor),title = "Model 2", box.lty = 0, bg="transparent")
boyce3<-ecospat.boyce(fit=pred3, obs3, nclass = 0, window.w = "default", res=100, PEplot=TRUE)
legend("bottomright", legend =paste("Spearman cor", boyce3$Spearman.cor),title = "Model 3", box.lty = 0, bg="transparent")

# nclass=0 means moving bins/classes, i.e. continuous Boyce 
# window.w=The width of the moving window (by default 1/10 of the suitability range)
# res= The resolution of the moving window (by default 100 focals)


### Discussion
# Compare and interpret ROC and CBI plots for the three models.

# If you have time and feel ready for a challenge:
# modify the code above to set aside 20% of the data as
# evaluation data, refit the models with the reduced (80%) training data (exercises 1-4),
# and evaluate the model with the set-aside evaluation data (exercise 4).
# Do you get similar validation results?
# Overall, what do you think about the quality of the models?

# Prepare a few results (tables, graphics) with comments for presentation and discussion in plenary



# Exercise 5: Scenarios ----

# Get modeled climate scenario data CanESM ssp585 (RCP8.5) for 2041-2060
scenario_maps <- stack("Data/DM_tutorial_scenario_maps")

# Plot predictions
p4 <- predict(scenario_maps,m3,type="response")
par(mfrow=c(1,3))
plot(p3,col=tim.colors(64),main=paste("Current probability of", species, "presence")) 
plot(p4,col=tim.colors(64),main=paste("Probability of", species, "presence in 2050 (CMIP6, RCP8.5, CanESM"))
plot(p4-p3,col=tim.colors(64),main=paste("Change"))

### Discussion
# Discuss the forecast. What assumptions are we making? Are they realistic? Are the predicitons realistic?



# Exercise 6: Modelling based on presence only data, using maxent ----

# Install and load MIAmaxent package for model building and selection
library(MIAmaxent)

# Prepare data
path <- getwd()
PO <- readData(paste(path,"/Data/DM_tutorial_training_data_PO.csv",sep=""), ##load our presence only data
                     contEV=paste(path,"/Data/",sep=""),PA=FALSE) ## environmental data (aka temp.asc and prec.asc)

# Plot empirical response curves: frequency of presence (FOP)
par(mfrow=c(2,1))
tempFOP <- plotFOP(PO,"temp")
precFOP <- plotFOP(PO,"prec")

# Forward model selection, see MIAmaxent vignette for details
DV <- deriveVars(PO,transformtype=c("L","D")) # Derive (transform) variables
sDVEV <- selectDVforEV(DV$dvdata)                   # Select derived variables for each predictor (temp, prec)
sEV <- selectEV(sDVEV$dvdata)                       # Select environmental variables (predictors)

# Plot response curves
plotResp(sEV$selectedmodel,DV$transformations,"temp")
plotResp(sEV$selectedmodel,DV$transformations,"prec")
# Discuss predicted response curves. Realistic?

# Predict
par(mfrow=c(1,3))
pMaxent <- projectModel(sEV$selectedmodel,DV$transformations,predictor_maps)

# Evaluate model
testAUC(sEV$selectedmodel,DV$transformations,PO) # Ideally independent data, here using training data

#CBI

PA.d<-read.csv(paste(path,"/Data/DM_tutorial_training_data_PA.csv",sep="")) ## Load the presence and background points
PO.d<-read.csv(paste(path,"/Data/DM_tutorial_training_data_PO.csv",sep="")) ## Load only the presences

PA.d<-na.omit(extract(pMaxent$output,PA.d[c("x", "y")]))#extract model predictions of habitat suitability for the different observation points
PO.d<-na.omit(extract(pMaxent$output,PO.d[c("x", "y")])) #extract only presences.

boyce_maxent<-ecospat.boyce(fit=PA.d, PO.d, nclass = 0, window.w = "default", res=100, PEplot=TRUE) # same as for GLM
legend("bottomright", legend = paste("Spearman cor", boyce_maxent$Spearman.cor), title="Maxent", box.lty = 0, bg="transparent")


### Discussion
# Interpret and discuss map (Relative Predicted Probabilities of Presence - RPPP).
# Compare results to glm.



# Exercise 7. Independent exploration ----

# Work on your own system. Examples of what you can do:

# - Make more complicated models, e.g. with an interaction term between temp and prec: does the interaction improve the model?
# - Try a glm model with many bioclim predictors, do backward model selection (e.g. function 'step'): do other variables explain the distribution better than annual mean temperature (temp) and total precip (prec)?
# - Change the number of pseudo-absence ponits: does this change the models?
# - Consider other climate change scenarios (see 'getData')
# - Explore other distribution modelling packages, e.g. 'dismo', 'sdm', 'ecospat' and try
# their tools for e.g. model ensembles and cross-validation

### Discussion
# Prepare a few results (tables, graphics) with comments for presentation and discussion in plenary

