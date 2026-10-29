## --------------------
## wisdmStsimConnector - prepare transition spatial multipliers
## ApexRMS, April 2024
## --------------------

## Built under R version 4.1.3 & SyncroSim version 3.0.0
## This script prepares output probability maps from WISDM as Transition Spatial
## Multipliers for use in ST-Sim

# source dependencies ----------------------------------------------------------

library(rsyncrosim) # install.packages("C:/GitHub/rsyncrosim", type="source", repos=NULL) 
library(tidyverse)
library(terra)

# Connect to library -----------------------------------------------------------

# Active project and scenario
myLibrary <- ssimLibrary()
myScenario <- scenario()
# datasheet(myScenario)

# path to ssim directories
ssimTempDir <- ssimEnvironment()$TransferDirectory 
resultScenario <- ssimEnvironment()$ScenarioId

# Read in WISDM datasheets
spatialOutputsSheet <- datasheet(myScenario, "wisdm_OutputSpatial", optional = T, lookupsAsFactors = T, )
ensembleOutputsSheet <- datasheet(myScenario, "wisdm_OutputEnsemble", optional = T, lookupsAsFactors = T)

# Read in connector datasheets
optionsSheet <- datasheet(myScenario, "wisdmStsimConnector_Options", optional = T)

# Read in ST-Sim datasheets
transitionSpatialMultiplierSheet <- datasheet(myScenario, "stsim_TransitionSpatialMultiplier", optional = T, empty = T)
initialConditionsSheet <- datasheet(myScenario, "stsim_InitialConditionsSpatial")

# Set progress bar -------------------------------------------------------------

steps <- 6
progressBar(type = "begin", totalSteps = steps)

# Error handling ---------------------------------------------------------------

if(is.na(optionsSheet$Model)){ stop("Target model not defined. Please select a target model before continuing.")}
if(is.na(optionsSheet$BuildFrom)){ stop("Please select the WISMD output to use when building transition spatial multipliers.")}
if(is.na(optionsSheet$BuildFor)){ stop("Please select the ST-Sim transition type that the transition spatial multipliers are being built for.")}

# Prepare WISDM outputs for ST-Sim ----------------------------------------------

# Converts a WISDM raster to a transition spatial multiplier: rescales it to a
# 0-1 suitability using the largest value the WISDM output can take, then maps
# suitability linearly onto [multMin, multMax]
toMultiplier <- function(r, maxValue, multMin, multMax) {
  multMin + (r / maxValue) * (multMax - multMin)
}

# multiplier range; blank values keep the original 0-1 behaviour
multMin <- if (is.null(optionsSheet$MultiplierMinimum) || is.na(optionsSheet$MultiplierMinimum)) 0 else optionsSheet$MultiplierMinimum
multMax <- if (is.null(optionsSheet$MultiplierMaximum) || is.na(optionsSheet$MultiplierMaximum)) 1 else optionsSheet$MultiplierMaximum

# load  initial conditions rasters
stratumRast <- rast(initialConditionsSheet$StratumFileName) # plot(stratumRast)

# select the WISDM raster and the largest value it can take: probability maps
# are 0-100, binary maps 0/1, and sum ensembles add one map per model
if (optionsSheet$BuildFrom == "Probability map"){
  maxValue <- 100
  if (optionsSheet$Model == "Ensemble"){
   fpath <-  ensembleOutputsSheet$ProbabilityRasterMean
   if (is.na(fpath)){
     fpath <- ensembleOutputsSheet$ProbabilityRasterSum
     maxValue <- 100 * sum(!is.na(spatialOutputsSheet$ProbabilityRaster))
   }
  } else {
    fpath <- spatialOutputsSheet$ProbabilityRaster[as.character(spatialOutputsSheet$ModelsID) == as.character(optionsSheet$Model)]
  }
}
if (optionsSheet$BuildFrom == "Binary map"){
  maxValue <- 1
  if (optionsSheet$Model == "Ensemble"){
    fpath <-  ensembleOutputsSheet$BinaryRasterMean
    if (is.na(fpath)){
      fpath <- ensembleOutputsSheet$BinaryRasterSum
      maxValue <- sum(!is.na(spatialOutputsSheet$BinaryRaster))
    }
  } else {
    fpath <- spatialOutputsSheet$BinaryRaster[as.character(spatialOutputsSheet$ModelsID) == as.character(optionsSheet$Model)]
  }
}

# load probability/binary raster from wisdm and clip to target area for stsim model  
wisdmRast <- rast(fpath) # plot(wisdmRast)

outRast <- project(wisdmRast, stratumRast)
progressBar(type = "step")
outRast <- crop(outRast, stratumRast)
progressBar(type = "step")
outRast <- mask(outRast, stratumRast)
progressBar(type = "step")
outRast <- toMultiplier(outRast, maxValue, multMin, multMax)
progressBar(type = "step")


if(is.na(optionsSheet$Timestep)){
  outPath <- file.path(ssimTempDir, tolower(str_c(str_replace(optionsSheet$BuildFor, " ", "_"), "_from_", str_replace(optionsSheet$Model, " ", "_"), "_", str_replace(optionsSheet$BuildFrom, " ", "_"), ".tif")))
} else {
  outPath <- file.path(ssimTempDir, tolower(str_c(str_replace(optionsSheet$BuildFor, " ", "_"), "_from_", str_replace(optionsSheet$Model, " ", "_"), "_", str_replace(optionsSheet$BuildFrom, " ", "_"), "_", optionsSheet$Timestep, ".tif")))
  # outPath <- file.path(ssimTempDir, str_c(str_c(str_replace(optionsSheet$BuildFor, " ", "_"), optionsSheet$Timestep, sep = "_"),".tif"))
}

writeRaster(outRast, 
            filename = outPath,
            gdal="COMPRESS=DEFLATE", 
            NAflag= -9999,
            overwrite = T)

# save raster to transition spatial multiplier datasheet
transitionSpatialMultiplierSheet[1, ] <- NA # empty datasheet has no rows to fill
transitionSpatialMultiplierSheet$Timestep <- optionsSheet$Timestep
transitionSpatialMultiplierSheet$TransitionGroupId <- str_c(optionsSheet$BuildFor, " [Type]")
transitionSpatialMultiplierSheet$MultiplierFileName <- outPath

saveDatasheet(myScenario, transitionSpatialMultiplierSheet, "stsim_TransitionSpatialMultiplier", append = T)
progressBar(type = "end")
