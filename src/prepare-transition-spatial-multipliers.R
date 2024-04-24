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

# load  initial conditions rasters 
stratumRast <- rast(initialConditionsSheet$StratumFileName) # plot(stratumRast)

if (optionsSheet$BuildFrom == "Probability map"){
  if (optionsSheet$Model == "Ensemble"){
   fpath <-  ensembleOutputsSheet$ProbabilityRasterMean 
   if (is.na(fpath)){ fpath <- ensembleOutputsSheet$ProbabilityRasterSum }
  } else {
    fpath <- spatialOutputsSheet$ProbabilityRaster[spatialOutputsSheet$ModelsID == optionsSheet$Model]
  }
}
if (optionsSheet$BuildFrom == "Binary map"){
  if (optionsSheet$Model == "Ensemble"){
    fpath <-  ensembleOutputsSheet$BinaryRasterMean 
    if (is.na(fpath)){ fpath <- ensembleOutputsSheet$BinaryRasterSum }
  } else {
    fpath <- spatialOutputsSheet$BinaryRaster[spatialOutputsSheet$ModelsID == optionsSheet$Model]
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
outRast <- outRast/100
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
transitionSpatialMultiplierSheet$Timestep <- optionsSheet$Timestep
transitionSpatialMultiplierSheet$TransitionGroupId <- str_c(optionsSheet$BuildFor, " [Type]")
transitionSpatialMultiplierSheet$MultiplierFileName <- outPath

saveDatasheet(myScenario, transitionSpatialMultiplierSheet, "stsim_TransitionSpatialMultiplier", append = T)
progressBar(type = "end")
