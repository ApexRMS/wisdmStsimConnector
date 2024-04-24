## --------------------
## wisdmStsimConnector - prepare transition spatial multipliers
## ApexRMS, April 2024
## --------------------

## Built under R version 4.1.3 & SyncroSim version 3.0.0
## This script prepares output probability maps from WISDM as Transition Spatial
## Multipliers for use in ST-Sim

library(rsyncrosim)
library(tidyverse)
library(terra)
library(yaml)

# Settings ---------------------------------------------------------------------

config <- read_yaml("config/config.yml")

tempDir <- config$tempDir
dir.create(tempDir)

wisdmLibPath <- file.path("library", config$wisdmLib)
stsimLibPath <- file.path("library", config$stsimLib)
projectName = config$projectName

ssimDir <- config$ssimDir

# Connect to Libraries ---------------------------------------------------------

ssimSession <- session(ssimDir)

wisdmLib <- ssimLibrary(wisdmLibPath, session = ssimSession)
stsimLib <- ssimLibrary(stsimLibPath, session = ssimSession) 
stsimProject <- rsyncrosim::project(stsimLib, projectName)


# Load WISDM outputs -----------------------------------------------------------

scenarioNames <- scenario(wisdmLib) %>%
  filter(IsResult == "No") 

# get result scenario Ids
sid <- scenario(wisdmLib) %>%
  filter(IsResult == "Yes") %>%
  # Only keep last run from each parent scenario
  group_by(ParentID) %>%
  filter(ScenarioID == max(ScenarioID)) %>%
  pull(ScenarioID) %>%
  set_names(scenarioNames$Name)

# get apply model result scenarios
keepIDs <- NULL
for (id in as.numeric(sid)){
  pipeline <- datasheet(wisdmLib, scenario = id, "core_Pipeline")
  if ("6 - Apply Model" %in% pipeline$StageNameID) { keepIDs <- c(keepIDs, id) } 
}

# get output folder location
baseDir <- getwd()
wisdmOutputDir <- str_c(baseDir, "/", filepath(wisdmLib),".output")

# set file paths for wisdm outputs
spatialOutputsDatasheet <- datasheet(wisdmLib, scenario = keepIDs, "wisdm_SpatialOutputs")

# only include outputs for models defined in config
spatialOutputsDatasheet <- spatialOutputsDatasheet[spatialOutputsDatasheet$ModelsID %in% config$modType,]
spatialOutputsDatasheet$ProbabiltyRasterPath <- file.path(wisdmOutputDir, str_c("Scenario-",spatialOutputsDatasheet$ScenarioID), "wisdm_SpatialOutputs" , spatialOutputsDatasheet$ProbabilityRaster)

# Load and Prepare ST-Sim data ------------------------------------------------- 

# load transition multiplier datasheet for st-sim library
# scenario(stsimLib, summary = F) 
# datasheet(ssimObject = stsimLib, scenario = scenario(stsimLib)$ScenarioID[1])

transitionSpatialMultiplierDatasheet <- datasheet(ssimObject = stsimLib, scenario = 23, name = "stsim_TransitionSpatialMultiplier", optional = T)

tsmFolder <- folder(stsimLib)$FolderID[which(folder(stsimLib)$Name == "09 - Transition Spatial Multiplier")]

# load  initial conditions rasters 
initialConditionsDatasheet <- datasheet(ssimObject = stsimLib, scenario = 32, name = "stsim_InitialConditionsSpatial")
stratumRast <- rast(initialConditionsDatasheet$StratumFileName) # plot(stratumRast)

# Save WISDM output rasters to ST-Sim Sub-Scenarios -----------------------------

subScenarioNames <- list()

# create current conditions sub-scenarios
for (i in 1:nrow(spatialOutputsDatasheet)){
  
  if (spatialOutputsDatasheet$ScenarioID[i] %in% config$currentScenarioIDs){
    
    # create transition spatial multiplier sub-scenario 
    scenarioName <- paste0("Transition Spatial Multiplier: Thistle Establishment from WISDM [", spatialOutputsDatasheet$ParentName[i], " - ", spatialOutputsDatasheet$ModelsID[i], "]")
    subScenario <- scenario(stsimProject, scenarioName, overwrite = TRUE, folder = tsmFolder)
    
  # load probability raster from wisdm and clip to target area for stsim model
    probRast <- rast(spatialOutputsDatasheet$ProbabiltyRasterPath[i]) # plot(probRast)
    
    outRast <- project(probRast, stratumRast)
    outRast <- crop(outRast, stratumRast)
    outRast <- mask(outRast, stratumRast)
    outRast <- outRast/100
    
    outPath <- file.path(tempDir, str_c(config$minTimestepCurrentScenario,"_cropped_", basename(spatialOutputsDatasheet$ProbabiltyRasterPath[i])))
    
    writeRaster(outRast, 
                filename = outPath,
                overwrite = T)
    
    # save clipped probability raster to transition spatial multiplier sub-scenario 
    transitionSpatialMultiplierDatasheet$MultiplierFileName <- file.path(getwd(), outPath)
    transitionSpatialMultiplierDatasheet$Timestep <- config$minTimestepCurrentScenario
    
    saveDatasheet(subScenario, transitionSpatialMultiplierDatasheet, "stsim_TransitionSpatialMultiplier", append = F)
    
    subScenarioNames[paste0("[", spatialOutputsDatasheet$ParentName[i], " - ", spatialOutputsDatasheet$ModelsID[i], "]")] <- scenarioName
  }
}

# create future condition sub-scenarios
currentSubScenarios <- subScenarioNames
for (i in 1:nrow(spatialOutputsDatasheet)){
  if (spatialOutputsDatasheet$ScenarioID[i] %in% config$futureScenarioIDs){
    for (j in 1:length(currentSubScenarios)){
      
      # copy the current conditions datasheet
      transitionSpatialMultiplierDatasheet <- datasheet(ssimObject = stsimLib, scenario = currentSubScenarios[[j]], name = "stsim_TransitionSpatialMultiplier", optional = T)
  
      # create transition spatial multiplier sub-scenario 
      scenarioName <- paste0("Transition Spatial Multiplier: Thistle Establishment from WISDM [Canada Thistle - Current & Future Scenarios - ", spatialOutputsDatasheet$ModelsID[i], "]")
      subScenario <- scenario(stsimProject, scenarioName, overwrite = TRUE, folder = tsmFolder)
    
      # load probability raster from wisdm and clip to target area for stsim model
      probRast <- rast(spatialOutputsDatasheet$ProbabiltyRasterPath[i]) # plot(probRast)
    
      outRast <- project(probRast, stratumRast)
      outRast <- crop(outRast, stratumRast)
      outRast <- mask(outRast, stratumRast)
      outRast <- outRast/100
        
      outPath <- file.path(tempDir, str_c(config$minTimestepFutureScenario,"_cropped_", basename(spatialOutputsDatasheet$ProbabiltyRasterPath[i])))
        
      writeRaster(outRast, 
                  filename = outPath,
                  overwrite = T)
      
      # save clipped probability raster to transition spatial multiplier sub-scenario 
      newRow <- data.frame(TransitionGroupID = "Thistle Establishment [Type]") %>%
        mutate(
          Timestep = config$minTimestepFutureScenario,
          MultiplierFileName = file.path(getwd(), outPath)
        )
      transitionSpatialMultiplierDatasheet <- addRow(transitionSpatialMultiplierDatasheet, newRow)
      
      saveDatasheet(subScenario, transitionSpatialMultiplierDatasheet, "stsim_TransitionSpatialMultiplier", append = F)
      
      subScenarioNames[paste0("[Canada Thistle - Current & Future Scenarios - ", spatialOutputsDatasheet$ModelsID[i], "]")] <- scenarioName
      
    }
  }
}

# Create ST-Sim Run Scenarios --------------------------------------------------

stsimScenarios <- scenario(stsimLib)

futureForecastFolder <- folder(stsimLib)$FolderID[which(folder(stsimLib)$Name == "02 - Future Forecast")]

baseScenarioNames <- stsimScenarios$Name[str_detect(stsimScenarios$Name, "Base Scenario") & stsimScenarios$IsResult == "No"]

runScenarioNames <- NULL
for (i in baseScenarioNames){
  for (j in 1:length(subScenarioNames)){
    
    runScenarioName <- paste("Forecast", str_remove(i, "Base "), names(subScenarioNames)[j])
    runScenario <- scenario(stsimProject, runScenarioName, overwrite = TRUE, folder = futureForecastFolder)
    
    stsimDepends <- c(i,subScenarioNames[[j]])
    
    dependencyScenarioIDs<- scenario(stsimLib)$ScenarioID[which(scenario(stsimLib)$Name %in% stsimDepends)]
    
    dependency(runScenario, dependency = dependencyScenarioIDs)
    
    runScenarioNames <- c(runScenarioNames, runScenarioName)
  }
}

# Run ST-Sim Scenarios ---------------------------------------------------------

runScenarios <- scenario(ssimObject = stsimLib, runScenarioNames)
run(runScenarios)





