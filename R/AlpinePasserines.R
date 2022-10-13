library(NIcalc)

## Retrieve data from NI database
passerinesImport <- importDatasetApi(username = "chloe.nater@nina.no",
                                     password = "NIaction25!",
                                     eco = NULL,
                                     indic = c("Blåstrupe","Fjellerke","Heipiplerke",
                                               "Lappspurv","Ringtrost","Snøspurv","Steinskvett"),
                                     year = c("1990","2000","2010","2014","2019"),
                                     norwegian = TRUE,
                                     refYearCode = 0)

#saveRDS(passerinesImport,file = "passerinesImport.rds")

## Re-define NI units
BSunitId <- passerinesImport$BSunits$id
NIunitName <- rep("x", length(BSunitId))
NIunitName[BSunitId < 1800] <- "SÃ¸r-Norge"
NIunitName[BSunitId >= 1800] <- "Nord-Norge"

NIunitsPasserines <- data.frame(NIunitName = NIunitName, BSunitId = BSunitId)

## Assemble NI object
passerinesInput <- assembleNiObject(inputData = passerinesImport,
                                    indicators = NULL,
                                    referenceValues = NULL,
                                    indicatorObservations = NULL,
                                    ICunits = NULL,
                                    BSunits = NULL,
                                    ecosystems = NULL,
                                    NIunits = NIunitsPasserines,
                                    predefNIunits = c(allArea = T, parts = F, counties = F),
                                    indexType = "thematic",
                                    part = "ecosystem", 
                                    total = "terrestrial", 
                                    partOfTotal = 0.2)


#saveRDS(passerinesInput,file = "passerinesInput.rds")

## Impute diagnostics
passerinesDiagnostics <- imputeDiagnostics(x = passerinesInput,
                                           nSim = 10,
                                           transConst = 0.01,
                                           maxit = 50)

passerinesDiagnostics$diagnostics

#saveRDS(passerinesDiagnostics, file ="passerinesDiagnostics.rds")

## Impute missing data
passerinesImputes <- imputeData(x = passerinesInput,
                                nSim = 100, # Changed from 1000 to 100
                                transConst = 0.01,
                                maxit = 50,
                                printFlag = TRUE)

#saveRDS(passerinesImputes,file = "passerinesImputes.rds")


## Calculate index values
passerinesIndex <- calculateIndex(x = passerinesInput,
                                  imputations = passerinesImputes,
                                  nsim = 100, # Changed from 1000 to 100
                                  fids = FALSE,
                                  tgroups = FALSE,
                                  keys = "ignore",
                                  w = 0,
                                  awbs = TRUE,
                                  awBSunit = "Fjell")

#saveRDS(passerinesIndex, file = "passerinesIndex.rds")

## Basic plots
#passerinesIndex <- readRDS("passerinesIndex.rds")
plot(passerinesIndex$wholeArea)
summary(passerinesIndex$wholeArea)
plotWeights(passerinesIndex$wholeArea$'1990')
plotWeights(passerinesIndex$wholeArea$'2019', group = "troph")
plotWeights(passerinesIndex$wholeArea$'1990', keys = T, keycol = 2)
summaryWeights(passerinesIndex$wholeArea)

summary(passerinesIndex$wholeArea)
passerinesIndex$wholeArea$`1990`$metadata

