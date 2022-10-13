library(gamlss)
library(msm)
library(NIcalc)
library(plyr)
library(mice)
library(RJSONIO)
library(tibble)
library(distr)
library(lattice)

path <- getwd()
op <- options("echo", "warn")
options(echo = FALSE, warn = -1)
options(op)


## Retrieve data from NI database
openLowlandImport <- importDatasetApi(username = "[...]",
                                         password = "[...]",
                                         eco = "Åpent lavland",
                                         indic = NULL,
                                         year = c("1990","2000","2010","2014","2019"),
                                         norwegian = TRUE,
                                         refYearCode = 0)

#saveRDS(openLowlandImport,file = "openLowlandImport.rds")

## Assemble NI object
openLowlandInput <- assembleNiObject(inputData = openLowlandImport,
                                     indicators = NULL,
                                     referenceValues = NULL,
                                     indicatorObservations = NULL,
                                     ICunits = NULL,
                                     BSunits = NULL,
                                     ecosystems = NULL,
                                     NIunits = NULL,
                                     predefNIunits = c(allArea = T, parts = T, counties = F),
                                     indexType = "ecosystem",
                                     part = "ecosystem",
                                     total = "total",
                                     partOfTotal = 0)


## Combine generalists and specialists into one group
xxx <- yyy <- openLowlandInput$indicators$functionalGroup
xxxId <- yyyId <- openLowlandInput$indicators$functionalGroupId

yyy[xxxId %in% c(1,2)] <- "Mellompredator"
yyyId[xxxId %in% c(1,2)] <- 1
yyy[xxxId %in% c(6,7)] <- "Primærprodusent"
yyyId[xxxId %in% c(6,7)] <- 6
yyy[xxxId %in% c(8,9)] <- "Topp-predator"
yyyId[xxxId %in% c(8,9)] <- 8

openLowlandInput$indicators$functionalGroup <- yyy
openLowlandInput$indicators$functionalGroupId <- yyyId

#saveRDS(openLowlandInput,file = "openLowlandInput.rds")

## Impute diagnostics
openLowlandDiagnostics <- imputeDiagnostics(x = openLowlandInput,
                                            nSim = 10,
                                            transConst = 0.01,
                                            maxit = 20)

# Multiple imputations:
# m = 10 imputations for each of 65 missing indicator observations ..... 
openLowlandDiagnostics$diagnostics$convergencePlot

#saveRDS(openLowlandDiagnostics, file ="openLowlandDiagnostics.rds")


## Impute missing data
openLowlandImputes <- imputeData(x = openLowlandInput,
                                 nSim = 100, # Changed from 1000 to 100
                                 transConst = 0.01,
                                 maxit = 20,
                                 printFlag = TRUE)


#saveRDS(openLowlandImputes,file ="openLowlandImputes.rds")


## Calculate index values
openLowlandIndex <- calculateIndex(x = openLowlandInput,
                                      imputations = openLowlandImputes,
                                      awBSunit = "Åpent lavland",
                                      nsim = 100, # Changed from 1000 to 100
                                      fids = FALSE,
                                      tgroups = TRUE,
                                      keys = "specialWeight",
                                      w = 0.5)

#saveRDS(openLowlandIndex, file = "openLowlandIndex.rds")

