library(NIcalc)

# RETRIEVE DATA FROM DATABASE #
#-----------------------------#'
indicator <- c("Dikesoldogg")
indicator <- c("Jerv")

myUsername <- "anders.kolstad@nina.no"
myPassword <- ""

indicatorImport <- importDatasetApi(username = myUsername,
                                    password = myPassword,
                                    eco = NULL,
                                    indic = indicator,
                                    year = c("1990","2000","2010","2014","2019"),
                                    norwegian = TRUE,
                                    refYearCode = 0)
#getwd()
#saveRDS(indicatorImport, "data/jerv.rds")


# ASSEMBLE 
# Spesify all of Norway incl the five regions, som NIunits:
myNIunits <- c(allArea = T, parts = T, counties = F)

# Include all BSunits (kommuner):
myPartOfTotal <- 0

assemeble <- NIcalc::assembleNiObject(
  inputData = indicatorImport,
  predefNIunits = myNIunits, 
  partOfTotal = myPartOfTotal, 
  indexType = "thematic",
  part = "ecosystem",
  total = "terrestrial")  


#saveRDS(assemeble, "data/jerv_assemble.rds")    