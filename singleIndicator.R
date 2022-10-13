library(NIcalc)

# RETRIEVE DATA FROM DATABASE #
#-----------------------------#'
indicator <- c("Dikesoldogg")

indicatorImport <- importDatasetApi(username = "chloe.nater@nina.no",
                                    password = "NIaction25!",
                                    eco = NULL,
                                    indic = indicator,
                                    year = c("1990","2000","2010","2014","2019"),
                                    norwegian = TRUE,
                                    refYearCode = 0)
