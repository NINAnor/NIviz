# Downloading and preparing example data 


```r
if(!require(NIcalc)){
  devtools::install_github("NINAnor/NIcalc", build_vignettes = F)
}
library(NIcalc)
```

Fill in your username (NINA email) and password.

```r

myUser <- "user@nina.no" # insert NINA email
myPwd  <- "" # secret password

```

Choose which indicator(s) you want, use the NIcalc "importDatasetApi" function to retrieve data from the database and save the dataset locally.


```r
indicator <- c("Dikesoldogg",
               "Jerv",
               "Elg",
               "Lomvi",
               "Havørn",
               "Lange")
```


```r
for(i in indicator){
indicatorImport <- NULL
indicatorImport <- NIcalc::importDatasetApi(
  username = myUser,
  password = myPwd,
  indic = i,
  year = c("1990","2000","2010","2014","2019"))

assign(paste0(i, "_import"), indicatorImport)
}

```


```r
path <- "P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/temp/"

for(i in indicator){
  temp <- get(paste0(i, "_import"))
  saveRDS(temp, paste0(path, i, "_import.rds"))
}

for(i in indicator){
  temp <- paste0(path, i, "_import.rds")
  assign(i, readRDS(temp))
}
```

Next, I need to assemble the data set. This shouldn't be necessary since all the data is already present. But One thing I notices was that for jerv, the distribution familiy and parameters only appear after assembling.


```r
# Spesify all of Norway incl the five regions, som NIunits:
myNIunits <- c(allArea = T, parts = T, counties = F)
# Include all BSunits (kommuner) irrespective of the proportion of the main ecosystems:
myPartOfTotal <- 0

for(i in indicator){

  temp <- get(paste0(i, "_import"))
  assemeble <- NULL
  assemeble <- NIcalc::assembleNiObject(
    inputData = temp,
    predefNIunits = myNIunits, 
    partOfTotal = myPartOfTotal, 
    indexType = "thematic",
    part = "ecosystem",
    total = "total")  
  
  # I dont se the output changing if I for example chose total = marine. Perhaps 'part' and 'total' only becomes an issue if partOfTotal != 0.
  
  assign(paste0(i, "_assemble"), assemeble)

}

```


Save the files 

```r

for(i in indicator){
  temp <- get(paste0(i, "_assemble"))
  saveRDS(temp, paste0("data/", i, "_assemebled.rds"))
}

```




Loading the datafiles back into R.

```r
for(i in indicator){
  temp <- paste0("data/", i, "_assemebled.rds")
  assign(i, readRDS(temp))
}
```



```r
myYears <- as.character(c(1990,2000,2010,2014,2019))


for(j in indicator){
print(j)

  temp <- get(j)
  temp2 <- get(paste0(j, "_import"))
  temp_comb <- data.frame(NULL)
  myMat2 <- NULL
  myMat2_comb <- NULL
  obstype <- NULL
  
  
  obstype <- temp$referenceValues$distributionFamilyName
  obstype[!is.na(obstype)] <- "tradObs"
  obstype[is.na(obstype)]  <- "customObs"
  
myMatr <- NIcalc::sampleObsMat(
  ICunitId           = temp$referenceValues$ICunitId, 
  value              = temp$referenceValues$expectedValue,
  distrib            = temp$referenceValues$distributionFamilyName,
  mu                 = temp$referenceValues$distParameter1,
  sig                = temp$referenceValues$distParameter2,
  customDistribution = temp$referenceValues$customDistribution,
  obsType            = obstype,
  nsim =1000
        )  
  
myMatr <- as.data.frame(myMatr)
myMatr <- myMatr %>%
  tibble::add_column(.before=1,
    ICunitID = row.names(myMatr))

myMatr <- myMatr %>%
  tibble::add_column(.after = 1,
      year = NA) 
  
for(i in 1:length(myYears)){
print(i)

obs <- NULL
  obs <- temp$indicatorValues[[i]]$distributionFamilyName
  obs[!is.na(obs)] <- "tradObs"
  obs[is.na(obs)]  <- "customObs"


myMat <- NIcalc::sampleObsMat(
  ICunitId           = temp$indicatorValues[[i]]$ICunitId, 
  value              = temp$indicatorValues[[i]]$expectedValue,
  distrib            = temp$indicatorValues[[i]]$distributionFamilyName,
  mu                 = temp$indicatorValues[[i]]$distParameter1,
  sig                = temp$indicatorValues[[i]]$distParameter2,
  customDistribution = temp$indicatorValues[[i]]$customDistribution,
  obsType            = obs,
  nsim               = 1000
          
)


myMat2 <- as.data.frame(myMat)

myMat2 <- myMat2 %>%
  tibble::add_column(.before=1,
    ICunitID = row.names(myMat))

myMat2 <- myMat2 %>%
  tibble::add_column(.after = 1,
    year = myYears[i]) 

myMat2_comb <- rbind(myMat2_comb, myMat2)

 }

comb <- rbind(myMatr, myMat2_comb)

comb <- comb %>%
  tibble::add_column(.after = 1,
    ICunitName = temp2$ICunits$name[match(
      comb$ICunitID, temp2$ICunits$id)])

comb2 <- comb[!is.na(comb$year),]
comb3 <- comb[is.na(comb$year),]
comb3$ref_mean <- rowMeans(comb3[,-c(1:3)])

combScaled <- comb2 %>%
  tidyr::pivot_longer(cols = starts_with("V")) 

combScaled <- combScaled %>%
  tibble::add_column(ref = comb3$ref_mean[
    match(combScaled$ICunitID, comb3$ICunitID)])
combScaled$scaledIndicator <- combScaled$value/combScaled$ref
combScaled <- dplyr::select(combScaled,
                     -name,
                     -value,
                     -ref)

assign(paste0(j, "_bootstrapped_raw"), comb)
assign(paste0(j, "_bootstrapped_scaled"), combScaled)

}

```

The reference values are also bootstrapped with uncertainties. These are coded as year = NA. We might, however, just end up using the row means.



Save the files 

```r

for(i in indicator){
  temp <- get(paste0(i, "_bootstrapped_raw"))
    temp <- get(paste0(i, "_bootstrapped_scaled"))

  saveRDS(temp, paste0("data/", i, "_bootstrapped_raw.rds"))
    saveRDS(temp, paste0("data/", i, "_bootstrapped_scaled.rds"))

}

```
