# Maps




## Raw data

## Scaled data

### Jerv


#### Prepare NI data

The jerv (wolverine) data was downloaded using the `R/singleIndicator.R` script and the importDatasetApi() function,, and subsequently the assembleNiObject() function, so now I can simply import it.




```r
jerv <- readRDS("data/Jerv_assemebled.rds")
```


This data file contains the raw data in the form of expected values for each BSunits (municipalities). But we actually want to keep the original geometeris of the eight rovviltregioner, and so we need to focus in the ICunits instead.

```r
par(mar=c(9,5,1,1))
barplot(jerv$indicatorValues$`2019`$expectedValue,
        names.arg = jerv$indicatorValues$`2019`$ICunitName, 
        las=2,
        ylab = "Estimated number of\nwolverine in 2019")
```

![](03-maps_files/figure-epub3/unnamed-chunk-3-1.png)<!-- -->

The data also contains upper and lower quantiles, but we can also get the full probability distribution and sample from it to get standard deviations. 
but also as probability functions that we can sample from:

```r
# bruker tradOb siden custumDist er NA. Dette er ikke en generisk løsning. 
obstype <- rep("tradObs", nrow(jerv$indicatorValues$'2019'))

#myYears <- as.character(c(1990,2000,2010,2014,2019))
myYears <- as.character(c(2019))

for(i in 1:length(myYears)){
# print(i)

myMat <- NIcalc::sampleObsMat(
  ICunitId           = jerv$indicatorValues[[i]]$ICunitId, 
  value              = jerv$indicatorValues[[i]]$expectedValue,
  distrib            = jerv$indicatorValues[[i]]$distributionFamilyName,
  mu                 = jerv$indicatorValues[[i]]$distParameter1,
  sig                = jerv$indicatorValues[[i]]$distParameter2,
  customDistribution = jerv$indicatorValues[[i]]$customDistribution,
          obsType = obstype,
          nsim = 1000
          
)
assign(paste0("myMat", myYears[i]), myMat)
}
#> Warning: replacing previous import 'distr::plot' by
#> 'graphics::plot' when loading 'NIcalc'

par(mfrow = c(1,2))
hist(myMat2019[1,], main = "Rovviltregion 1", xlab = "")
hist(myMat2019[8,], main = "Rovviltregion 8", xlab = "")
```

![](03-maps_files/figure-epub3/unnamed-chunk-4-1.png)<!-- -->

For some reason the extected values are far from the mean of these distributions. I did this exercise [once before](https://ninanor.github.io/IBECA/jerv.html), and did not get this problem then. I think the difference is that I use eco = NULL this time, in the `importDatasetApi()`, and this cause the output to somehow split into forest and alpine ecosystems. I will ignore this here for this example.

I can also get the reference values in the same way, and then divide one by the other to get scaled values

```r
myMatr <- NIcalc::sampleObsMat(
            jerv$referenceValues$ICunitId, 
            jerv$referenceValues$expectedValue,
            jerv$referenceValues$distributionFamilyName,
            mu = jerv$referenceValues$distParameter1,
            sig = jerv$referenceValues$distParameter2,
            customDistribution = jerv$referenceValues$customDistribution,
            obsType = obstype,
            nsim =1000
        )

temp <- colSums(myMat2019)/colSums(myMatr)
hist(temp)
```

![](03-maps_files/figure-epub3/unnamed-chunk-5-1.png)<!-- -->

Then I will create a data frame with the mean indicator values and the SD.

```r
library(matrixStats)
#> Warning: package 'matrixStats' was built under R version
#> 4.1.3
#> 
#> Attaching package: 'matrixStats'
#> The following object is masked from 'package:dplyr':
#> 
#>     count
jerv_tbl <- data.frame("raw2019" = round(rowMeans(myMat2019), 2),
                       "sd2019"  = round(matrixStats::rowSds(myMat2019), 2),
                       "ref"     = round(rowMeans(myMatr), 2))
jerv_tbl$scaled <- round(jerv_tbl$raw2019/jerv_tbl$ref, 2)
jerv_tbl$cv <- round(jerv_tbl$sd2019/jerv_tbl$raw2019, 2)
jerv_tbl$region <- jerv$indicatorValues$`2019`$ICunitName
DT::datatable(jerv_tbl)
```

![](03-maps_files/figure-epub3/unnamed-chunk-6-1.png)<!-- -->
This is a special case maybe, because the sd is often larger than the mean.

Btw, we could use inbuilt NIcalc functions to get the indicator value, like I do below, but that will aggregate to regions, and we want to keep the original geometry.

```r
jervComp <- NIcalc::calculateIndex(
  x       = jerv,
  nsim     = 1000,
  awBSunit = "terrestrialArea",
  fids     = F,    # should fidelities be ignored in 
                   # the calculation of Wi?
  tgroups  = F, # should grouping of indicators 
                   # into trophic and key indicator 
                   # groups be ignored
  keys     = "specialWeight", #"ignore",
)
#> Indices for NIunits 'wholeArea', 'E', 'S', 'W', 'C', 'N'
#> and years '1990', '2000', '2010', '2014', '2019' will be calculated.
#> The 30 index distributions will each be based on  1000 simulations.
#> There are 8 ICunits with observations in data set 'jerv'.
#> 
#> Calculating weights that are the same for all years .....
#> 
#> Sampling reference values .....
#> 
#> Sampling and scaling indicator observations from  1990 .....
#> 
#> Sampling and scaling indicator observations from  2000 .....
#> 
#> Sampling and scaling indicator observations from  2010 .....
#> 
#> Sampling and scaling indicator observations from  2014 .....
#> 
#> Sampling and scaling indicator observations from  2019 .....
plot(jervComp$wholeArea)
```

<div class="figure">
<img src="03-maps_files/figure-epub3/unnamed-chunk-7-1.png" alt="The scaled indicator values for wolverine across Norway."  />
<p class="caption">(\#fig:unnamed-chunk-7)The scaled indicator values for wolverine across Norway.</p>
</div>



#### Get geometries

Then I can get the spatial geometries associated with the data. There are the so called rovviltregioner. There are eight of them. They are actually linked to the BS-units (municipalites), but we don't want to plot the outlines of the municipalities.
The geometries for the appropriate spatial units of each indicator can be downloaded in .json format via a previously created API for the nature index database: https://ninweb08.nina.no/NaturindeksAPI/index.html
To get the file for a specific indicator, one needs to enter the numerical indicator id under "/api/Indicator/\{id\}/Areas" and then click download. We then converted the .json file to shapefiles for use in R.  


```r
path <- "P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Pilot_Forbedring_Innsynsløsning/Shapefiles/Jerv"
```


```r
library(sf)
#> Warning: package 'sf' was built under R version 4.1.3
#> Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 7.2.1; sf_use_s2() is TRUE
rov <- sf::read_sf(path)
rov <- sf::st_make_valid(rov)
rov <- rov[rov$area!="DEF jerv",]
```

Clip it against the outline of Norway to make it look more pretty

```r
path <- "data/outlineOfNorway_EPSG25833.shp"
nor <- sf::read_sf(path)
nor <- st_transform(nor, crs=st_crs(rov))
```


```r
rov <- st_intersection(rov, nor)
#> Warning: attribute variables are assumed to be spatially
#> constant throughout all geometries
```




#### Link data and geometries


```r
rov$scaledIndicator <- jerv_tbl$scaled[match(rov$area, jerv_tbl$region)]
rov$cv <- jerv_tbl$cv[match(rov$area, jerv_tbl$region)]
rov$raw <- jerv_tbl$raw2019[match(rov$area, jerv_tbl$region)]
```




```r
library(tmap)
#> Warning: package 'tmap' was built under R version 4.1.3
one <- tm_shape(rov)+
  tm_polygons(col="scaledIndicator", 
              border.col = "white")

two <- tm_shape(rov)+
  tm_polygons(col="cv", 
              border.col = "white")

three <- tm_shape(rov)+
  tm_polygons(col="raw", 
              border.col = "white")


tmap_arrange(one, two, 
             widths = c(.75, .25),
             heights = c(1, 0.5))
#> Legend labels were too wide. The labels have been resized to 0.27, 0.27, 0.27, 0.27, 0.27, 0.27. Increase legend.width (argument of tm_layout) to make the legend wider and therefore the labels larger.
```

![](03-maps_files/figure-epub3/unnamed-chunk-13-1.png)<!-- -->

