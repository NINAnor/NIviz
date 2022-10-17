# Other figures

## Wordcloud figures

```r

library(wordcloud)
library(NIcalc)
library(wordcloud2)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(tm)
```

The following wordcloud figures show the pressure factors for each indicator, where both the color saturation and text size represent the importance of each pressure factor. Small text size and low saturation means low importance, while large text size represent pressure factors with high importance to the indicator.  

```r

# Load dataset
pressure = read_excel("P:/41201612_naturindeks_2021_2023_database_og_innsynslosning/Pilot_Forbedring_Innsynsløsning/R/Indikator_paavirkning.xlsx")

# Filter for species of interest
pressure = pressure %>% filter(navn_norsk == "Elg" | navn_norsk == "Dikesoldogg" | navn_norsk == "Havørn" | navn_norsk == "Lange" | navn_norsk == "Jerv" | navn_norsk == "Lomvi") # NB - Jerv was not included in the provided excel file
pressure = arrange(pressure, by = navn_norsk)
pressure = pressure %>% rename(PressureFactor = Paavirkningsfaktor, PressureValue = FK_PaavirkningsverdiID)

# Remove all instances of "Ikke rel/ukjent" category" and increase the value of pressure factors for better contrast in wordcloud figures
pressure = pressure %>% filter(PressureValue != 7) %>% mutate(PressureValue = PressureValue*2) 

# Create separate datasets for the different species
dikesoldogg = pressure %>% filter(navn_norsk == "Dikesoldogg") %>% dplyr::select(PressureFactor, PressureValue)

elg = pressure %>% filter(navn_norsk == "Elg") %>% dplyr::select(PressureFactor, PressureValue)

havørn = pressure %>% filter(navn_norsk == "Havørn") %>% dplyr::select(PressureFactor, PressureValue)

lange = pressure %>% filter(navn_norsk == "Lange") %>% dplyr::select(PressureFactor, PressureValue)

lomvi = pressure %>% filter(navn_norsk == "Lomvi") %>% dplyr::select(PressureFactor, PressureValue)

# Create a custom color gradient (green in this case)
pressureColors = c("#bde4aa", "#addb9d", "#9cd28f", "#8cc982", "#7dc275", "#6cb967", "#5db15a", "#50a54e", "#479845", "#3f8c3b", "#367e31", "#2e7228", "#25661f") 
```

## The most important pressure factors for dikesoldogg (oblong-leaved sundew)

```r

# Dikesoldogg
wordcloud(words = dikesoldogg$PressureFactor, freq = dikesoldogg$PressureValue, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = pressureColors, scale = c(1.5, 0.5)) # use rot.per to set the percentage of words that are at a 90 degree angle
```

![](04-other_figures_files/figure-epub3/Wordcloud - Dikesoldogg-1.png)<!-- -->

## The most important pressure factors for elg (Moose)

```r

wordcloud(words = elg$PressureFactor, freq = elg$PressureValue, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = pressureColors, scale = c(1.5, 0.5))
```

![](04-other_figures_files/figure-epub3/Wordcloud - Elg-1.png)<!-- -->

## The most important pressure factors for havørn (White-tailed eagle)

```r

wordcloud(words = havørn$PressureFactor, freq = havørn$PressureValue, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = pressureColors, scale = c(1.5, 0.5))
```

![](04-other_figures_files/figure-epub3/Wordcloud - Havørn-1.png)<!-- -->


## The most important pressure factors for Lange (Common ling) 

```r

wordcloud(words = lange$PressureFactor, freq = lange$PressureValue, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = pressureColors, scale = c(1.5, 0.5))
```

![](04-other_figures_files/figure-epub3/Wordcloud - Lange-1.png)<!-- -->


## The most important pressure factors for Lomvi (common guillemot)

```r

wordcloud(words = lomvi$PressureFactor, freq = lomvi$PressureValue, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = pressureColors, scale = c(1.5, 0.5))
```

![](04-other_figures_files/figure-epub3/Wordcloud - Lomvi-1.png)<!-- -->

## Data type

```r
source("R/colorPalettes.R")
```


```r
#Elg datatyper
data <- data.frame(
  category=c("Ekspert", 
             "Modeller", 
             "Overvåkning"),
  count=c(4.9, 95.1, 0)
)

# load library
library(tidyverse)
#> -- Attaching packages ------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v stringr 1.4.0
#> v tidyr   1.2.0     v forcats 0.5.1
#> v readr   2.1.2
#> -- Conflicts ---------------------- tidyverse_conflicts() --
#> x ggplot2::annotate() masks NLP::annotate()
#> x dplyr::filter()     masks stats::filter()
#> x dplyr::lag()        masks stats::lag()
# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2
data$labelPosition[3]<-0.8
# Compute a good label
data$label <- paste0(data$category, "\n ", data$count, " %")
library(ggrepel)
#> Warning: package 'ggrepel' was built under R version 4.1.3
# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text(x=2, aes(y=labelPosition, label=label, color=category), size=2.5) + # x here controls label position (inner / outer)
  scale_fill_NIviz_d("IndMap_cols") +
  scale_colour_NIviz_d("IndMap_cols") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
```

![](04-other_figures_files/figure-epub3/unnamed-chunk-2-1.png)<!-- -->


