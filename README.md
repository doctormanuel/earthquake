
# EarthquakeVisualization

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/doctormanuel/earthquake.svg?branch=master)](https://travis-ci.com/doctormanuel/earthquake)
<!-- badges: end -->

The goal of EarthquakeVisualization is to provide a set of functions to clean
    and process data from the NOAA Significant Earthquake Database, and
    visualize it by using ad hoc ggplot2 geoms and Leaflet maps.

## Installation

You can install the released version of EarquakeVisualization from [GitHub](https://github.com/doctormanuel/earthquake) with:

``` r
library(devtools)
install.packages("doctormanuel/earthquake")
```

## Examples


The following example will create a timeline plot of the earthquakes detected
in Japan an Indonesia since 2000, adding labels indicating the Location Name
for the five strongest earthquakes.

``` r
library(EarthquakeVisualization)
library(magrittr)

filename="earthquakes.tsv"

data=readr::read_delim(filename,delim="\t")
data=data[-1,]

data %>% eq_clean_data() %>% eq_location_clean() %>%
         dplyr::filter(Country %in% c("Japan","Indonesia") & lubridate::year(Date) >= 2000 ) %>%
         dplyr::select(Date, Country, Mag, `Total Deaths`, `Location Name`)  %>%
         ggplot2::ggplot() +
         geom_timeline(aes(x=Date, y=Country, size = Mag, color = `Total Deaths`))+
         geom_timeline_label(aes(x=Date,label=`Location Name`,y=Country,mag=Mag,n_max=5))


data3 %>% dplyr::filter(Country=="Mexico" & lubridate::year(Date) >= 2000) %>%
        eq_create_label() %>% eq_map(annot_col="popup_text")

```

The following example will create a Leaflet map with plotted earthquakes
detected in Mexico since 2000 and including a popup for each of them with its
basic characteristics.

``` r
library(EarthquakeVisualization)
library(magrittr)

filename="earthquakes.tsv"

data=readr::read_delim(filename,delim="\t")
data=data[-1,]

data %>% eq_clean_data() %>% eq_location_clean() %>%
         dplyr::filter(Country=="Mexico" & lubridate::year(Date) >= 2000) %>%
         eq_create_label() %>% eq_map(annot_col="popup_text")
```
