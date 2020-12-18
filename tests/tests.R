library(testthat)
library(magrittr)
library(readr)
library(EarthquakeVisualization)

testthat::test_that("eq_clean_data",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        cleandata=eq_clean_data(data)
        testthat::expect_that(dim(cleandata)[1], testthat::equals(6213))
        testthat::expect_that(dim(cleandata)[2], testthat::equals(37))
        testthat::expect_that(class(cleandata$Latitude),testthat::equals("numeric"))
        testthat::expect_that(class(cleandata$Longitude),testthat::equals("numeric"))
        testthat::expect_that(class(cleandata$Date),testthat::equals("Date"))
})

testthat::test_that("eq_location_clean",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        cleandata=eq_location_clean(eq_clean_data(data))
        testthat::expect_that(stringr::str_to_title(cleandata$`Location Name`), testthat::equals(cleandata$`Location Name`))
        testthat::expect_that("Country" %in% colnames(cleandata), testthat::equals(TRUE))
})

# This function is not exported
#testthat::test_that("GeomTimeline",{
#        testthat::expect_is(GeomTimeline,"Geom")
#        testthat::expect_is(GeomTimeline,"ggproto")
#        testthat::expect_is(GeomTimeline,"gg")
#})

testthat::test_that("geom_timeline",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        plot = data %>% eq_clean_data() %>% eq_location_clean() %>%
                dplyr::filter(Country %in% c("Philippines","Indonesia")) %>%
                ggplot2::ggplot() +
                geom_timeline(ggplot2::aes(x=Date, y=Country, size = Mag, color = `Total Deaths`))
        testthat::expect_is(plot,"ggplot")
})

# This function is not exported
#testthat::test_that("GeomTimelineLabel",{
#        testthat::expect_is(GeomTimeline,"Geom")
#        testthat::expect_is(GeomTimeline,"ggproto")
#        testthat::expect_is(GeomTimeline,"gg")
#})

testthat::test_that("geom_timeline_label",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        plot = data %>% eq_clean_data() %>% eq_location_clean() %>%
                dplyr::filter(Country %in% c("Greece","Turkey")) %>%
                ggplot2::ggplot() +
                geom_timeline(ggplot2::aes(x=Date, y=Country, size = Mag, color = `Total Deaths`)) +
                geom_timeline_label(ggplot2::aes(x=Date,label=`Location Name`,y=Country,mag=Mag,n_max=4))
        testthat::expect_is(plot,"ggplot")
})

testthat::test_that("eq_create_label",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        annotateddata=eq_create_label(eq_location_clean(eq_clean_data(data)))
        testthat::expect_that("popup_text" %in% colnames(annotateddata), testthat::equals(TRUE))
        testthat::expect_that(class(annotateddata$popup_text),testthat::equals("character"))
})

testthat::test_that("eq_map",{
        filename="earthquakes.tsv"
        data=readr::read_delim(filename,delim="\t")
        data=data[-1,]
        data=data %>% eq_clean_data() %>% eq_location_clean() %>%
                eq_create_label()  %>% dplyr::filter(Country %in% c("Greece"))
        map1=data %>% eq_map(annot_col="Date")
        map2=data %>% eq_map(annot_col="popup_text")
        testthat::expect_that(class(map1)[1],testthat::equals("leaflet"))
        testthat::expect_that(class(map2)[1],testthat::equals("leaflet"))
})

