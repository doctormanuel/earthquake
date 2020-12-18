#' print("eq_clean_data")
#' This function applies some cleaning processes to a dataset downloaded from
#' the \href{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search}{NOAA Significant Earthquake Database}.
#' Specifically, it converts the Latitude and Longitude columns to numeric class
#' and combines year, month and day into a single variable in the Date class.
#'
#' @param data the raw data frame to clean
#'
#' @return This function returns a dataset where Longitude and Latitude are
#' numeric variables and there is a new Date column containing the date. The old
#' year, month and day columns are dropped.
#'
#' @examples
#' \dontrun{
#' cleandata <- eq_clean_data(rawdata)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate ymd
#' @importFrom lubridate years
#' @importFrom stringr str_pad
#'
#' @export
eq_clean_data <- function(data){
        data %>% dplyr::mutate(Date=lubridate::ymd(paste(stringr::str_pad(ifelse(Year>0,as.character(Year),"0"),4,"0",side="left"),
                                                         stringr::str_pad(ifelse(is.na(Mo),"1",as.character(Mo)),2,"0",side="left"),
                                                         stringr::str_pad(ifelse(is.na(Dy),"1",as.character(Dy)),2,"0",side="left"),sep="-"))
                               +lubridate::years(ifelse(Year<0,Year,0)),.before=Year) %>%
                dplyr::select(-c(Year,Mo,Dy)) %>%
                dplyr::mutate(Latitude=as.numeric(Latitude,Longitude=as.numeric(Longitude)))
}

#' print("eq_location_clean")
#' This function applies some cleaning processes to the Location Name column in
#' a dataset downloaded from the \href{https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/search}{NOAA Significant Earthquake Database}.
#' Specifically, it drops any location beyond that or those of the first
#' occurring country, it drops the preceding name of country and region, and
#' converts the resulting location name to title case.
#'
#' @param data the raw data frame to clean
#'
#' @return This function returns a dataset where the Location Name column has
#' been cleaned, by removing location in more that one country, removing the
#' country name and converting the name to title case. The country name, where
#' available is also converted to title case and assigned to a separate column.
#'
#' @examples
#' \dontrun{
#' cleandata <- eq_location_clean(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom stringr str_locate
#' @importFrom stringr str_to_title
#' @importFrom stringr str_trim
#' @importFrom stringi stri_reverse
#'
#' @export
eq_location_clean <- function(data){
        data  %>% dplyr::mutate(`Location Name`=ifelse(stringr::str_detect(`Location Name`,";"),
                                                       stringr::str_sub(`Location Name`,
                                                                        end=stringr::str_locate(`Location Name`,
                                                                                                ";")-1),
                                                       `Location Name`)) %>%
                dplyr::mutate(Country=ifelse(stringr::str_detect(`Location Name`,":"),
                                             stringr::str_sub(`Location Name`,
                                                              end=stringr::str_locate(`Location Name`,
                                                                                      ":")-1),
                                             NA)) %>%
                dplyr::mutate(`Location Name`=stringi::stri_reverse(`Location Name`)) %>%
                dplyr::mutate(`Location Name`=ifelse(stringr::str_detect(`Location Name`,":"),
                                                     stringr::str_sub(`Location Name`,
                                                                      end=stringr::str_locate(`Location Name`,
                                                                                              ":")-1),
                                                     `Location Name`)) %>%
                dplyr::mutate(`Location Name`=stringi::stri_reverse(`Location Name`)) %>%
                dplyr::mutate(Country=stringr::str_to_title(Country),
                              `Location Name`=stringr::str_to_title(stringr::str_trim(`Location Name`,side="both")))
}
