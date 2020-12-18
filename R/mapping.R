#' print("eq_create_label")
#' This function creates an HTML label as a new column in an earthquake
#' dataframe where Location, Magnitude and Total Deaths are introduced and that
#' can be used for Leaflet map annotation
#'
#' @param data the data frame to add the HTML label to
#'
#' @return This function returns the input dataset with an additional column
#' called popup_text which contains an HTML string that can be used to
#' annotate in Leaflet maps  the Location, Magnitude and Total Deaths of
#' earthquakes.
#'
#' @examples
#' \dontrun{
#' annotateddata <- eq_create_label(data)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @export
eq_create_label <- function(data = rlang::.data) {
        data %>% dplyr::mutate(popup_text=paste(ifelse(!is.na(`Location Name`)&`Location Name`!="",
                                                       paste("<p><strong>Location:</strong> ",
                                                             `Location Name`,"</p>",sep=""),
                                                       ""),
                                                ifelse(!is.na(Mag),
                                                       paste("<p><strong>Magnitude:</strong> ",
                                                             Mag,"</p>",sep=""),
                                                       ""),
                                                ifelse(!is.na(`Total Deaths`),
                                                       paste("<p><strong>Total deaths:</strong> ",
                                                             `Total Deaths`,"</p>",sep=""),
                                                       "")))
}

#' print("eq_map")
#' This function creates an interactive Leaflet map to visualize earthquake
#' locations contained in the input dataframe marking them with a circle whose
#' size is proportional to the earthquake's magnitude. Earthquake locations can
#' be annotated
#'
#' @param data the data frame to use for map creation
#' @param annot_col a string indicating the name of the column used for popup
#' annotation
#'
#' @return This function creates a Leaflet map to visualize the earthquake
#' locations contained in the input dataframe
#'
#' @examples
#' \dontrun{
#' eq_map(eqdata, annot_col="Date")
#' eq_map(eq_create_label(eqdata), annot_col="popup_text")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircles
#'
#' @export
eq_map <- function(data, annot_col){
        data %>% dplyr::filter(!is.na(Latitude)&!is.na(Longitude)) %>%
                leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addCircles(lat=~Latitude, lng=~Longitude,
                                    weight=1,radius=~Mag*10000, popup=~eval(parse(text=annot_col)))

}
