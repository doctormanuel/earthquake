#' print("geom_timeline")
#' This is a ggplot2 geom to build a layer to display at timeline plot for
#' hurricane data.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return This function adds a layer to a ggplot2 object
#'
#' @examples
#' \dontrun{
#' ggplot(data = eqdata) +
#' geom_timeline(aes(x = Date, y = Country,
#' size = Mag, color = `Total Deaths`)
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = GeomTimeline, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}

#' print("GeomTimeline")
#' This is the function that actually does the job behind a call to the ggplot2
#' geom_timeline.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid circleGrob
#' @importFrom grid gpar
#' @importFrom scales alpha
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom, required_aes = c("x"),
                                 optional_aes = c("y", "color","size","alpha"),
                                 default_aes = ggplot2::aes(pch = 21,
                                                            size = 0.01,
                                                            colour = "black",
                                                            alpha = 0.4,
                                                            stroke = 1),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord) {
                                         data$size=data$size/max(data$size,na.rm=TRUE)
                                         coords=coord$transform(data,panel_params)
                                         grid::circleGrob(
                                                 coords$x,
                                                 coords$y,
                                                 r = coords$size/25,
                                                 gp = grid::gpar(col = scales::alpha(coords$colour, coords$alpha),
                                                                 fill = scales::alpha(coords$colour, coords$alpha),
                                                                 alpha = coords$alpha,
                                                                 fontsize = coords$fontsize,
                                                                 lwd = coords$stroke))
                                 })


#' print("geom_timeline_label")
#' This is a ggplot2 geom to build a layer to add annotations to the timeline
#' plot created for hurricane data using the geom_timeline.
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return This function adds a layer to a ggplot2 object
#'
#' @examples
#' \dontrun{
#' ggplot(data = eqdata) +
#' geom_timeline(aes(x = Date, y = Country,
#' size = Mag, color = `Total Deaths`) +
#' geom_timeline_label(aes(x = Date, y = Country,
#' label = `Location Name`, mag = Mag, n_max = 5))
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
                geom = GeomTimelineLabel, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}

#' print("GeomTimelineLabel")
#' This is the function that actually does the job behind a call to the ggplot2
#' geom_timeline_label.
#'
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom utils head
#' @importFrom grid segmentsGrob
#' @importFrom grid gpar
#' @importFrom grid textGrob
#' @importFrom grid gTree
#' @importFrom grid gList
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom, required_aes = c("x"),
                                      optional_aes = c("label", "y", "color","mag","alpha", "n_max"),
                                      default_aes = ggplot2::aes(shape = 21,
                                                                 colour = "black",
                                                                 size = 15,
                                                                 n_max = 5,
                                                                 alpha = 0.4,
                                                                 stroke = 2),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_params, coord) {
                                              data=data %>% dplyr::arrange(dplyr::desc(.data$mag))
                                              data=utils::head(data,unique(data$n_max))
                                              coords=coord$transform(data,panel_params)
                                              lines=grid::segmentsGrob(x0=coords$x, y0=coords$y,
                                                                       x1=coords$x, y1=coords$y+0.15,
                                                                       default.units = "npc",
                                                                       gp=grid::gpar(col=coords$colour,
                                                                                     alpha=coords$alpha,
                                                                                     fontsize=coords$size,
                                                                                     lwd=coords$stroke))
                                              texts=grid::textGrob(label=coords$label,
                                                                   x=coords$x,
                                                                   y=coords$y+0.15,
                                                                   just="left",
                                                                   rot=45,
                                                                   check.overlap=TRUE,
                                                                   default.units = "npc",
                                                                   gp=grid::gpar(col=coords$colour,
                                                                                 fontsize=coords$size,
                                                                                 lwd=coords$stroke))
                                              grid::gTree(children=grid::gList(lines,texts))
                                      })
