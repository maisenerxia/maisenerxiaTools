#' @name theme_mais 
#' @title Tema de Mais Enerxia para ggplot2
#' @description 
#' Tema personalizado para ggplot2 con la identidad visual de Mais Enerxia:
#' tipografia Roboto, fondo limpio y grillas suaves.
#' 
#' @return Un objeto `ggplot2::theme` listo para aplicar. 
#' @import ggplot2
#' @importFrom ggtext element_markdown
#' @export 
#' 
#' @examples 
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, color = factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_mais(base_size = 12, legend_position = "bottom")

theme_mais <- function(){
  theme(
    #text elements
    panel.grid.major = element_line(color = "#f0f0f0", size=.2),
    panel.grid.minor = element_line(color = "#f0f0f0", size=.1),
    plot.background = element_rect(color="#ffffff", fill="#ffffff"),
    panel.background = element_rect(color="#ffffff", fill="#ffffff"),
    plot.title = element_markdown(hjust = 0, size = 13,
                              family="Oddlini",
                              color = "#000000"),
    plot.subtitle = element_markdown(hjust = 0, size = 12,
                                  family="Oddlini",
                                  color = "#000000"),
    plot.caption = element_markdown(size = 9,
                                hjust = 0, 
                                vjust = .5,
                                family="Oddlini",
                                color = "#777777"),
    legend.position="none",
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10,
                               family="Oddlini Light"),
    axis.text.y = element_text(size = 10,
                               color = "grey27",
                               family="Oddlini Light"),
    strip.text = element_text(family = "Oddlini Light", size = 10),
    strip.background = element_rect(fill = "transparent")
  )
}