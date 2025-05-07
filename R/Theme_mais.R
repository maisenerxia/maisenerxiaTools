#' @name theme_mais 
#' @title Tema de Mais Enerxia para ggplot2
#' @description 
#' Tema personalizado para ggplot2 con la identidad visual de Mais Enerxia:
#' tipografia Roboto, fondo limpio y grillas suaves.
#' 
#' @param base_size Tamaño de la fuente en punto. Por defecto es 12.
#' @param base_family Familia de fuentes. Por defecto es "Roboto".
#' @param legend_position Posicion de la leyenda. Por defecto es "none".
#' 
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

theme_mais <- function(base_size    = 12,
                       base_family  = "Roboto",
                       legend_position = "right") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major    = element_line(color = "#f0f0f0", size = .2),
      panel.grid.minor    = element_line(color = "#f0f0f0", size = .1),
      plot.background     = element_rect(fill = "#ffffff", color = NA),
      panel.background    = element_rect(fill = "#ffffff", color = NA),
      plot.title          = ggtext::element_markdown(
                              hjust  = 0,
                              size   = base_size * 1.2,
                              family = base_family,
                              color  = "#000000"
                            ),
      plot.subtitle       = ggtext::element_markdown(
                              hjust  = 0,
                              size   = base_size,
                              family = base_family,
                              color  = "#000000"
                            ),
      plot.caption        = ggtext::element_markdown(
                              size   = base_size * 0.8,
                              hjust  = 0,
                              vjust  = .5,
                              family = base_family,
                              color  = "#777777"
                            ),
      legend.position     = legend_position,
      legend.key          = element_rect(fill = "transparent", colour = "transparent"),
      legend.title        = element_blank(),
      axis.title.x        = element_text(size = base_size, family = base_family),
      axis.title.y        = element_text(size = base_size, family = base_family),
      axis.text.x         = element_text(size = base_size * 0.9, family = base_family),
      axis.text.y         = element_text(size = base_size * 0.9, family = base_family),
      strip.text          = element_text(size = base_size * 0.9, family = base_family),
      strip.background    = element_rect(fill = "transparent", color = NA)
    )
}