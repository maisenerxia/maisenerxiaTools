#' @name palettes_maisenerxia
#' @title Paletas de colores de Mais Enerxia
#' @description
#' Lista de paletas de colores empleadas en visualizaciones de Mais Energía.
#' Incluye:
#' - Colores corporativos (primary, secondary, neutral)
#' - Paleta semáforo (y su versión invertida)
#' - Paletas discretas de distintos tamaños
#' - Definiciones de paletas continuas (gradientes)
#'
#' @return Lista de paletas, cada una un vector de hex.
#' @export
palettes_maisenerxia <- list(
  # Colores corporativos
  brand_primary    = c("#00A47D"),                # verde Mais Enerxia
  brand_secondary  = c("#F2A900"),                # amarillo sol
  brand_tertiary   = c("#003865"),                # azul oscuro
  brand_neutral    = c("#F5F5F5", "#E0E0E0", "#BDBDBD", "#4A4A4A"),

  # Semáforo
  semaforo         = c("#00A47D", "#F2A900", "#E05656"),
  semaforo_inverted= c("#E05656", "#F2A900", "#00A47D"),

  # Discretas
  discrete2        = c("#00A47D", "#003865"),
  discrete3        = c("#00A47D", "#F2A900", "#003865"),
  discrete4        = c("#00A47D", "#F2A900", "#003865", "#4A4A4A"),
  discrete5        = c("#00A47D", "#F2A900", "#003865", "#4A4A4A", "#E05656")
)

#' @export
palette_maisenerxia <- function(name, n = NULL) {
  pal <- palettes_maisenerxia[[name]]
  if (is.null(pal)) {
    stop("No existe la paleta '", name, "'. Usa names(palettes_maisenerxia).", call. = FALSE)
  }
  if (is.null(n)) n <- length(pal)
  if (n > length(pal)) {
    stop("La paleta '", name, "' tiene solo ", length(pal), " colores.", call. = FALSE)
  }
  out <- pal[1:n]
  structure(out, class = "palette", name = name)
}

# Exportación de paletas concretas
#' @export
palette_brand_primary    <- palette_maisenerxia("brand_primary")
#' @export
palette_brand_secondary  <- palette_maisenerxia("brand_secondary")
#' @export
palette_brand_tertiary   <- palette_maisenerxia("brand_tertiary")
#' @export
palette_brand_neutral    <- palette_maisenerxia("brand_neutral")
#' @export
palette_semaforo         <- palette_maisenerxia("semaforo")
#' @export
palette_semaforo_inv     <- palette_maisenerxia("semaforo_inverted")
#' @export
palette_discrete2        <- palette_maisenerxia("discrete2")
#' @export
palette_discrete3        <- palette_maisenerxia("discrete3")
#' @export
palette_discrete4        <- palette_maisenerxia("discrete4")
#' @export
palette_discrete5        <- palette_maisenerxia("discrete5")

# Definición de gradientes base
gradient_palettes_maisenerxia <- list(
  green_to_yellow = c("#00A47D", "#F2A900"),
  yellow_to_red   = c("#F2A900", "#E05656"),
  full_spectrum   = c("#00A47D", "#F2A900", "#E05656")
)

#' @export
gradient_green_yellow <- grDevices::colorRampPalette(gradient_palettes_maisenerxia$green_to_yellow)
#' @export
gradient_yellow_red   <- grDevices::colorRampPalette(gradient_palettes_maisenerxia$yellow_to_red)
#' @export
gradient_full        <- grDevices::colorRampPalette(gradient_palettes_maisenerxia$full_spectrum)

# Crear versiones de 3 a 20 colores de cada gradiente
for (i in 3:20) {
  assign(paste0("gradient_green_yellow_", i),
         gradient_green_yellow(i),
         envir = .GlobalEnv)
  assign(paste0("gradient_yellow_red_", i),
         gradient_yellow_red(i),
         envir = .GlobalEnv)
  assign(paste0("gradient_full_", i),
         gradient_full(i),
         envir = .GlobalEnv)
}

#' @export
print_palette <- function(x, ...) {
  n <- length(x)
  op <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(op))
  image(1:n, 1, as.matrix(1:n),
        col = x, ylab = "", xaxt = "n", yaxt = "n", bty = "n", ...)
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "sans")
}
