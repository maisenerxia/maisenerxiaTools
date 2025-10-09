library(maisenerxiaTools)
library(polars)
library(arrow)
library(dplyr)

df = open_dataset("preciospdbc") %>%
    mutate(ts_cest = with_tz(ts_cest, "Europe/Madrid")) %>%
    filter(ts_cest >= as.POSIXct('2025-10-01', tz = "Europe/Madrid")) |>
    collect()

p <- df |> 
    filter(ts_cest >= as.POSIXct('2025-09-18', tz = "Europe/Madrid")) |>
    ggplot(aes(x = ts_cest, y = precio_es)) +
    geom_bar(stat = "identity", fill = "#23243F", color = "#23243F", alpha = 0.3) +
    scale_y_continuous(labels = function(x) paste0(x, ' €/MWh')) +
    scale_x_datetime(
        date_breaks = "2 hours",
        date_labels = "%H",
        limits = c(
            as.POSIXct('2025-10-01 00:00'),
            as.POSIXct('2025-10-01 23:59')
        ),
        oob = scales::oob_keep
    ) +
    labs(
        title = 'Precio del mercado mayorista diario para el día 1 de octubre',
        caption = 'Fuente: OMIE'
    ) + 
    theme_mais()
save_to_png(p, 'precios_pdbc.png')
