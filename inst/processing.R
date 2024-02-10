library(patchwork)
library(tidyverse)
library(openxlsx)

# Parámetros del mes a procesar -------------------------------------------
year <- 2023
mes <- 'diciembre'
font_size = 15

# Cargando funciones y objetos utilitarios --------------------------------
source(here::here('general_scripts', 'utilitarios.R'), encoding = "UTF-8")
source(here::here('eem/scripts/functions/handling_data.R'), encoding = "UTF-8")
source(here::here('eem/scripts/functions/graficos.R'), encoding = "UTF-8")
source(here::here('eem/scripts/functions/write_excel_outputs.R'), 
       encoding = "UTF-8")

# Rutas para guardar archivos ---------------------------------------------

# Crea el directorio de outputs para el mes
create_month_dirs("eem", year, mes)

base_path <- list.dirs(here::here('eem/outputs', year)) |>
  stringr::str_subset(pattern = paste0(mes, '$'))

graficos_path <- here::here(base_path, 'graficos')

# Actualizando el histórico -----------------------------------------------
month_data <- eem_month_data(year, mes)

update_eem_historic(month_data)

# Archivos Respuestas desagregadas para el TC para internacional
openxlsx::write.xlsx(
  respuestas_desagregadas_tc(),
  here::here(base_path, 'data_tc_desagregada.xlsx')
  )

# Guardando los resultados en excel ---------------------------------------
write_eem_historic(here::here(base_path, "pagina_web"), toweb = TRUE)

write_eem_month_result(
  here::here(base_path, "pagina_web"), s_year = year, s_mes = mes, 
  toweb = TRUE)

write_eem_historic(base_path, toweb = FALSE)

# Guardando resultados en la carpeta principal 
path_outside <- eem_server_path(year = year, mes = mes)
path_outside_web <- here::here(path_outside, "pagina_web")

write_eem_historic(path_outside, toweb = FALSE)

write_eem_historic(path_outside_web, toweb = TRUE)
write_eem_month_result(
  path_outside_web,
  s_year = year,
  s_mes = mes,
  toweb = TRUE
)

openxlsx::write.xlsx(
  respuestas_desagregadas_tc(),
  here::here(path_outside, 'data_tc_desagregada.xlsx')
)

# Creando las visualizaciones ---------------------------------------------

# Boxplot: evolución de las expectativas de fin de año actual 
(plt_boxplot_inflacion_diciembre <- 
   eem_boxplot_diciembre('inflacion', colores$blue, font_size = font_size) +
   scale_y_continuous(labels = ~paste0(scales::comma(., accuracy = 0.1), "%"))
)
(plt_boxplot_tcd_diciembre <- eem_boxplot_diciembre('tcd', colores$green, font_size = font_size) +
    scale_y_continuous(labels = ~paste0(scales::comma(., accuracy = 0.1), "%")))
(plt_boxplot_pib_diciembre <- eem_boxplot_diciembre('pib', colores$red, font_size = font_size) +
    scale_y_continuous(labels = ~paste0(scales::comma(., accuracy = 0.1), "%")))

# Inflación: desviación respecto a la menta
(plt_desviacion_meta <- eem_inflacion_target_desv(start_date = "2019-01-01") +
    ggplot2::labs(y = "Desviación respecto a la meta", x = NULL))

# Anclaje de las expectativas de inflación
(plt_anclaje_diciembre <- plot_eem_inflacion_anclaje("diciembre", 15, label_size = 3.5) + 
    ggplot2::labs(title = 'Fin de año'))
(plt_anclaje_12meses <- plot_eem_inflacion_anclaje("interanual", 15, label_size = 3.5) + 
    ggplot2::labs(title = '12 meses'))
(plt_anclaje_24meses <- plot_eem_inflacion_anclaje("interanual2", 15, label_size = 3.5) +
    ggplot2::labs(title = '24 meses'))

plt_anclaje <- plt_anclaje_diciembre / plt_anclaje_12meses / plt_anclaje_24meses +
  plot_layout(guides = 'collect') & ggplot2::theme(legend.position = 'bottom') + add_margins()

saving_plot(plt_anclaje, graficos_path, 8, 10)

# Density plot: expectativas en los distintos horizontes
(plt_densidad_inflacion <- eem_density_plot('inflacion', facets = TRUE, font_size = font_size))
(plt_densidad_tpm <- eem_density_plot(
  'tpm', facets = FALSE, font_size = font_size, adjust = 3))
(plt_densidad_tc <- eem_density_plot('tc', facets = FALSE, font_size = font_size))
(plt_densidad_tcd <- eem_density_plot('tcd', facets = FALSE, font_size = font_size))

# Libe Ribbon plot: expectativas en los distintos horizontes

# Inflación
(plt_ribbon_inflacion_12meses <-  eem_expectativa_ribbon(
  plot_var = 'inflacion', horizon = '12 meses', 
  color = colores$blue, font_size = font_size))

(plt_ribbon_inflacion_24meses <- eem_expectativa_ribbon(
  plot_var = 'inflacion', horizon = '24 meses', 
  color = colores$blue, font_size = font_size))

(plt_ribbon_tcd_12meses <- eem_expectativa_ribbon(
  plot_var = 'tcd', horizon = '12 meses', 
  color = colores$green, font_size = font_size))

(plt_ribbon_tcd_24meses <- eem_expectativa_ribbon(
  plot_var = 'tcd', horizon = '24 meses', 
  color = colores$green, font_size = font_size))

(plt_ribbon_tc_24meses <- eem_expectativa_ribbon(
  plot_var = 'tc', horizon = '24 meses', 
  color = colores$green, font_size = font_size))

(plt_ribbon_tc_12meses <- eem_expectativa_ribbon(
  plot_var = 'tc', horizon = '12 meses', 
  color = colores$green, font_size = font_size))

(plt_ribbon_pib_diciembre <- eem_expectativa_ribbon(
  plot_var = 'pib', horizon = 'Fin de año',
  color = colores$red, font_size = font_size, 
  start_year = 2021))

(plt_ribbon_pib_diciembre2 <- eem_expectativa_ribbon(
  plot_var = 'pib', horizon = 'Fin año siguiente', 
  color = colores$red, font_size = font_size, start_year = 2021))

(plt_ribbon_tpm_12meses <- eem_expectativa_ribbon(
  plot_var = 'tpm', horizon = '12 meses', 
  font_size = font_size, color = "black"))

(plt_ribbon_tpm_24meses <- eem_expectativa_ribbon(
  plot_var = 'tpm', horizon = '24 meses', 
  color = "black", font_size = font_size))

(plt_ribbon_tpm_diciembre <- eem_expectativa_ribbon(
  plot_var = 'tpm', horizon = 'Fin de año', 
  color = "black", font_size = font_size))

# Jitter plot por horizonte
(plt_jitter_inflacion <- eem_jitter_by_horizonte('inflacion', font_size = font_size))
(plt_jitter_tcd <- eem_jitter_by_horizonte('tcd', font_size = font_size))
(plt_jitter_pib <- eem_jitter_by_horizonte('pib'))
(plt_jitter_tpm <- eem_jitter_by_horizonte('tpm'))


# Gráficos html -----------------------------------------------------------
(plotly_inflacion_diciembre <-  plotly_boxplot_diciembre(
  'inflacion', color = colores$blue))

(plotly_pib_diciembre <- plotly_boxplot_diciembre('pib', color = colores$red))
(plotly_tcd_diciembre <- plotly_boxplot_diciembre('tcd', colores$green))

(plolty_jitter_inflacion <- plotly_jitter_by_horizonte('inflacion'))

(plolty_jitter_tcd <- plotly_jitter_by_horizonte('tcd'))

(plotly_ribbon_inflacion_12m <- plotly_ribbon(
  plot_var = 'inflacion', horizon = '12 meses', 
  color = colores$blue, font_size = font_size))

(plotly_ribbon_inflacion_24m <- plotly_ribbon(
  plot_var = 'inflacion', horizon = '24 meses', 
  color = colores$blue, font_size = font_size))

(plotly_ribbon_tcd_12m <- plotly_ribbon(
  plot_var = 'tcd', horizon = '12 meses', 
  color = colores$green, font_size = font_size))

(plotly_ribbon_tcd_24m <- plotly_ribbon(
  plot_var = 'tcd', horizon = '24 meses', 
  color = colores$green , font_size = font_size))

(plotly_ribbon_tc_12m <- plotly_ribbon(
  plot_var = 'tc', horizon = '12 meses', 
  color = colores$green, font_size = font_size))

(plotly_ribbon_tc_24m <- plotly_ribbon(
  plot_var = 'tc', horizon = '24 meses', 
  color = colores$green, font_size = font_size))

(plotly_ribbon_pib_diciembre <- plotly_ribbon(
  plot_var = 'pib', horizon = 'Fin de año', 
  color = colores$red, start_year = 2021, font_size = font_size))

(plotly_ribbon_pib_diciembre2 <- plotly_ribbon(
  plot_var = 'pib', horizon = 'Fin año siguiente',
  color = colores$red, start_year = 2021, font_size = font_size))

# Guardando workspace del mes ---------------------------------------------
save.image(here::here(base_path, paste('ws', mes, year, sep = '_')))



# Playground --------------------------------------------------------------
perio <- lubridate::make_date(year, crear_mes(mes), "01")

tpm_step_ecdf <- get_eem_historic_long() %>%
  filter(variable == 'tpm', periodo == perio,
         !horizonte %in% c('Fin de mes', 'Fin de trimestre')) %>%
  ggplot(aes(x = value, color = horizonte)) +
  stat_ecdf(geom = "step", size = 1) +
  theme_em() +
  scale_color_manual(values = colores$paleta) +
  labs(x = "Tasa anual", y = "Porcentaje de respuestas", color = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = function(x) paste0(x, '%'))

# Guardando gráficos ------------------------------------------------------

saving_plot(plt_boxplot_inflacion_diciembre, graficos_path, 7, 4)

saving_plot(plt_ribbon_inflacion_12meses, graficos_path, 7, 4)
saving_plot(plt_ribbon_inflacion_24meses, graficos_path, 7, 4)

saving_plot(plt_anclaje_diciembre, graficos_path, 7, 4)
saving_plot(plt_anclaje_12meses, graficos_path, 7, 4)
saving_plot(plt_anclaje_24meses, graficos_path, 7, 4)
saving_plot(plt_anclaje, graficos_path, 7, 10)
saving_plot(plt_densidad_inflacion, graficos_path, 7, 4)
saving_plot(plt_desviacion_meta, graficos_path, 7, 4)


saving_plot(plt_boxplot_tcd_diciembre, graficos_path, 7, 4)
saving_plot(plt_densidad_tc, graficos_path, 7, 4)
saving_plot(plt_densidad_tcd, graficos_path, 7, 4)

saving_plot(plt_boxplot_pib_diciembre, graficos_path, 7, 4)
saving_plot(plt_ribbon_pib_diciembre, graficos_path, 7, 4)


saving_plot(plt_densidad_tpm, graficos_path, 7, 4)
saving_plot(tpm_step_ecdf, graficos_path, 6, 4)
saving_plot(plt_ribbon_tpm_24meses, graficos_path, 7, 4)

saving_plot(plt_jitter_inflacion, graficos_path, 7, 4)
saving_plot(plt_jitter_tcd, graficos_path, 7, 4)
