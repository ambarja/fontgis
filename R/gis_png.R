#' Exporta un ícono GIS a PNG
#'
#' @param name Alias del ícono
#' @param file Ruta de salida (.png)
#' @param width Tamaño en px
#' @param height Tamaño en px
#' @param fill Color del ícono
#' @return Ruta del archivo PNG generado
#' @export
gis_png <- function(name, file = tempfile(fileext = ".png"),
                    width = 100, height = 100, fill = "black") {
  tmp_html <- tempfile(fileext = ".html")
  icon_html <- gis_i(name, width = paste0(width, "px"), height = paste0(height, "px"), fill = fill)

  htmltools::save_html(icon_html, tmp_html)

  if (!requireNamespace("webshot2", quietly = TRUE)) stop("Instala el paquete 'webshot2'")

  webshot2::webshot(url = tmp_html, file = file, vwidth = width, vheight = height, cliprect = "viewport")

  return(normalizePath(file))
}
