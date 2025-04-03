#' Renderiza i tag GIS como SVG en el Viewer (usando alias)
#'
#' @param name Alias amigable del icono (por ejemplo: "north-arrow")
#' @param width Ancho (por ejemplo "100px", "1em")
#' @param height Alto
#' @param fill Color de relleno del icono
#'
#' @return HTML visual en Viewer.
#' @export
gis_i <- function(name, width = "1em", height = "1em", fill = "currentColor") {
  icon_aliases <- generate_icon_aliases()
  alias_match <- icon_aliases[icon_aliases$alias == name, ]
  if (nrow(alias_match) == 0) stop("Icono no encontrado")

  real_name <- alias_match$name[1]
  category  <- alias_match$category[1]

  svg_path <- file.path(system.file(package = "rfontgis"), "svg", category, paste0(real_name, ".svg"))
  if (!file.exists(svg_path)) stop("Archivo SVG no encontrado")

  svg_lines <- readLines(svg_path, warn = FALSE)
  svg_lines <- svg_lines[!grepl("^<\\?xml|<metadata|</metadata|<defs|</defs", svg_lines)]
  svg_str <- paste(svg_lines, collapse = "\n")

  # Limpiar atributos que bloquean estilo personalizado
  svg_str <- gsub("width=\"[^\"]*\"", "", svg_str)
  svg_str <- gsub("height=\"[^\"]*\"", "", svg_str)
  svg_str <- gsub("fill=\"#[A-Fa-f0-9]+\"", "", svg_str)
  svg_str <- gsub("fill:[^;\\\"]+;?", "", svg_str)

  # Si no tiene viewBox, agregar uno estándar
  if (!grepl("viewBox", svg_str)) {
    svg_str <- sub("<svg", '<svg viewBox="0 0 100 100"', svg_str)
  }

  # Inyectar atributos seguros
  svg_str <- sub(
    "<svg([^>]*)>",
    paste0(
      "<svg\\1 width=\"", width,
      "\" height=\"", height,
      "\" fill=\"", fill,
      "\" preserveAspectRatio=\"xMidYMid meet\"",
      " style=\"display:inline-block;vertical-align:middle\">"
    ),
    svg_str
  )

  htmltools::HTML(svg_str)
}
