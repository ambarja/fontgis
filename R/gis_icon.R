#' Renderiza un icono GIS como SVG en el Viewer (usando alias)
#'
#' @param name Alias amigable del icono (por ejemplo: "north-arrow")
#' @param width Ancho (por ejemplo "100px", "1em")
#' @param height Alto
#' @param fill Color de relleno del icono
#'
#' @return HTML visual en Viewer.
#' @export
gis_icon <- function(name, width = "100px", height = "100px", fill = "black") {
  # Generar alias en tiempo real
  icon_aliases <- generate_icon_aliases()

  # Buscar alias
  alias_match <- icon_aliases[icon_aliases$alias == name, ]

  if (nrow(alias_match) == 0) {
    stop(paste0("Icono '", name, "' no encontrado. Usa `gis_icons()` para ver disponibles."))
  }

  real_name <- alias_match$name[1]
  category <- alias_match$category[1]

  base_dir <- system.file(package = "fontgis")
  svg_path <- file.path(base_dir, "svg", category, paste0(real_name, ".svg"))

  if (!file.exists(svg_path)) stop("Archivo SVG no encontrado")

  svg_lines <- readLines(svg_path, warn = FALSE)
  svg_lines <- svg_lines[!grepl("^<\\?xml|^<!DOCTYPE|<metadata|</metadata|<defs|</defs", svg_lines)]

  svg_str <- paste(svg_lines, collapse = "\n")

  svg_str <- gsub("width=\"[^\"]*\"", "", svg_str)
  svg_str <- gsub("height=\"[^\"]*\"", "", svg_str)
  svg_str <- gsub("fill=\"#[A-Fa-f0-9]+\"", "", svg_str)
  svg_str <- gsub("fill:[^;\\\"]+;?", "", svg_str)

  svg_str <- sub("<svg([^>]*)>", paste0("<svg\\1 width=\"", width, "\" height=\"", height, "\" fill=\"", fill, "\">"), svg_str)

  htmltools::html_print(
    htmltools::browsable(
      htmltools::HTML(svg_str)
    )
  )
}


#' Listar todos los iconos disponibles con alias
#'
#' @return Un data frame con columnas: alias, name, category
#' @export
gis_icons <- function() {
  generate_icon_aliases() |>
    tidyr::as_tibble()
}
