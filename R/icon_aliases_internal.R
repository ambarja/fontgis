#' @noRd
#' @importFrom utils strcapture
generate_icon_aliases <- function() {
  svg_root <- file.path(system.file(package = "fontgis"), "svg")
  svg_paths <- list.files(svg_root, pattern = "\\.svg$", recursive = TRUE, full.names = FALSE)
  info <- strcapture("([^/]+)/([^/]+)\\.svg", svg_paths,
    proto = data.frame(category = character(), name = character())
  )
  # Generar alias limpio
  info$alias <- tolower(gsub("^u[0-9A-Fa-f]+[-]?", "", info$name))
  info[, c("alias", "name", "category")]
}
