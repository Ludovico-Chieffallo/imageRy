#' Generate Ridgeline Plots from Satellite Raster Data
#'
#' This function generates ridgeline plots from stacked satellite imagery data.
#'
#' @param im A `SpatRaster` object representing the raster data to be visualized.
#' @param scale A numeric value that defines the vertical scale of the ridgeline plot.
#' @param palette A character string specifying the `viridis` color palette option to use.
#' Available options: `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`, `"mako"`, `"rocket"`, `"turbo"`.
#'
#' @return A `ggplot` object displaying the ridgeline plot.
#'
#' @details
#' Ridgeline plots are useful for analyzing temporal variations in raster-based satellite imagery.
#' This function extracts raster values and visualizes their distribution across layers.
#'
#' @references
#' See also `im.import()`, `im.ggplot()`.
#'
#' @seealso [GitHub Repository](https://github.com/ducciorocchini/imageRy/)
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(ggridges)
#' library(ggplot2)
#'
#' # Import raster data
#' r <- im.import("greenland")
#'
#' # Generate ridgeline plot
#' im.ridgeline(r, 2, "viridis") + theme_bw()
#' }
#'
#' @export
im.ridgeline <- function(im, scale, palette = c("viridis", "magma", "plasma", "inferno",
                                                "cividis", "mako", "rocket", "turbo")) {

  palette <- palette[1]  # Ensure only the first palette is used

  # Input validation
  if (!is(im, "SpatRaster")) stop("The input 'im' must be a SpatRaster object.")
  if (!is.numeric(scale)) stop("The 'scale' parameter must be numeric.")
  if (!is.character(palette)) stop("The 'palette' parameter must be a character string.")
  if (!palette %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
    stop("The 'palette' must be one of the following: 'viridis', 'magma', 'plasma', 'inferno', 'cividis', 'mako', 'rocket', 'turbo'.")
  }

  # Define the color option mapping
  palette_option <- switch(palette,
                           viridis = 'D',
                           magma = 'A',
                           inferno = 'B',
                           plasma = 'C',
                           cividis = 'E',
                           rocket = 'F',
                           mako = 'G',
                           turbo = 'H')

  # Convert raster to a dataframe
  df <- as.data.frame(im, wide = FALSE)

  # Generate the ridgeline plot
  pl <- ggplot2::ggplot(df, ggplot2::aes(x = values, y = layer, fill = after_stat(x))) +
    ggridges::geom_density_ridges_gradient(scale = scale, rel_min_height = 0.01) +
    ggplot2::scale_fill_viridis_c(option = palette_option)

  return(pl)
}

