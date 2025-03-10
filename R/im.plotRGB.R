#' Plot a Raster Image as an RGB Composite with User-Selected Bands
#'
#' This function visualizes a multispectral raster image using user-defined bands
#' for the Red, Green, and Blue channels. A linear contrast stretch is applied to enhance visualization.
#'
#' @param x A `SpatRaster` object representing the input multispectral image.
#' @param r An integer specifying the band index for the Red channel.
#' @param g An integer specifying the band index for the Green channel.
#' @param b An integer specifying the band index for the Blue channel.
#' @param title A character string specifying the plot title (default: "").
#'
#' @return This function does not return an object. It directly generates a plot.
#'
#' @details
#' - The function allows users to **manually select bands** for RGB visualization.
#' - It applies `stretch="lin"` in `plotRGB()` to enhance contrast.
#' - Axis and label colors are set to white for better contrast with dark backgrounds.
#' - The function supports displaying axes (`axes = TRUE`) and sets plot margins.
#'
#' @seealso [im.plotRGB.auto()], [im.ggplot()]
#'
#' @examples
#' \dontrun{
#' library(terra)
#'
#' # Load a multispectral raster dataset
#' r <- rast(system.file("ex/logo.tif", package = "terra"))
#'
#' # Plot with user-selected bands (example: bands 3, 2, 1 as RGB)
#' im.plotRGB(r, r = 3, g = 2, b = 1, title = "Custom RGB Visualization")
#' }
#'
#' @export
im.plotRGB <- function(x, r, g, b, title = "") {

  # Validate that x is a SpatRaster object
  if (!inherits(x, "SpatRaster")) {
    stop("Input image should be a SpatRaster object.")
  }

  # Validate that r, g, and b are numeric indices
  if (!is.numeric(r) || !is.numeric(g) || !is.numeric(b)) {
    stop("RGB band indices should be numeric.")
  }

  # Set graphical parameters for better visualization
  par(col.axis = "white", col.lab = "white", tck = 0)

  # Plot the raster as an RGB composite
  plotRGB(x, r, g, b, stretch = "lin", axes = TRUE, mar = c(1,1,2,1))

  # Add title
  title(main = title, cex.main = 1.3, line = 2.5)
}
