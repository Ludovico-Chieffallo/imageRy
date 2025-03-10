#' Import and Display a Raster Image from the imageRy Package
#'
#' This function imports and visualizes a raster image from the `imageRy` package.
#' It handles different image formats, ensures the correct path is used, and
#' automatically displays the raster with appropriate visualization.
#'
#' @param im A character string specifying the exact filename of the raster to import.
#' The filename should be present in the "images" directory of the package.
#'
#' @return A `SpatRaster` object containing the imported raster.
#'
#' @details
#' This function loads raster images stored in the "images" directory of the package.
#' It automatically detects the correct path, whether the package is installed or in
#' development mode. The function supports:
#' - **Single-band images:** displayed as grayscale.
#' - **Multi-band images (3+ bands):** displayed as RGB (if applicable).
#' - **Two-band images:** each band is plotted separately.
#'
#' If an exact filename match is not found, an error is returned.
#'
#' @seealso [terra::rast()], [terra::plotRGB()]
#'
#' @examples
#' \dontrun{
#'   # Import and display an example raster
#'   r <- im.import("Sentinel2_NDVI_2020-02-21.tif")
#'
#'   # Check raster properties
#'   print(r)
#' }
#'
#' @importFrom terra rast nlyr plotRGB
#' @export
im.import <- function(im) {
  suppressWarnings({
    # List available image files in the package
    ls <- list.files(system.file("images", package="imageRy"))

    # Find the matching file
    fname <- ls[grep(im, ls)]
    fpath <- system.file("images", fname, package="imageRy")

    # Load the raster image
    r <- rast(fpath)

    # Check if the raster has an extent
    if (is.na(ext(r))) {
      warning("The imported raster has an unknown extent: ", im)
    }

    # Plot the raster
    plot(r)  
    return(r)
  })
}
