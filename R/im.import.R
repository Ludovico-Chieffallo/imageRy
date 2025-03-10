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
  # Get the correct directory path for images
  image_dir <- system.file("images", package = "imageRy")

  # If the package is being tested locally, use a direct path
  if (image_dir == "") {
    image_dir <- "C:/Users/ludov/imageRy/images"  # Update this path as needed
  }

  # List available image files
  files_list <- list.files(image_dir)

  # Find an exact match for the file name
  fname <- files_list[files_list == im]

  # Check if exactly one file was found
  if (length(fname) == 0) {
    stop("No matching file found for: ", im)
  } else if (length(fname) > 1) {
    stop("Multiple matches found for: ", im, " - Please specify the exact filename.")
  }

  # Construct the full file path
  fpath <- file.path(image_dir, fname)

  # Load the raster image
  r <- tryCatch({
    terra::rast(fpath)
  }, error = function(e) {
    stop("Failed to load raster: ", im, " - Error: ", e$message)
  })

  # Get the number of bands in the raster
  num_bands <- terra::nlyr(r)

  # Display the raster according to the number of bands
  if (num_bands == 1) {
    plot(r, main = paste("Single-band image:", im))
  } else if (num_bands >= 3) {
    tryCatch({
      terra::plotRGB(r, r = 1, g = 2, b = 3, stretch = "lin", main = paste("Multi-band image:", im))
    }, error = function(e) {
      message("RGB plotting failed for: ", im, " - ", e$message)
      plot(r, main = paste("Displaying first band of:", im))
    })
  } else if (num_bands == 2) {
    # Plot each band separately if there are exactly 2 bands
    old_par <- par(mfrow = c(1, num_bands))
    on.exit(par(old_par))
    for (i in 1:num_bands) {
      plot(r[[i]], main = paste("Band", i, "of", im))
    }
  }

  return(r)
}
