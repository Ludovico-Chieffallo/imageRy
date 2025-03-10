#' Classify a Raster Image Using K-Means Clustering
#'
#' This function performs unsupervised classification on a raster image using k-means clustering.
#' It assigns each pixel to a cluster and optionally visualizes the classified image.
#'
#' @param input_image A `SpatRaster` object representing the input raster image.
#' @param num_clusters An integer specifying the number of clusters (default: 3).
#' @param seed An optional integer seed for reproducibility of k-means clustering results (default: NULL).
#' @param do_plot A logical value indicating whether to display the classified raster (default: TRUE).
#' @param custom_colors A vector of custom colors to be used for classification visualization (default: NULL).
#' If NULL, a predefined set of colors is used.
#' @param num_colors The number of colors to interpolate in the visualization palette (default: 100).
#'
#' @return A `SpatRaster` object with cluster assignments, where each pixel belongs to a classified cluster.
#'
#' @details
#' The function applies k-means clustering on the pixel values of the raster image. Each pixel is treated
#' as a multi-dimensional point, where each band represents a feature. The classified raster assigns
#' each pixel to a cluster, which can be visualized using a color palette.
#'
#' - If `custom_colors` is provided, it is used as the classification color palette.
#' - If `seed` is provided, it ensures reproducibility of k-means clustering.
#' - If `do_plot = TRUE`, the classified raster is displayed with the chosen color scheme.
#'
#' @references
#' K-means clustering is a widely used unsupervised classification algorithm. For more information, see:
#' \url{https://en.wikipedia.org/wiki/K-means_clustering}
#'
#' @seealso [im.import()], [im.ridgeline()]
#'
#' @examples
#' \dontrun{
#' library(terra)
#' 
#' # Load a raster dataset
#' r <- rast(system.file("ex/elev.tif", package = "terra"))
#' 
#' # Perform k-means classification with 4 clusters
#' classified_raster <- im.classify(r, num_clusters = 4, seed = 123, do_plot = TRUE)
#' }
#'
#' @export
im.classify <- function(input_image, num_clusters = 3, seed = NULL, do_plot = TRUE, 
                        custom_colors = NULL, num_colors = 100) {
  
  # Set a default color palette
  base_colors <- c('khaki', 'slateblue', 'olivedrab', 'salmon', 'lightpink', 'darkgrey')
  
  # Determine color palette to use
  colors <- if (is.null(custom_colors)) {
    if (num_clusters > length(base_colors)) {
      colorRampPalette(base_colors)(num_clusters)
    } else {
      base_colors[1:num_clusters]
    }
  } else {
    if (num_clusters > length(custom_colors)) {
      colorRampPalette(custom_colors)(num_clusters)
    } else {
      custom_colors[1:num_clusters]
    }
  }
  
  # Validate input_image as SpatRaster
  if (!inherits(input_image, "SpatRaster")) {
    stop("input_image should be a SpatRaster object.")
  }
  
  # Convert raster to matrix and remove NA values
  image_values <- na.omit(terra::as.matrix(input_image))

  # Normalize values between 0 and 1 (to handle different bit depths)
  image_values <- scale(image_values)  # Standardization (mean=0, sd=1)
  image_values <- (image_values - min(image_values)) / (max(image_values) - min(image_values)) # Min-Max Scaling

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Perform k-means clustering
  kmeans_result <- kmeans(image_values, centers = num_clusters)
  
  # Create classified raster
  classified_image <- input_image[[1]]  # Use first band as base
  values(classified_image) <- kmeans_result$cluster
  
  # Visualize classified raster if requested
  if (do_plot) {
    color_palette <- colorRampPalette(colors)(num_colors)
    plot(classified_image, col = color_palette, axes = FALSE)
  }
  
  # Return classified raster
  return(classified_image)
}
