# im_plotRGB directly plot any image with the first three bands
im.plotRGB <- function(x){
  plotRGB(x,1,2,3,stretch="lin")
}
