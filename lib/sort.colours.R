## FUNCTION TO SORT COLORS
#  Source: http://goo.gl/3lg2

sort.colours <- function(col) {
  require(colorspace)
  c.rgb = col2rgb(col)
  c.RGB = RGB(t(c.rgb) %*% diag(rep(1/255, 3)))
  c.HSV = as(c.RGB, "HSV")@coords
  order(c.HSV[, 1], c.HSV[, 2], c.HSV[, 3])
}
