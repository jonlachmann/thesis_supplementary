# Title     : General functions
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-26

# Plot many columns in a matrix, log scale can be enabled too
multiplot <- function (mat, logscale=F, ylim=c(min(mat), max(mat)), ...) {
  if (logscale) {
    mat[mat > 0] <- log(mat[mat > 0])
    mat[mat < 0] <- -log(-mat[mat < 0])
  }
  rbcol <- rainbow(ncol(mat))
  plot(mat[,1], type="l", ylim=ylim, col=rbcol[1], ...)
  for (i in 2:ncol(mat)) lines(mat[,i], col=rbcol[i])
}