# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-30

cormat <- cor(mill_x_g[,-1])
lower.tri(cormat)
heatmap(cormat, Colv=NA, Rowv=NA, symm=T)