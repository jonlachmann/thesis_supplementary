# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-04-30

library(gplots)

# Correlation plot for example 1
cormat <- cor(mill_x_g[,-1])
rownames(cormat) <- paste0("x",1:15)
colnames(cormat) <- paste0("x",1:15)
col <- colorRampPalette(c("white", "black"))(20)
heatmap.2(abs(cormat), Rowv=F, Colv=F, dendrogram="none", col=col, density.info = "none", trace="none",
          key.par=list(mar=c(3.5,0,3,0)),
          margins=c(3,0), key.title=NA, key.xlab=NA,
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(1.25, 5), lwid=c(1, 10, 1))

# Correlation plot for example 2
cormat2 <- cor(logistic_ex2[,-c(1,2)])
rownames(cormat2) <- paste0("x",1:20)
colnames(cormat2) <- paste0("x",1:20)
col <- colorRampPalette(c("white", "black"))(20)
heatmap.2(abs(cormat2), Rowv=F, Colv=F, dendrogram="none", col=col, density.info = "none", trace="none",
          key.par=list(mar=c(3.5,0,3,0)),
          margins=c(3,0), key.title=NA, key.xlab=NA,
          lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(1.25, 5), lwid=c(1, 10, 1))
# saved as 1100x1200 @ 200DPI