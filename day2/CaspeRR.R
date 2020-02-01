library(plot3D)
### Declare variables
x <- df203$southernEdgeDist
y <- df203$mahalDist
z <- df203$painorm

###Change meters to kilometers
x <- x/10^3

### Fit data
fit <- lm (z ~ x+y+x:y)
fitpoints <- predict(fit)

### Prepare surface
grid.lines = 51
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
                 nrow = grid.lines, ncol = grid.lines)
### PDF Device and Plot
pdf("pai203_surface.pdf", width = 8, height = 8)
scatter3D(x, y, z, pch = 16, cex = 1.2, cex.axis=0.7, cex.clab = 
            0.2,
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "Dist. from Southern Edge (Km)",
          ylab = "Dist. from Climate Centroid",
          zlab = "Population Adaptive Index (PAI)",
          zlim = c(0.06047, 0.35814),
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints),
          main= "PAI Relationship with Predictor distances (203 SNPs)")
dev.off()