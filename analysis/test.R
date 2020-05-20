
### Define models

```{r define-models, out.width = "100%", dpi = 300, echo=TRUE, warning=FALSE}
fit.size<-procD.lm(shape ~ size, data = gdf, print.progress = FALSE, iter = 9999)
fit.sizeunit<-procD.lm(size ~ unit, data = gdf, print.progress = FALSE, iter = 9999)
fit.shapeunit<-procD.lm(shape ~ unit, data = gdf, print.progress = FALSE, iter = 9999)
fit.unique<-procD.lm(shape ~ size * unit, data = gdf, print.progress = FALSE, iter = 9999) # unique allometries
fit.common<-procD.lm(shape ~ size + unit, data = gdf, print.progress = FALSE, iter = 9999) # common allometries
```

### Allometry

```{r allometry, out.width = "100%", dpi = 300, echo=TRUE, warning=FALSE}
# allometry: does shape change with size?  
anova(fit.size)
anova(fit.common)
anova(fit.unique)
plot(fit.size, type = "regression", reg.type = "RegScore", predictor = log(gdf$size), pch = shapes, col = colors)
# common allometric component (Mitteroecker 2004)
plotAllometry(fit.size, size = gdf$size, logsz = TRUE, method = "CAC", pch = shapes, col = colors)
# size-shape PCA (Mitteroecker 2004)
plotAllometry(fit.shapeunit, size = gdf$size, logsz = TRUE, method = "size.shape", pch = shapes, col = colors)
# do Gahagan biface forms from different regions express parallel, convergent, or divergent morphological characteristics?
extremes<-plotAllometry(fit.unique, size = gdf$size, logsz = TRUE, method = "PredLine", pch = shapes, col = colors)
```

### Size/Shape ~ Unit?

```{r region, out.width = "100%", dpi = 300, echo=TRUE, warning=FALSE}
# ANOVA: do incision sizes differ by unit?
anova(fit.sizeunit)
# ANOVA: do incision shapes differ by unit?
anova(fit.shapeunit)
```