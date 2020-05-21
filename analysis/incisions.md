Supplementary materials for paper: Does similar ceramic incision
morphology provide inference to a common toolkit?
================
Robert Z. Selden, Jr.
19 May 2020

## Incision morphology

``` r
# location of landmarks
knitr::include_graphics('images/FigLM.jpg')
```

<img src="images/FigLM.jpg" width="100%" />

``` r
fig.cap="Location of landmarks (blue) and equidistant semilandmarks (white)."
```

``` r
# library(devtools)
# devtools::install_github("geomorphR/geomorph", ref = "Stable", build_vignettes = TRUE)
library(geomorph)
```

    ## Loading required package: RRPP

    ## Loading required package: rgl

``` r
setwd(getwd())
source('readmulti.csv.r')

# Read .csv files
setwd("./data2")
filelist <- list.files(pattern = ".csv")
coords<-readmulti.csv(filelist)
setwd("../")

# read qualitative data
qdata<-read.csv("qdata.csv",header=TRUE,row.names=1)
qdata<-qdata[match(dimnames(coords)[[3]],rownames(qdata)),]
```

### Generalised Procrustes Analysis

``` r
Y.gpa<-gpagen(coords, PrinAxes = TRUE, print.progress = FALSE)

# 3D GPA plot
knitr::include_graphics('images/gpa3d.png')
```

<img src="images/gpa3d.png" width="100%" />

``` r
fig.cap="Results of generalized Procrustes analysis."

# geomorph data frame
gdf<-geomorph.data.frame(shape=Y.gpa$coords, size=Y.gpa$Csize, site=qdata$site, unit=qdata$unit, county=qdata$county) 
csz<-Y.gpa$Csize # attribute for boxplot
unt<-qdata$unit # attribute for boxplot
# boxplot of incision (centroid) size by nfgt unit
boxplot(csz~unt, 
        main = "Centroid size of Caddo incisions by NFGT Unit",
        names = c("AngelinaNF", "DavyCrockettNF", "SabineNF"),
        xlab = "Unit",
        ylab = "Centroid Size",
        col = c("dodgerblue4","indianred4","tan3")
        )
```

<img src="incisions_files/figure-gfm/gpa-2.png" width="100%" />

### Principal Components Analysis

``` r
# principal components analysis
pca<-gm.prcomp(Y.gpa$coords)
summary(pca)
```

    ## Importance of components: 
    ##                               PC1         PC2         PC3         PC4
    ## Tips variance          0.04552417 0.006417556 0.003889341 0.001184799
    ## Proportion of variance 0.78157688 0.110179116 0.066773730 0.020341090
    ## Cumulative Proportion  0.78157688 0.891755993 0.958529723 0.978870813
    ##                                 PC5          PC6          PC7          PC8
    ## Tips variance          0.0003866688 0.0002677133 0.0001492361 9.466408e-05
    ## Proportion of variance 0.0066384821 0.0045962070 0.0025621440 1.625230e-03
    ## Cumulative Proportion  0.9855092953 0.9901055023 0.9926676462 9.942929e-01
    ##                                 PC9         PC10         PC11         PC12
    ## Tips variance          5.982512e-05 4.801541e-05 4.546707e-05 3.480457e-05
    ## Proportion of variance 1.027101e-03 8.243475e-04 7.805966e-04 5.975386e-04
    ## Cumulative Proportion  9.953200e-01 9.961443e-01 9.969249e-01 9.975225e-01
    ##                                PC13         PC14         PC15         PC16
    ## Tips variance          2.931908e-05 2.171431e-05 1.614939e-05 1.395524e-05
    ## Proportion of variance 5.033615e-04 3.727999e-04 2.772591e-04 2.395891e-04
    ## Cumulative Proportion  9.980258e-01 9.983986e-01 9.986759e-01 9.989155e-01
    ##                                PC17         PC18         PC19         PC20
    ## Tips variance          1.179212e-05 8.664935e-06 7.477584e-06 6.085707e-06
    ## Proportion of variance 2.024518e-04 1.487630e-04 1.283781e-04 1.044818e-04
    ## Cumulative Proportion  9.991179e-01 9.992667e-01 9.993951e-01 9.994995e-01
    ##                                PC21         PC22         PC23         PC24
    ## Tips variance          4.841792e-06 4.352263e-06 3.069070e-06 2.601889e-06
    ## Proportion of variance 8.312579e-05 7.472136e-05 5.269099e-05 4.467026e-05
    ## Cumulative Proportion  9.995827e-01 9.996574e-01 9.997101e-01 9.997548e-01
    ##                                PC25         PC26         PC27         PC28
    ## Tips variance          2.267951e-06 1.964615e-06 1.700595e-06 1.456587e-06
    ## Proportion of variance 3.893707e-05 3.372928e-05 2.919648e-05 2.500726e-05
    ## Cumulative Proportion  9.997937e-01 9.998274e-01 9.998566e-01 9.998816e-01
    ##                                PC29         PC30         PC31         PC32
    ## Tips variance          1.081584e-06 9.990276e-07 8.407050e-07 7.496439e-07
    ## Proportion of variance 1.856906e-05 1.715170e-05 1.443355e-05 1.287018e-05
    ## Cumulative Proportion  9.999002e-01 9.999173e-01 9.999318e-01 9.999446e-01
    ##                                PC33         PC34         PC35         PC36
    ## Tips variance          5.708100e-07 4.904253e-07 4.188088e-07 3.581936e-07
    ## Proportion of variance 9.799890e-06 8.419814e-06 7.190273e-06 6.149608e-06
    ## Cumulative Proportion  9.999544e-01 9.999629e-01 9.999701e-01 9.999762e-01
    ##                                PC37         PC38         PC39         PC40
    ## Tips variance          3.271383e-07 2.813133e-07 2.379978e-07 1.677313e-07
    ## Proportion of variance 5.616439e-06 4.829698e-06 4.086039e-06 2.879676e-06
    ## Cumulative Proportion  9.999818e-01 9.999867e-01 9.999907e-01 9.999936e-01
    ##                                PC41         PC42         PC43         PC44
    ## Tips variance          1.062281e-07 8.567677e-08 5.197859e-08 4.179147e-08
    ## Proportion of variance 1.823765e-06 1.470933e-06 8.923889e-07 7.174924e-07
    ## Cumulative Proportion  9.999954e-01 9.999969e-01 9.999978e-01 9.999985e-01
    ##                                PC45         PC46         PC47         PC48
    ## Tips variance          3.089382e-08 2.124354e-08 1.856247e-08 1.530597e-08
    ## Proportion of variance 5.303972e-07 3.647175e-07 3.186878e-07 2.627789e-07
    ## Cumulative Proportion  9.999991e-01 9.999994e-01 9.999997e-01 1.000000e+00

``` r
# set plot parameters
# for site
site <- qdata$site
pch.gps.site <- c(1:14)[as.factor(site)]
# for unit
unit <- qdata$unit
pch.gps <- c(15,17,19)[as.factor(unit)]
col.gps <- c("dodgerblue4","indianred4","tan3")[as.factor(unit)]
# for county
county <- qdata$county
pch.gps.cty <- c(1:5)[as.factor(county)]

# plotPCAbySite
pca.plot1<-plot(pca,
                pch = pch.gps.site)
```

<img src="incisions_files/figure-gfm/pca-1.png" width="100%" />

``` r
# plotPCAbyUnit
pca.plot2<-plot(pca,
               pch = pch.gps,
               col = col.gps)
```

<img src="incisions_files/figure-gfm/pca-2.png" width="100%" />

``` r
# plotPCAbyCounty
pca.plot1<-plot(pca,
                pch = pch.gps.cty)
```

<img src="incisions_files/figure-gfm/pca-3.png" width="100%" />

### Define models

``` r
fit.size<-procD.lm(shape ~ size, data = gdf, print.progress = FALSE, iter = 9999)
fit.sizesite<-procD.lm(size ~ site, data = gdf, print.progress = FALSE, iter = 9999)
fit.sizeunit<-procD.lm(size ~ unit, data = gdf, print.progress = FALSE, iter = 9999)
fit.sizecounty<-procD.lm(size ~ county, data = gdf, print.progress = FALSE, iter = 9999)
fit.shapesite<-procD.lm(shape ~ site, data = gdf, print.progress = FALSE, iter = 9999)
fit.shapeunit<-procD.lm(shape ~ unit, data = gdf, print.progress = FALSE, iter = 9999)
fit.shapecounty<-procD.lm(shape ~ county, data = gdf, print.progress = FALSE, iter = 9999)
```

### Size/Shape \~ Site?

``` r
# ANOVA: do incision sizes differ by site?
anova(fit.sizesite)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS     MS     Rsq      F       Z Pr(>F)
    ## site      14  38.744 2.7674 0.31061 1.3195 0.66338 0.2404
    ## Residuals 41  85.990 2.0973 0.68939                      
    ## Total     55 124.733                                     
    ## 
    ## Call: procD.lm(f1 = size ~ site, iter = 9999, data = gdf, print.progress = FALSE)

``` r
# ANOVA: do incision shapes differ by site?
anova(fit.shapesite)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df     SS       MS     Rsq      F       Z Pr(>F)
    ## site      14 0.4223 0.030163 0.13182 0.4446 -1.6922 0.9529
    ## Residuals 41 2.7813 0.067836 0.86818                      
    ## Total     55 3.2036                                       
    ## 
    ## Call: procD.lm(f1 = shape ~ site, iter = 9999, data = gdf, print.progress = FALSE)

### Size/Shape \~ Unit?

``` r
# ANOVA: do incision sizes differ by unit?
anova(fit.sizeunit)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS     MS     Rsq      F      Z Pr(>F)
    ## unit       2   7.626 3.8129 0.06114 1.7256 0.8544 0.1841
    ## Residuals 53 117.108 2.2096 0.93886                     
    ## Total     55 124.733                                    
    ## 
    ## Call: procD.lm(f1 = size ~ unit, iter = 9999, data = gdf, print.progress = FALSE)

``` r
# pairwise comparison of LS means = which differ?
sz.unit<-pairwise(fit.sizeunit, groups = qdata$unit)
summary(sz.unit, confidence = 0.95, test.type = "dist")
```

    ## 
    ## Pairwise comparisons
    ## 
    ## Groups: angelinaNF davycNF sabineNF 
    ## 
    ## RRPP: 10000 permutations
    ## 
    ## LS means:
    ## Vectors hidden (use show.vectors = TRUE to view)
    ## 
    ## Pairwise distances between means, plus statistics
    ##                              d UCL (95%)         Z Pr > d
    ## angelinaNF:davycNF  1.10609363 1.4301861  1.186953 0.1318
    ## angelinaNF:sabineNF 1.11849632 1.1916408  1.686973 0.0665
    ## davycNF:sabineNF    0.01240268 0.9971793 -1.311534 0.9826

``` r
# pairwise distance between variances = standardization?
summary(sz.unit, confidence = 0.95, test.type = "var")
```

    ## 
    ## Pairwise comparisons
    ## 
    ## Groups: angelinaNF davycNF sabineNF 
    ## 
    ## RRPP: 10000 permutations
    ## 
    ## 
    ## Observed variances by group
    ## 
    ## angelinaNF    davycNF   sabineNF 
    ##   1.081951   1.994539   2.305105 
    ## 
    ## Pairwise distances between variances, plus statistics
    ##                            d UCL (95%)           Z Pr > d
    ## angelinaNF:davycNF  0.912588  4.126200 -0.41439254 0.5222
    ## angelinaNF:sabineNF 1.223154  4.018034  0.03534721 0.3267
    ## davycNF:sabineNF    0.310566  2.868934 -0.91594089 0.8200

``` r
# ANOVA: do incision shapes differ by unit?
anova(fit.shapeunit)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df     SS       MS     Rsq      F       Z Pr(>F)
    ## unit       2 0.1054 0.052682 0.03289 0.9012 0.20151 0.4343
    ## Residuals 53 3.0982 0.058457 0.96711                      
    ## Total     55 3.2036                                       
    ## 
    ## Call: procD.lm(f1 = shape ~ unit, iter = 9999, data = gdf, print.progress = FALSE)

``` r
# pairwise comparison of LS means = which differ?
sh.unit<-pairwise(fit.shapeunit, groups = qdata$unit)
summary(sh.unit, confidence = 0.95, test.type = "dist")
```

    ## 
    ## Pairwise comparisons
    ## 
    ## Groups: angelinaNF davycNF sabineNF 
    ## 
    ## RRPP: 10000 permutations
    ## 
    ## LS means:
    ## Vectors hidden (use show.vectors = TRUE to view)
    ## 
    ## Pairwise distances between means, plus statistics
    ##                              d UCL (95%)          Z Pr > d
    ## angelinaNF:davycNF  0.07130926 0.2082799 -0.6004452 0.6856
    ## angelinaNF:sabineNF 0.12343786 0.1761212  0.7569169 0.1985
    ## davycNF:sabineNF    0.05901499 0.1447249 -0.3873345 0.5883

``` r
# pairwise distance between variances = standardization?
summary(sh.unit, confidence = 0.95, test.type = "var")
```

    ## 
    ## Pairwise comparisons
    ## 
    ## Groups: angelinaNF davycNF sabineNF 
    ## 
    ## RRPP: 10000 permutations
    ## 
    ## 
    ## Observed variances by group
    ## 
    ## angelinaNF    davycNF   sabineNF 
    ## 0.03229303 0.05200414 0.06052898 
    ## 
    ## Pairwise distances between variances, plus statistics
    ##                               d  UCL (95%)          Z Pr > d
    ## angelinaNF:davycNF  0.019711105 0.09341561 -0.6264371 0.6769
    ## angelinaNF:sabineNF 0.028235950 0.07617484 -0.1767030 0.5489
    ## davycNF:sabineNF    0.008524844 0.06121075 -0.9477908 0.8099

### Size/Shape \~ Unit?

``` r
# ANOVA: do incision sizes differ by county?
anova(fit.sizecounty)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df      SS     MS     Rsq      F       Z Pr(>F)
    ## county     4   8.006 2.0015 0.06419 0.8745 0.15623 0.4666
    ## Residuals 51 116.727 2.2888 0.93581                      
    ## Total     55 124.733                                     
    ## 
    ## Call: procD.lm(f1 = size ~ county, iter = 9999, data = gdf, print.progress = FALSE)

``` r
# ANOVA: do incision shapes differ by county?
anova(fit.shapecounty)
```

    ## 
    ## Analysis of Variance, using Residual Randomization
    ## Permutation procedure: Randomization of null model residuals 
    ## Number of permutations: 10000 
    ## Estimation method: Ordinary Least Squares 
    ## Sums of Squares and Cross-products: Type I 
    ## Effect sizes (Z) based on F distributions
    ## 
    ##           Df     SS       MS     Rsq      F        Z Pr(>F)
    ## county     4 0.1683 0.042085 0.05255 0.7071 -0.29143 0.6018
    ## Residuals 51 3.0352 0.059514 0.94745                       
    ## Total     55 3.2036                                        
    ## 
    ## Call: procD.lm(f1 = shape ~ county, iter = 9999, data = gdf, print.progress = FALSE)

### Morphological disparity

``` r
# morphological disparity: does incision morphology display greater shape variation among individuals relative to site, unit, or county?
# site
morphol.disparity(fit.shapesite, groups = qdata$site, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.shapesite, groups = qdata$site, iter = 9999,  
    ##     data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ##        na132        sa255         sa65         sa66        sb125        sb164 
    ## 7.256502e-31 1.385143e-35 1.325657e-35 4.071424e-02 1.620162e-02 1.798813e-33 
    ##        sb189        sb291        sb308        sy253        sy255        sy258 
    ## 6.318766e-03 1.446752e-02 4.024853e-32 2.407781e-31 5.369324e-32 3.271987e-03 
    ##        sy280         sy43         tn91 
    ## 7.517003e-02 8.397089e-02 5.200414e-02 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##              na132        sa255         sa65       sa66       sb125
    ## na132 0.000000e+00 7.256363e-31 7.256369e-31 0.04071424 0.016201617
    ## sa255 7.256363e-31 0.000000e+00 5.948569e-37 0.04071424 0.016201617
    ## sa65  7.256369e-31 5.948569e-37 0.000000e+00 0.04071424 0.016201617
    ## sa66  4.071424e-02 4.071424e-02 4.071424e-02 0.00000000 0.024512623
    ## sb125 1.620162e-02 1.620162e-02 1.620162e-02 0.02451262 0.000000000
    ## sb164 7.238514e-31 1.784962e-33 1.785557e-33 0.04071424 0.016201617
    ## sb189 6.318766e-03 6.318766e-03 6.318766e-03 0.03439547 0.009882851
    ## sb291 1.446752e-02 1.446752e-02 1.446752e-02 0.02624672 0.001734096
    ## sb308 6.854016e-31 4.023468e-32 4.023527e-32 0.04071424 0.016201617
    ## sy253 4.848720e-31 2.407643e-31 2.407649e-31 0.04071424 0.016201617
    ## sy255 6.719569e-31 5.367939e-32 5.367998e-32 0.04071424 0.016201617
    ## sy258 3.271987e-03 3.271987e-03 3.271987e-03 0.03744225 0.012929630
    ## sy280 7.517003e-02 7.517003e-02 7.517003e-02 0.03445579 0.058968418
    ## sy43  8.397089e-02 8.397089e-02 8.397089e-02 0.04325665 0.067769273
    ## tn91  5.200414e-02 5.200414e-02 5.200414e-02 0.01128990 0.035802521
    ##              sb164       sb189       sb291        sb308        sy253
    ## na132 7.238514e-31 0.006318766 0.014467522 6.854016e-31 4.848720e-31
    ## sa255 1.784962e-33 0.006318766 0.014467522 4.023468e-32 2.407643e-31
    ## sa65  1.785557e-33 0.006318766 0.014467522 4.023527e-32 2.407649e-31
    ## sa66  4.071424e-02 0.034395475 0.026246719 4.071424e-02 4.071424e-02
    ## sb125 1.620162e-02 0.009882851 0.001734096 1.620162e-02 1.620162e-02
    ## sb164 0.000000e+00 0.006318766 0.014467522 3.844972e-32 2.389793e-31
    ## sb189 6.318766e-03 0.000000000 0.008148756 6.318766e-03 6.318766e-03
    ## sb291 1.446752e-02 0.008148756 0.000000000 1.446752e-02 1.446752e-02
    ## sb308 3.844972e-32 0.006318766 0.014467522 0.000000e+00 2.005296e-31
    ## sy253 2.389793e-31 0.006318766 0.014467522 2.005296e-31 0.000000e+00
    ## sy255 5.189443e-32 0.006318766 0.014467522 1.344471e-32 1.870849e-31
    ## sy258 3.271987e-03 0.003046779 0.011195535 3.271987e-03 3.271987e-03
    ## sy280 7.517003e-02 0.068851269 0.060702513 7.517003e-02 7.517003e-02
    ## sy43  8.397089e-02 0.077652124 0.069503369 8.397089e-02 8.397089e-02
    ## tn91  5.200414e-02 0.045685372 0.037536617 5.200414e-02 5.200414e-02
    ##              sy255       sy258       sy280        sy43       tn91
    ## na132 6.719569e-31 0.003271987 0.075170035 0.083970890 0.05200414
    ## sa255 5.367939e-32 0.003271987 0.075170035 0.083970890 0.05200414
    ## sa65  5.367998e-32 0.003271987 0.075170035 0.083970890 0.05200414
    ## sa66  4.071424e-02 0.037442253 0.034455794 0.043256650 0.01128990
    ## sb125 1.620162e-02 0.012929630 0.058968418 0.067769273 0.03580252
    ## sb164 5.189443e-32 0.003271987 0.075170035 0.083970890 0.05200414
    ## sb189 6.318766e-03 0.003046779 0.068851269 0.077652124 0.04568537
    ## sb291 1.446752e-02 0.011195535 0.060702513 0.069503369 0.03753662
    ## sb308 1.344471e-32 0.003271987 0.075170035 0.083970890 0.05200414
    ## sy253 1.870849e-31 0.003271987 0.075170035 0.083970890 0.05200414
    ## sy255 0.000000e+00 0.003271987 0.075170035 0.083970890 0.05200414
    ## sy258 3.271987e-03 0.000000000 0.071898048 0.080698903 0.04873215
    ## sy280 7.517003e-02 0.071898048 0.000000000 0.008800855 0.02316590
    ## sy43  8.397089e-02 0.080698903 0.008800855 0.000000000 0.03196675
    ## tn91  5.200414e-02 0.048732151 0.023165896 0.031966752 0.00000000
    ## 
    ## 
    ## P-Values
    ##        na132  sa255   sa65   sa66  sb125  sb164  sb189  sb291  sb308  sy253
    ## na132 1.0000 0.9861 0.9861 0.4126 0.6585 0.9878 0.8479 0.6657 0.9869 0.9913
    ## sa255 0.9861 1.0000 1.0000 0.4117 0.6568 0.9994 0.8512 0.6673 0.9968 0.9900
    ## sa65  0.9861 1.0000 1.0000 0.4160 0.6621 0.9991 0.8451 0.6685 0.9968 0.9900
    ## sa66  0.4126 0.4117 0.4160 1.0000 0.5564 0.4098 0.4862 0.5349 0.4060 0.4242
    ## sb125 0.6585 0.6568 0.6621 0.5564 1.0000 0.6596 0.7668 0.9583 0.6572 0.6666
    ## sb164 0.9878 0.9994 0.9991 0.4098 0.6596 1.0000 0.8479 0.6601 0.9979 0.9917
    ## sb189 0.8479 0.8512 0.8451 0.4862 0.7668 0.8479 1.0000 0.7894 0.8509 0.8448
    ## sb291 0.6657 0.6673 0.6685 0.5349 0.9583 0.6601 0.7894 1.0000 0.6611 0.6713
    ## sb308 0.9869 0.9968 0.9968 0.4060 0.6572 0.9979 0.8509 0.6611 1.0000 0.9921
    ## sy253 0.9913 0.9900 0.9900 0.4242 0.6666 0.9917 0.8448 0.6713 0.9921 1.0000
    ## sy255 0.9907 0.9945 0.9948 0.4116 0.6563 0.9940 0.8497 0.6604 0.9978 0.9923
    ## sy258 0.9252 0.9181 0.9218 0.4690 0.7035 0.9264 0.9180 0.7196 0.9199 0.9256
    ## sy280 0.1406 0.1310 0.1327 0.5023 0.2470 0.1314 0.2008 0.2355 0.1282 0.1437
    ## sy43  0.1699 0.1576 0.1688 0.4173 0.2491 0.1643 0.2223 0.2470 0.1614 0.1724
    ## tn91  0.3231 0.3117 0.3146 0.8207 0.5142 0.3168 0.4021 0.4861 0.3156 0.3265
    ##        sy255  sy258  sy280   sy43   tn91
    ## na132 0.9907 0.9252 0.1406 0.1699 0.3231
    ## sa255 0.9945 0.9181 0.1310 0.1576 0.3117
    ## sa65  0.9948 0.9218 0.1327 0.1688 0.3146
    ## sa66  0.4116 0.4690 0.5023 0.4173 0.8207
    ## sb125 0.6563 0.7035 0.2470 0.2491 0.5142
    ## sb164 0.9940 0.9264 0.1314 0.1643 0.3168
    ## sb189 0.8497 0.9180 0.2008 0.2223 0.4021
    ## sb291 0.6604 0.7196 0.2355 0.2470 0.4861
    ## sb308 0.9978 0.9199 0.1282 0.1614 0.3156
    ## sy253 0.9923 0.9256 0.1437 0.1724 0.3265
    ## sy255 1.0000 0.9180 0.1285 0.1619 0.3045
    ## sy258 0.9180 1.0000 0.1957 0.2113 0.3719
    ## sy280 0.1285 0.1957 1.0000 0.8281 0.5122
    ## sy43  0.1619 0.2113 0.8281 1.0000 0.4350
    ## tn91  0.3045 0.3719 0.5122 0.4350 1.0000

``` r
# unit
morphol.disparity(fit.shapeunit, groups = qdata$unit, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.shapeunit, groups = qdata$unit, iter = 9999,  
    ##     data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ## angelinaNF    davycNF   sabineNF 
    ## 0.03229303 0.05200414 0.06052898 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##            angelinaNF     davycNF    sabineNF
    ## angelinaNF 0.00000000 0.019711105 0.028235950
    ## davycNF    0.01971111 0.000000000 0.008524844
    ## sabineNF   0.02823595 0.008524844 0.000000000
    ## 
    ## 
    ## P-Values
    ##            angelinaNF davycNF sabineNF
    ## angelinaNF     1.0000  0.6769   0.5489
    ## davycNF        0.6769  1.0000   0.8099
    ## sabineNF       0.5489  0.8099   1.0000

``` r
# county
morphol.disparity(fit.shapecounty, groups = qdata$county, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.shapecounty, groups = qdata$county,  
    ##     iter = 9999, data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ##   nacogdoches        sabine san-augustine        shelby       trinity 
    ##  1.276665e-31  1.471299e-02  3.214775e-02  7.809059e-02  5.200414e-02 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##               nacogdoches     sabine san-augustine     shelby    trinity
    ## nacogdoches    0.00000000 0.01471299    0.03214775 0.07809059 0.05200414
    ## sabine         0.01471299 0.00000000    0.01743476 0.06337760 0.03729115
    ## san-augustine  0.03214775 0.01743476    0.00000000 0.04594284 0.01985639
    ## shelby         0.07809059 0.06337760    0.04594284 0.00000000 0.02608645
    ## trinity        0.05200414 0.03729115    0.01985639 0.02608645 0.00000000
    ## 
    ## 
    ## P-Values
    ##               nacogdoches sabine san-augustine shelby trinity
    ## nacogdoches        1.0000 0.7936        0.5306 0.1203  0.3563
    ## sabine             0.7936 1.0000        0.7121 0.0497  0.3620
    ## san-augustine      0.5306 0.7121        1.0000 0.2454  0.6835
    ## shelby             0.1203 0.0497        0.2454 1.0000  0.4618
    ## trinity            0.3563 0.3620        0.6835 0.4618  1.0000

``` r
# morphological disparity: does incision morphology display greater size variation among individuals relative to site, unit, or county?
# site
morphol.disparity(fit.sizeunit, groups = qdata$site, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.sizeunit, groups = qdata$site, iter = 9999,  
    ##     data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ##       na132       sa255        sa65        sa66       sb125       sb164 
    ##  0.01487731  0.22620050  0.84475079  1.62195741  0.75617117  0.72295373 
    ##       sb189       sb291       sb308       sy253       sy255       sy258 
    ##  1.80963102  5.45290735  1.01898103  1.91564179  0.24877461 14.82459952 
    ##       sy280        sy43        tn91 
    ##  1.66315000  1.12141317  1.99453916 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##            na132       sa255        sa65        sa66       sb125       sb164
    ## na132  0.0000000  0.21132319  0.82987348  1.60708010  0.74129386  0.70807642
    ## sa255  0.2113232  0.00000000  0.61855029  1.39575691  0.52997067  0.49675323
    ## sa65   0.8298735  0.61855029  0.00000000  0.77720662  0.08857962  0.12179706
    ## sa66   1.6070801  1.39575691  0.77720662  0.00000000  0.86578624  0.89900368
    ## sb125  0.7412939  0.52997067  0.08857962  0.86578624  0.00000000  0.03321744
    ## sb164  0.7080764  0.49675323  0.12179706  0.89900368  0.03321744  0.00000000
    ## sb189  1.7947537  1.58343052  0.96488023  0.18767361  1.05345985  1.08667729
    ## sb291  5.4380300  5.22670685  4.60815656  3.83094994  4.69673618  4.72995362
    ## sb308  1.0041037  0.79278053  0.17423024  0.60297638  0.26280986  0.29602730
    ## sy253  1.9007645  1.68944129  1.07089100  0.29368438  1.15947062  1.19268806
    ## sy255  0.2338973  0.02257411  0.59597618  1.37318280  0.50739656  0.47417912
    ## sy258 14.8097222 14.59839902 13.97984873 13.20264211 14.06842835 14.10164578
    ## sy280  1.6482727  1.43694950  0.81839921  0.04119259  0.90697883  0.94019627
    ## sy43   1.1065359  0.89521267  0.27666238  0.50054424  0.36524200  0.39845944
    ## tn91   1.9796618  1.76833866  1.14978837  0.37258175  1.23836799  1.27158543
    ##            sb189    sb291      sb308       sy253       sy255     sy258
    ## na132  1.7947537 5.438030  1.0041037  1.90076448  0.23389730 14.809722
    ## sa255  1.5834305 5.226707  0.7927805  1.68944129  0.02257411 14.598399
    ## sa65   0.9648802 4.608157  0.1742302  1.07089100  0.59597618 13.979849
    ## sa66   0.1876736 3.830950  0.6029764  0.29368438  1.37318280 13.202642
    ## sb125  1.0534599 4.696736  0.2628099  1.15947062  0.50739656 14.068428
    ## sb164  1.0866773 4.729954  0.2960273  1.19268806  0.47417912 14.101646
    ## sb189  0.0000000 3.643276  0.7906500  0.10601077  1.56085641 13.014968
    ## sb291  3.6432763 0.000000  4.4339263  3.53726556  5.20413274  9.371692
    ## sb308  0.7906500 4.433926  0.0000000  0.89666076  0.77020642 13.805618
    ## sy253  0.1060108 3.537266  0.8966608  0.00000000  1.66686718 12.908958
    ## sy255  1.5608564 5.204133  0.7702064  1.66686718  0.00000000 14.575825
    ## sy258 13.0149685 9.371692 13.8056185 12.90895773 14.57582491  0.000000
    ## sy280  0.1464810 3.789757  0.6441690  0.25249179  1.41437539 13.161450
    ## sy43   0.6882179 4.331494  0.1024321  0.79422862  0.87263856 13.703186
    ## tn91   0.1849081 3.458368  0.9755581  0.07889737  1.74576455 12.830060
    ##             sy280       sy43        tn91
    ## na132  1.64827269  1.1065359  1.97966185
    ## sa255  1.43694950  0.8952127  1.76833866
    ## sa65   0.81839921  0.2766624  1.14978837
    ## sa66   0.04119259  0.5005442  0.37258175
    ## sb125  0.90697883  0.3652420  1.23836799
    ## sb164  0.94019627  0.3984594  1.27158543
    ## sb189  0.14648102  0.6882179  0.18490814
    ## sb291  3.78975735  4.3314942  3.45836819
    ## sb308  0.64416897  0.1024321  0.97555813
    ## sy253  0.25249179  0.7942286  0.07889737
    ## sy255  1.41437539  0.8726386  1.74576455
    ## sy258 13.16144952 13.7031863 12.83006036
    ## sy280  0.00000000  0.5417368  0.33138916
    ## sy43   0.54173683  0.0000000  0.87312599
    ## tn91   0.33138916  0.8731260  0.00000000
    ## 
    ## 
    ## P-Values
    ##        na132  sa255   sa65   sa66  sb125  sb164  sb189  sb291  sb308  sy253
    ## na132 1.0000 0.8734 0.6442 0.4170 0.7070 0.6990 0.3804 0.0782 0.5878 0.3915
    ## sa255 0.8734 1.0000 0.7269 0.4746 0.7859 0.7664 0.4280 0.0868 0.6581 0.4424
    ## sa65  0.6442 0.7269 1.0000 0.7068 0.9640 0.9194 0.6155 0.1071 0.8962 0.5760
    ## sa66  0.4170 0.4746 0.7068 1.0000 0.5772 0.6508 0.9113 0.1209 0.7717 0.8830
    ## sb125 0.7070 0.7859 0.9640 0.5772 1.0000 0.9850 0.5322 0.0937 0.8978 0.5495
    ## sb164 0.6990 0.7664 0.9194 0.6508 0.9850 1.0000 0.5839 0.1017 0.8241 0.5534
    ## sb189 0.3804 0.4280 0.6155 0.9113 0.5322 0.5839 1.0000 0.1129 0.6781 0.9500
    ## sb291 0.0782 0.0868 0.1071 0.1209 0.0937 0.1017 0.1129 1.0000 0.1094 0.1613
    ## sb308 0.5878 0.6581 0.8962 0.7717 0.8978 0.8241 0.6781 0.1094 1.0000 0.6232
    ## sy253 0.3915 0.4424 0.5760 0.8830 0.5495 0.5534 0.9500 0.1613 0.6232 1.0000
    ## sy255 0.8532 0.9807 0.7390 0.4822 0.8026 0.7863 0.4383 0.0865 0.6725 0.4492
    ## sy258 0.0271 0.0286 0.0396 0.0253 0.0135 0.0397 0.0529 0.0758 0.0380 0.0452
    ## sy280 0.4070 0.4883 0.7503 0.9762 0.5678 0.6989 0.9312 0.0452 0.8036 0.9265
    ## sy43  0.5979 0.6865 0.9090 0.7285 0.8147 0.8660 0.6833 0.0799 0.9677 0.7229
    ## tn91  0.3219 0.3736 0.6116 0.7978 0.4351 0.5553 0.9153 0.1028 0.6729 0.9774
    ##        sy255  sy258  sy280   sy43   tn91
    ## na132 0.8532 0.0271 0.4070 0.5979 0.3219
    ## sa255 0.9807 0.0286 0.4883 0.6865 0.3736
    ## sa65  0.7390 0.0396 0.7503 0.9090 0.6116
    ## sa66  0.4822 0.0253 0.9762 0.7285 0.7978
    ## sb125 0.8026 0.0135 0.5678 0.8147 0.4351
    ## sb164 0.7863 0.0397 0.6989 0.8660 0.5553
    ## sb189 0.4383 0.0529 0.9312 0.6833 0.9153
    ## sb291 0.0865 0.0758 0.0452 0.0799 0.1028
    ## sb308 0.6725 0.0380 0.8036 0.9677 0.6729
    ## sy253 0.4492 0.0452 0.9265 0.7229 0.9774
    ## sy255 1.0000 0.0305 0.5038 0.6962 0.3896
    ## sy258 0.0305 1.0000 0.0260 0.0169 0.0309
    ## sy280 0.5038 0.0260 1.0000 0.6976 0.8140
    ## sy43  0.6962 0.0169 0.6976 1.0000 0.5413
    ## tn91  0.3896 0.0309 0.8140 0.5413 1.0000

``` r
# unit
morphol.disparity(fit.sizeunit, groups = qdata$unit, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.sizeunit, groups = qdata$unit, iter = 9999,  
    ##     data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ## angelinaNF    davycNF   sabineNF 
    ##   1.081951   1.994539   2.305105 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##            angelinaNF  davycNF sabineNF
    ## angelinaNF   0.000000 0.912588 1.223154
    ## davycNF      0.912588 0.000000 0.310566
    ## sabineNF     1.223154 0.310566 0.000000
    ## 
    ## 
    ## P-Values
    ##            angelinaNF davycNF sabineNF
    ## angelinaNF     1.0000  0.5222   0.3267
    ## davycNF        0.5222  1.0000   0.8200
    ## sabineNF       0.3267  0.8200   1.0000

``` r
# county
morphol.disparity(fit.sizeunit, groups = qdata$unit, data = gdf, print.progress = FALSE, iter = 9999)
```

    ## 
    ## Call:
    ## morphol.disparity(f1 = fit.sizeunit, groups = qdata$unit, iter = 9999,  
    ##     data = gdf, print.progress = FALSE) 
    ## 
    ## 
    ## 
    ## Randomized Residual Permutation Procedure Used
    ## 10000 Permutations
    ## 
    ## Procrustes variances for defined groups
    ## angelinaNF    davycNF   sabineNF 
    ##   1.081951   1.994539   2.305105 
    ## 
    ## 
    ## Pairwise absolute differences between variances
    ##            angelinaNF  davycNF sabineNF
    ## angelinaNF   0.000000 0.912588 1.223154
    ## davycNF      0.912588 0.000000 0.310566
    ## sabineNF     1.223154 0.310566 0.000000
    ## 
    ## 
    ## P-Values
    ##            angelinaNF davycNF sabineNF
    ## angelinaNF     1.0000  0.5222   0.3267
    ## davycNF        0.5222  1.0000   0.8200
    ## sabineNF       0.3267  0.8200   1.0000

### Mean shapes

``` r
# subset landmark coordinates to produce mean shapes by site
new.coords<-coords.subset(A = Y.gpa$coords, group = qdata$site)
names(new.coords)
```

    ##  [1] "na132" "sa255" "sa65"  "sa66"  "sb125" "sb164" "sb189" "sb291" "sb308"
    ## [10] "sy253" "sy255" "sy258" "sy280" "sy43"  "tn91"

``` r
# group shape means
mean<-lapply(new.coords, mshape)
# plot(mean$tn91)
knitr::include_graphics('images/site-meanshape.png')
```

<img src="images/site-meanshape.png" width="2916" />

``` r
fig.cap="Mean shapes for Caddo incisions from sites on the National Forests and Grasslands in Texas."
# end of code
```

### Acknowledgments

I extend my gratitude to the Caddo Tribe of Oklahoma and the National
Forests and Grasslands in Texas for the requisite permissions and access
needed to generate the scans of the Caddo incisions, and to the National
Center for Preservation Technology and Training for access to the
optical profilometer.

### Funding

Components of this analytical work flow were developed and funded by a
Preservation Technology and Training grant (P14AP00138) to the author
from the National Center for Preservation Technology and Training, and
funding for this project was provided by grants to the author from the
Caddo Tribe of Oklahoma and the National Forests and Grasslands in Texas
(15-PA-11081300-033 and 20-PA-11081300-074).

### References cited
