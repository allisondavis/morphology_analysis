reg.lat.P2.L <- lm(lat$P2.L ~ lat$SL)
sd.lat.P2.L <- rstandard(reg.lat.P2.L)
reg.lat.P2.L.plot <- ggplot(lat, aes(x = SL, y = P2.L)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.lat.P2.L.plot

reg.lat.P2.R <- lm(lat$P2.R ~ lat$SL)
sd.lat.P2.R <- rstandard(reg.lat.P2.R)
reg.lat.P2.R.plot <- ggplot(lat, aes(x = SL, y = P2.R)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.lat.P2.R.plot

reg.form.P2.L <- lm(form$P2.L ~ form$SL)
sd.form.P2.L <- rstandard(reg.form.P2.L)
reg.form.P2.L.plot <- ggplot(form, aes(x = SL, y = P2.L)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.form.P2.L.plot

reg.form.P2.R <- lm(form$P2.R ~ form$SL)
sd.form.P2.R <- rstandard(reg.form.P2.R)
reg.form.P2.R.plot <- ggplot(form, aes(x = SL, y = P2.R)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.form.P2.R.plot

reg.mex.P2.L <- lm(mex$P2.L ~ mex$SL)
sd.mex.P2.L <- rstandard(reg.mex.P2.L)
reg.mex.P2.L.plot <- ggplot(mex, aes(x = SL, y = P2.L)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.mex.P2.L.plot

reg.mex.P2.R <- lm(mex$P2.R ~ mex$SL)
sd.mex.P2.R <- rstandard(reg.mex.P2.R)
reg.mex.P2.R.plot <- ggplot(mex, aes(x = SL, y = P2.R)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  stat_cor(label.y = 10)
reg.mex.P2.R.plot

##### F-tests

Gonna try F-tests for funsies. Would be interesting if both are varying, but in different ways (amazon with identical variance around best fit, whereas lat with more variation around best fit for example).

```{r, echo=FALSE}


Ftest.abs.BD <- var.test(abs.lat.BD, abs.form.BD, alternative = "two.sided")
Ftest.abs.BD


Ftest.abs.CPD <- var.test(abs.lat.CPD, abs.form.CPD, alternative = "two.sided")
Ftest.abs.CPD


Ftest.abs.CPL <- var.test(abs.lat.CPL, abs.form.CPL, alternative = "two.sided")
Ftest.abs.CPL


Ftest.abs.PreDL <- var.test(abs.lat.PreDL, abs.form.PreDL, alternative = "two.sided")
Ftest.abs.PreDL


Ftest.abs.DbL <- var.test(abs.lat.DbL, abs.form.DbL, alternative = "two.sided")
Ftest.abs.DbL


Ftest.abs.HL <- var.test(abs.lat.HL, abs.form.HL, alternative = "two.sided")
Ftest.abs.HL


Ftest.abs.HD <- var.test(abs.lat.HD, abs.form.HD, alternative = "two.sided")
Ftest.abs.HD


Ftest.abs.HW <- var.test(abs.lat.HW, abs.form.HW, alternative = "two.sided")
Ftest.abs.HW


Ftest.abs.SnL <- var.test(abs.lat.SnL, abs.form.SnL, alternative = "two.sided")
Ftest.abs.SnL


Ftest.abs.OL <- var.test(abs.lat.OL, abs.form.OL, alternative = "two.sided")
Ftest.abs.OL




```

##### Mann Whitney U tests

This will be performed on traits that **DID** vary with SL.

```{r}

(MW_D <- wilcox.test(abs.res.D~SPP, data=raw3, conf.int=T))

(MW_P1 <- wilcox.test(abs.res.P1~SPP, data=raw3, conf.int=T))

(MW_P1R <- wilcox.test(abs.res.P1.R~SPP, data=raw3, conf.int=T))

(MW_LLSC <- wilcox.test(abs.res.LLSC~SPP, data=raw3, conf.int=T))


```

##### Variance of residuals

I did notice that the deviance residual information was out of whack (median not super close to zero in many cases, the max and min VERY different). In the EXTRAS rscript, I ran a DHARMa test to see what the issue was, and apparently they fail the levene's test for homogeneity of variance. This indicates that while the AVERAGE variance is not different between the two species, there could be a different in how the species are varying, which may be interesting (similar to the F-tests of the continuous residuals). Will run some levene's test for these discrete residuals, just to see. 

```{r}

library(car)
library(carData)

(L_D <- leveneTest(abs.res.D~SPP, data=raw3))

(L_P1 <- leveneTest(abs.res.P1~SPP, data=raw3))

(L_P1R <- leveneTest(abs.res.P1.R~SPP, data=raw3))

(L_LLSC <- leveneTest(abs.res.LLSC~SPP, data=raw3))

```

##### F-tests

```{r}
var.left.pel <- var.test(lat$P2.L, form$P2.L, alternative = "two.sided")
var.left.pel

ggplot(raw2, aes(SPP, P2.L)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

var.rig.pel <- var.test(lat$P2.R, form$P2.R, alternative = "two.sided")
var.rig.pel

ggplot(raw2, aes(SPP, P2.R)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

var.anal <- var.test(lat$A, form$A, alternative = "two.sided")
var.anal

ggplot(raw2, aes(SPP, A)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

var.sca.ab.ll <- var.test(lat$SALL, form$SALL, alternative = "two.sided")
var.sca.ab.ll

ggplot(raw2, aes(SPP, SALL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

var.sca.df <- var.test(lat$SBDF, form$SBDF, alternative = "two.sided")
var.sca.df

ggplot(raw2, aes(SPP, SBDF)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

var.flu.asy <- var.test(lat$FLA, form$FLA, alternative = "two.sided")
var.flu.asy

ggplot(raw2, aes(SPP, FLA)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)


```

##### Component extractions

```{r}
library(AMR) 
library(ggplot2)
library(ggfortify)


(PCA$sdev ^ 2)

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}


ev <- PCA$sdev^2 
evplot(ev) #according to Kaiser-Guttman criteron, we can use the first 4 PCs, even though the broken stick model shows only the first above the red bar plot... not 100% confident I know what this means, but pretty sure PC1 is body size


```

###### By zone

```{r}

plot2<- autoplot(PCA, data = raw1a, colour='QUARTILE', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot2

```


###### By Basin

```{r}

plot3<- autoplot(PCA, data = raw1a, colour='BASIN', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot3

```


###### By watershed

```{r}
plot4<- autoplot(PCA, data = raw1a, colour='WATERSHED', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot4

```




###### By zone

```{r}

plot6<- autoplot(PCA, x=2, y=3, data = raw1a, colour='QUARTILE', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot6

```


###### By basin

```{r}

plot7<- autoplot(PCA, x=2, y=3, data = raw1a, colour='BASIN', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot7

```


###### By watershed

```{r}

plot8<- autoplot(PCA, x=2, y=3, data = raw1a, colour='WATERSHED', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot8

```

***
  
  
  #### Density comparisons
  
  I will try to compare the area of the density clusters for the PCA. I will attempt to do this use EMD (earth mover's distance). From what I understand, this will basically tell me how much "work" is needed to transform one distribution into another, thus providing a metric of the overall difference in shape between two distributions. 

**To do this, I would need to be able to separate the loadings based on species, but I have no idea how to do that without running two separate PCAs, one for each group (which sorta defeats the point I think). **


library(emdist) OR library(energy)












library("ggplot2")
library(ChemoSpec)
library(robustbase)
data(metMUD2)

# Original factor encoding:
levels(metMUD2$groups)

# Split those original levels into 2 new ones (re-code them)
new.grps <- list(geneBb = c("B", "b"), geneCc = c("C", "c"))
mM3 <- splitSpectraGroups(metMUD2, new.grps)

# run aov_pcaSpectra
PCAs <- aov_pcaSpectra(mM3, fac = c("geneBb", "geneCc"))

p1 <- aovPCAscores(mM3, PCAs, submat = 1, ellipse = "cls")
p1 <- p1 + ggtitle("aovPCA: B vs b")
p1

p2 <- aovPCAscores(mM3, PCAs, submat = 2)
p2 <- p2 + ggtitle("aovPCA: C vs c")
p2

p3 <- aovPCAscores(mM3, PCAs, submat = 3)
p3 <- p3 + ggtitle("aovPCA: Interaction Term")
p3

p4 <- aovPCAloadings(spectra = mM3, PCA = PCAs)
p4 <- p4 + ggtitle("aov_pcaSpectra: Bb Loadings")
p4











#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")

#BiocManager::install("DESeq2")


plot1a<- autoplot(PCA2, data = raw1c, colour='SPP', loadings=FALSE, loadings.label=FALSE, frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot1a

plot1b<- autoplot(PCA3, data = raw1d, colour='SPP', loadings=FALSE, loadings.label=FALSE, frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot1b


#### test for density shit ###
raw1c <- raw1a[raw1a$SPP == "p.latipinna",]

PCA2 <- prcomp(raw1c[, 34:52])

summary(PCA2)

(loadings2 <- PCA2$rotation[, 1:5])


raw1d <- raw1a[raw1a$SPP == "p.formosa",]

PCA3 <- prcomp(raw1d[, 34:52])

summary(PCA3)

(loadings3 <- PCA3$rotation[, 1:5])




library(DHARMa)

test <- glm(abs.res.D~SPP, data=raw3)
print(summary(test), show.residuals=TRUE)
test2 <- simulateResiduals(test)
plot(test2)

#library(lawstat)
#brunner.munzel.test(raw3$SPP, raw3$abs.res.D, alternative = "two.sided")






#### Residuals 

-   **STEP TWO:** get residuals for each individual for traits that were influenced by body size

```{r echo=FALSE}

##### LAT #####
res.lat.D <- resid(reg.lat.D)

res.lat.P1 <- resid(reg.lat.P1)

res.lat.P1.R <- resid(reg.lat.P1.R)

res.lat.LLSC <- resid(reg.lat.LLSC)

res.lat.SBLL <- resid(reg.lat.SBLL) #will only be used when comparing with p.mex; raw data used otherwise

res.lat.BD <- resid(reg.lat.BD)

res.lat.CPD <- resid(reg.lat.CPD)

res.lat.CPL <- resid(reg.lat.CPL)

res.lat.PreDL <- resid(reg.lat.PreDL)

res.lat.DbL <- resid(reg.lat.DbL)

res.lat.HL <- resid(reg.lat.HL)

res.lat.HD <- resid(reg.lat.HD)

res.lat.HW <- resid(reg.lat.HW)

res.lat.SnL <- resid(reg.lat.SnL)

res.lat.OL <- resid(reg.lat.OL)

##### FORM #####

res.form.D <- resid(reg.form.D)

res.form.P1 <- resid(reg.form.P1)

res.form.P1.R <- resid(reg.form.P1.R)

res.form.LLSC <- resid(reg.form.LLSC)

res.form.SBLL <- resid(reg.form.SBLL) #will only be used when comparing with p.mex; raw data used otherwise

res.form.BD <- resid(reg.form.BD)

res.form.CPD <- resid(reg.form.CPD)

res.form.CPL <- resid(reg.form.CPL)

res.form.PreDL <- resid(reg.form.PreDL)

res.form.DbL <- resid(reg.form.DbL)

res.form.HL <- resid(reg.form.HL)

res.form.HD <- resid(reg.form.HD)

res.form.HW <- resid(reg.form.HW)

res.form.SnL <- resid(reg.form.SnL)

res.form.OL <- resid(reg.form.OL)


##### MEX #####
res.mex.D <- resid(reg.mex.D)

res.mex.P1 <- resid(reg.mex.P1)

res.mex.P1.R <- resid(reg.mex.P1.R)

res.mex.LLSC <- resid(reg.mex.LLSC)

res.mex.SBLL <- resid(reg.mex.SBLL)

res.mex.BD <- resid(reg.mex.BD)

res.mex.CPD <- resid(reg.mex.CPD)

res.mex.CPL <- resid(reg.mex.CPL)

res.mex.PreDL <- resid(reg.mex.PreDL)

res.mex.DbL <- resid(reg.mex.DbL)

res.mex.HL <- resid(reg.mex.HL)

res.mex.HD <- resid(reg.mex.HD)

res.mex.HW <- resid(reg.mex.HW)

res.mex.SnL <- resid(reg.mex.SnL)

res.mex.OL <- resid(reg.mex.OL)


```

-   **STEP THREE:** convert residuals to absolute value

```{r}

##### LAT #####

abs.lat.D <- abs(res.lat.D)
mean(abs.lat.D)

abs.lat.P1 <- abs(res.lat.P1)
mean(abs.lat.P1)

abs.lat.P1.R <- abs(res.lat.P1.R)
mean(abs.lat.P1.R)

abs.lat.LLSC <- abs(res.lat.LLSC)
mean(abs.lat.LLSC)

abs.lat.SBLL <- abs(res.lat.SBLL)
mean(abs.lat.SBLL)

abs.lat.BD <- abs(res.lat.BD)
mean(abs.lat.BD)

abs.lat.CPD <- abs(res.lat.CPD)
mean(abs.lat.CPD)

abs.lat.CPL <- abs(res.lat.CPL)
mean(abs.lat.CPL)

abs.lat.PreDL <- abs(res.lat.PreDL)
mean(abs.lat.PreDL)

abs.lat.DbL <- abs(res.lat.DbL)
mean(abs.lat.DbL)

abs.lat.HL <- abs(res.lat.HL)
mean(abs.lat.HL)

abs.lat.HD <- abs(res.lat.HD)
mean(abs.lat.HD)

abs.lat.HW <- abs(res.lat.HW)
mean(abs.lat.HW)

abs.lat.SnL <- abs(res.lat.SnL)
mean(abs.lat.SnL)

abs.lat.OL <- abs(res.lat.OL)
mean(abs.lat.OL)


##### FORM #####

abs.form.D <- abs(res.form.D)
mean(abs.form.D)

abs.form.P1 <- abs(res.form.P1)
mean(abs.form.P1)

abs.form.P1.R <- abs(res.form.P1.R)
mean(abs.form.P1.R)

abs.form.LLSC <- abs(res.form.LLSC)
mean(abs.form.LLSC)

abs.form.SBLL <- abs(res.form.SBLL)
mean(abs.form.SBLL)

abs.form.BD <- abs(res.form.BD)
mean(abs.form.BD)

abs.form.CPD <- abs(res.form.CPD)
mean(abs.form.CPD)

abs.form.CPL <- abs(res.form.CPL)
mean(abs.form.CPL)

abs.form.PreDL <- abs(res.form.PreDL)
mean(abs.form.PreDL)

abs.form.DbL <- abs(res.form.DbL)
mean(abs.form.DbL)

abs.form.HL <- abs(res.form.HL)
mean(abs.form.HL)

abs.form.HD <- abs(res.form.HD)
mean(abs.form.HD)

abs.form.HW <- abs(res.form.HW)
mean(abs.form.HW)

abs.form.SnL <- abs(res.form.SnL)
mean(abs.form.SnL)

abs.form.OL <- abs(res.form.OL)
mean(abs.form.OL)

##### MEX #####

abs.mex.D <- abs(res.mex.D)
mean(abs.mex.D)

abs.mex.P1 <- abs(res.mex.P1)
mean(abs.mex.P1)

abs.mex.P1.R <- abs(res.mex.P1.R)
mean(abs.mex.P1.R)

abs.mex.LLSC <- abs(res.mex.LLSC)
mean(abs.mex.LLSC)

abs.mex.SBLL <- abs(res.mex.SBLL)
mean(abs.mex.SBLL)

abs.mex.BD <- abs(res.mex.BD)
mean(abs.mex.BD)

abs.mex.CPD <- abs(res.mex.CPD)
mean(abs.mex.CPD)

abs.mex.CPL <- abs(res.mex.CPL)
mean(abs.mex.CPL)

abs.mex.PreDL <- abs(res.mex.PreDL)
mean(abs.mex.PreDL)

abs.mex.DbL <- abs(res.mex.DbL)
mean(abs.mex.DbL)

abs.mex.HL <- abs(res.mex.HL)
mean(abs.mex.HL)

abs.mex.HD <- abs(res.mex.HD)
mean(abs.mex.HD)

abs.mex.HW <- abs(res.mex.HW)
mean(abs.mex.HW)

abs.mex.SnL <- abs(res.mex.SnL)
mean(abs.mex.SnL)

abs.mex.OL <- abs(res.mex.OL)
mean(abs.mex.OL)


#let's get this into the raw1 data set so that we can plot this more easily

abs.res.D <- c(abs.lat.D, abs.form.D, abs.mex.D)
abs.res.P1 <- c(abs.lat.P1, abs.form.P1, abs.mex.P1)
abs.res.P1.R <- c(abs.lat.P1.R, abs.form.P1.R, abs.mex.P1.R)
abs.res.LLSC<- c(abs.lat.LLSC, abs.form.LLSC, abs.mex.LLSC)
abs.res.SBLL<- c(abs.lat.SBLL, abs.form.SBLL, abs.mex.SBLL)
abs.res.BD<- c(abs.lat.BD, abs.form.BD, abs.mex.BD)
abs.res.CPD<- c(abs.lat.CPD, abs.form.CPD, abs.mex.CPD)
abs.res.CPL<- c(abs.lat.CPL, abs.form.CPL, abs.mex.CPL)
abs.res.PreDL <- c(abs.lat.PreDL, abs.form.PreDL, abs.mex.PreDL)
abs.res.DbL <- c(abs.lat.DbL, abs.form.DbL, abs.mex.DbL)
abs.res.HL<- c(abs.lat.HL, abs.form.HL, abs.mex.HL)
abs.res.HD<- c(abs.lat.HD, abs.form.HD, abs.mex.HD)
abs.res.HW <- c(abs.lat.HW, abs.form.HW, abs.mex.HW)
abs.res.SnL <- c(abs.lat.SnL, abs.form.SnL, abs.mex.SnL)
abs.res.OL <- c(abs.lat.OL, abs.form.OL, abs.mex.OL)


raw2 <- cbind(raw1, abs.res.D, abs.res.P1, abs.res.P1.R, abs.res.LLSC, abs.res.SBLL, abs.res.BD, abs.res.CPD, abs.res.CPL, abs.res.PreDL, abs.res.DbL, abs.res.HL, abs.res.HD, abs.res.HW, abs.res.SnL, abs.res.OL)

```

#### Residual Comparisons 

-   **STEP FOUR:** plot the mean of the |residuals| for both species, for all traits influenced by body size

```{r}

library(ggbeeswarm)
library(dplyr)


ggplot(raw2, aes(SPP, abs.res.D)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)


scatter_violin <- ggplot(data=raw2, aes(x=SPP, y=abs.res.D)) +
  geom_violin(trim = FALSE) + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1),
    geom = "pointrange", color = "black"
  )

print(scatter_violin)

scatter_violin1 <- ggplot(data=raw2, aes(x=SPP, y=abs.res.D)) +
  geom_violin(trim = FALSE) + 
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="crossbar", fill="red", width=0.03)

print(scatter_violin1)



ggplot(raw2, aes(SPP, abs.res.P1)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.P1.R)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3) 

ggplot(raw2, aes(SPP, abs.res.LLSC)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.SBLL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.BD)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.CPD)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.CPL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3) 

ggplot(raw2, aes(SPP, abs.res.PreDL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.DbL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.HL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.HD)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.HW)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.SnL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)

ggplot(raw2, aes(SPP, abs.res.OL)) +
  geom_point(alpha=0.3) +
  stat_summary(fun.data=function(x){mean_cl_normal(x, conf.int=.683)}, geom="errorbar", 
               width=0.03, colour="red", alpha=0.7) +
  stat_summary(fun=mean, geom="point", fill="red", pch=21, size=3)




```

## Comparing variation 

### Variance tests {.tabset .tabset-fade .tabset-pills}

t-test for residuals, continuous
f-test for raw, continuous
all discrete will go through glmer/non parametric with either residuals or raw

*(note: did non-parametric instead of transforming, but Mike said to transform, so I copied that work to the 'morphology-analysis-final' Rmd).*
  
  F-test on residuals doesn't make much sense; the residuals themselves are representative of the variation present, as they are the distance from the mean. Therefore, F-test on residuals is like variance of the variance. Instead, I have to do a t-test on the absolute value of the residuals. In this sense, we want to see if the mean of the absolute residuals is higher or lower for asexual species--is the average amount of variation higher or lower for this trait? Based on the regressions, if the trait was influenced by body size, I will perform a t-test on the absolute value of the residuals. If the trait was not influenced by body size, I will perform an F-test of variance on the raw data.

**Quick results summary:** For the F-test on raw data, none of the traits were significantly different (P2L/R, A, SBDF, FLA). For the t-tests on residuals, the only significant traits are left pectoral (), right pectoral (lat>form), scales above lateral line (), scales below lateral line (form>lat), and head length ().

```         
-   will do two-tailed and check out the residual means to infer direction; for traits in which we use raw data, a one-tailed f-test will be perfomed in both direction to determine which species is varying more. We will also visulize the variation using a histogram to confirm direction results.
```

#### F tests to compare species 

ALL OF THESE ARE DISCRETE SO I DON'T THINK I DO ANY F TESTS

This will only be for the traits that did **not** have a significant regression compared to SL (so P2s, A, SALL, SBDF, and FLA); none of these traits were transformed, so just doing raw for now.




var.left.pel <- var.test(lat$P2.L, form$P2.L, alternative = "two.sided")
var.left.pel

var.rig.pel <- var.test(lat$P2.R, form$P2.R, alternative = "two.sided")
var.rig.pel

var.anal <- var.test(lat$A, form$A, alternative = "two.sided")
var.anal

var.sca.ab.ll <- var.test(lat$SALL, form$SALL, alternative = "two.sided")
var.sca.ab.ll

var.sca.df <- var.test(lat$SBDF, form$SBDF, alternative = "two.sided")
var.sca.df

var.flu.asy <- var.test(lat$FLA, form$FLA, alternative = "two.sided")
var.flu.asy









<br><br><br><br>
  
  
  
  
  #### T tests to compare species 
  
  CONTINUOUS RESIDUALS


-    will do two-tailed and check out the means to infer direction (see beginning for trait means w/o body correction)
-    this will be for the traits that *did* have a significant regression compared to SL; will be using the absolute value residuals. 
-    For continuous traits, we will be using the log transformed data.



```{r, echo=FALSE}


ttest.abs.BD <- t.test(abs.lat.BD, abs.form.BD, alternative = "two.sided")
ttest.abs.BD


ttest.abs.CPD <- t.test(abs.lat.CPD, abs.form.CPD, alternative = "two.sided")
ttest.abs.CPD


ttest.abs.CPL <- t.test(abs.lat.CPL, abs.form.CPL, alternative = "two.sided")
ttest.abs.CPL


ttest.abs.PreDL <- t.test(abs.lat.PreDL, abs.form.PreDL, alternative = "two.sided")
ttest.abs.PreDL


ttest.abs.DbL <- t.test(abs.lat.DbL, abs.form.DbL, alternative = "two.sided")
ttest.abs.DbL


ttest.abs.HL <- t.test(abs.lat.HL, abs.form.HL, alternative = "two.sided")
ttest.abs.HL


ttest.abs.HD <- t.test(abs.lat.HD, abs.form.HD, alternative = "two.sided")
ttest.abs.HD


ttest.abs.HW <- t.test(abs.lat.HW, abs.form.HW, alternative = "two.sided")
ttest.abs.HW


ttest.abs.SnL <- t.test(abs.lat.SnL, abs.form.SnL, alternative = "two.sided")
ttest.abs.SnL


ttest.abs.OL <- t.test(abs.lat.OL, abs.form.OL, alternative = "two.sided")
ttest.abs.OL




```




#### Levene's test

Since the count data can't do parametric, I will do non-parametric tests. Logan mentioned a glmer with poisson distribution but I don't think this will answer the question of a difference in variance between the species. The linear model will instead tell me how much species effects differences in the trait?? I guess?
  
  I will use Levene's test as a non-parametric F-test, to check out differences in variance in raw data.

I will use Mann Whitney U test as a non-parametric t-test, to check out differences in variance in residual data.


Still only performing this on the traits that did **NOT** vary with SL (P2L/R, A, SALL, SBLL [between sail and amz only], SBDF, FLA).

```{r}
raw3 <- raw2[raw2$SPP !="p.mexicana", ]


library(car)
library(carData)

(LT_P2L <- leveneTest(P2.L~SPP, data=raw3)) #gives nothing since it's all the same value

(LT_P2R <- leveneTest(P2.R~SPP, data=raw3)) #same as above

(LT_A <- leveneTest(A~SPP, data=raw3)) 

(LT_SALL <- leveneTest(SALL~SPP, data=raw3))

(LT_SBLL <- leveneTest(SBLL~SPP, data=raw3))

(LT_SBDF <- leveneTest(SBDF~SPP, data=raw3))

(LT_FLA <- leveneTest(FLA~SPP, data=raw3))



```

#### Mann Whitney U tests

This will be performed on traits that **DID** vary with SL.

```{r}

(MW_D <- wilcox.test(abs.res.D~SPP, data=raw3, conf.int=T))

(MW_P1 <- wilcox.test(abs.res.P1~SPP, data=raw3, conf.int=T))

(MW_P1R <- wilcox.test(abs.res.P1.R~SPP, data=raw3, conf.int=T))

(MW_LLSC <- wilcox.test(abs.res.LLSC~SPP, data=raw3, conf.int=T))


```




### Summary of variance results 



average.table <- matrix(c(MW_D$p.value, mean(abs.lat.D, na.rm = TRUE), mean(abs.form.D, na.rm = TRUE), MW_P1$p.value, mean(abs.lat.P1, na.rm = TRUE), mean(abs.form.P1, na.rm = TRUE), LT_P2R$"Pr(>F)", mean(lat$P2.R, na.rm = TRUE), mean(form$P2.R, na.rm = TRUE), LT_P2R$"Pr(>F)", mean(lat$P2.L, na.rm = TRUE), mean(form$P2.L, na.rm = TRUE), LT_A$"Pr(>F)", mean(lat$A, na.rm = TRUE), mean(form$A, na.rm = TRUE), MW_P1R$p.value, mean(abs.lat.P1.R, na.rm = TRUE), mean(abs.form.P1.R, na.rm = TRUE), MW_LLSC$p.value, mean(abs.lat.LLSC, na.rm = TRUE), mean(abs.form.LLSC, na.rm = TRUE), LT_SALL$"Pr(>F)", mean(lat$SALL, na.rm = TRUE), mean(form$SALL, na.rm = TRUE), LT_SBLL$"Pr(>F)", mean(abs.lat.SBLL, na.rm = TRUE), mean(abs.form.SBLL, na.rm = TRUE), LT_SBDF$"Pr(>F)", mean(lat$SBDF, na.rm = TRUE), mean(form$SBDF, na.rm = TRUE), ttest.abs.BD$p.value, mean(abs.lat.BD, na.rm = TRUE), mean(abs.form.BD, na.rm = TRUE), ttest.abs.CPD$p.value, mean(abs.lat.CPD, na.rm = TRUE), mean(abs.form.CPD, na.rm = TRUE), ttest.abs.CPL$p.value, mean(abs.lat.CPL, na.rm = TRUE), mean(abs.form.CPL, na.rm = TRUE), ttest.abs.PreDL$p.value, mean(abs.lat.PreDL, na.rm = TRUE), mean(abs.form.PreDL, na.rm = TRUE), ttest.abs.DbL$p.value, mean(abs.lat.DbL, na.rm = TRUE), mean(abs.form.DbL, na.rm = TRUE), ttest.abs.HL$p.value, mean(abs.lat.HL, na.rm = TRUE), mean(abs.form.HL, na.rm = TRUE), ttest.abs.HD$p.value, mean(abs.lat.HD, na.rm = TRUE), mean(abs.form.HD, na.rm = TRUE), ttest.abs.HW$p.value, mean(abs.lat.HW, na.rm = TRUE), mean(abs.form.HW, na.rm = TRUE), ttest.abs.SnL$p.value, mean(abs.lat.SnL, na.rm = TRUE), mean(abs.form.SnL, na.rm = TRUE), ttest.abs.OL$p.value, mean(abs.lat.OL, na.rm = TRUE), mean(abs.form.OL, na.rm = TRUE), LT_FLA$"Pr(>F)", mean(lat$FLA, na.rm = TRUE), mean(form$FLA, na.rm = TRUE)), ncol=3, byrow=TRUE)

colnames(average.table)<- c("p-value for variance", "lat mean", "form mean")

rownames(average.table) <- c("dorsal rays", "L pect rays", "R pelvic rays*", "L pelvic rays*", "Anal rays", "R pect rays*", "lat line scales", "scales above LL", "scales below LL", "scales before dor*", "body dep", "peduncle dep", "peduncle leng", "pre-dorsal", "dorsal base", "head length", "head depth", "head width", "snout leng", "orbital leng", "fluct. asymmetries")

average.table





### Variance plots


```{r}

plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(bins=10, alpha=0.4, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.2)  +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}

plot_multi_histogram(raw3, "abs.res.CPD", "SPP")

plot_multi_histogram(raw3, "abs.res.DbL", "SPP")

plot_multi_histogram(raw3, "abs.res.P1", "SPP")

plot_multi_histogram(raw3, "abs.res.P1.R", "SPP")


```







## ANOVA 

Will run an ANOVA on the residuals with location and species as fixed effects. This will show me if morphology depends on the species, the location, and if the location and species interact to determine morphology. CANNOT DO WITH COUNT DATA--may have to run a glmer with zero-tuncated negative binomial/quasi-poisson distribution instead (think this will tell me if species, location, and interaction affect the trait...). Will only work with the residuals, because comparing the means of the residuals indicates differences in variation. **Would I not just use Levene's and MWU again but include spp and zone/basin/watershed?**

Leaving the anovas for count data in rn until I figure out what to do.

I will first run this using the zones as the location factor. Zones (1-4) represent the latitude range with equivalent sample sizes in each, since the collections were not equally representative of all latitudes, and I wanted to avoid a sampling bias when randomly selecting samples. Zone 1 corresponds to the southern most latitude range, and zone 4 corresponds to the northern most latitude range.

I will then run the same analysis using basin as the location factor. Since fish are physically isolated to the river basins they occupy, the genetic variation is also limited to that basin. Thus it is possible for fish within the same basin to be more similar due to genetic/physical constraints. (will also do with watershed just to see).

Lastly I will run ANOVAs with both zones and basins but with standardized residuals. This would allow me to compare overall variation across traits (at least those that are depended on body size) rather than just one trait at a time. Not 100% sure if this is useful (or correct to do), but thought it would be interesting.

### ANOVAs {.tabset .tabset-fade .tabset-pills}

#### Amazon

##### Zones 

```{r}
library(ggplot2)

lat3 <- raw2[raw2$SPP == "p.latipinna", ]

form3 <- raw2[raw2$SPP == "p.latipinna", ]

A.D <- aov(abs.res.D ~ QUARTILE, data=form3)
summary(A.D)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.D)) +
  geom_boxplot()


A.P1 <- aov(abs.res.P1 ~ QUARTILE, data=form3)
summary(A.P1)

A.P1.R <- aov(abs.res.P1.R ~ QUARTILE, data=form3)
summary(A.P1.R)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.P1.R)) +
  geom_boxplot()


A.LLSC <- aov(abs.res.LLSC ~ QUARTILE, data=form3)
summary(A.LLSC)


A.SBLL <- aov(abs.res.SBLL ~ QUARTILE, data=form3)
summary(A.SBLL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.SBLL)) +
  geom_boxplot()


A.BD <- aov(abs.res.BD ~ QUARTILE, data=form3)
summary(A.BD)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.BD)) +
  geom_boxplot()


A.CPD <- aov(abs.res.CPD ~ QUARTILE, data=form3)
summary(A.CPD)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.CPD)) +
  geom_boxplot()


A.CPL <- aov(abs.res.CPL ~ QUARTILE, data=form3)
summary(A.CPL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.CPL)) +
  geom_boxplot()


A.PreDL <- aov(abs.res.PreDL ~ QUARTILE, data=form3)
summary(A.PreDL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.PreDL)) +
  geom_boxplot()


A.DbL <- aov(abs.res.DbL ~ QUARTILE, data=form3)
summary(A.DbL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.DbL)) +
  geom_boxplot()


A.HL <- aov(abs.res.HL ~ QUARTILE, data=form3)
summary(A.HL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.HL)) +
  geom_boxplot()


A.HD <- aov(abs.res.HD ~ QUARTILE, data=form3)
summary(A.HD)

A.HW <- aov(abs.res.HW ~ QUARTILE, data=form3)
summary(A.HW)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.HW)) +
  geom_boxplot()


A.SnL <- aov(abs.res.SnL ~ QUARTILE, data=form3)
summary(A.SnL)

A.OL <- aov(abs.res.OL ~ QUARTILE, data=form3)
summary(A.OL)

ggplot(form3, aes(x=factor(QUARTILE), y=abs.res.OL)) +
  geom_boxplot()


```

##### Basins 

```{r}

A1.D <- aov(abs.res.D ~ BASIN, data=form3)
summary(A1.D)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.D)) +
  geom_boxplot()


A1.P1 <- aov(abs.res.P1 ~ BASIN, data=form3)
summary(A1.P1)

A1.P1.R <- aov(abs.res.P1.R ~ BASIN, data=form3)
summary(A1.P1.R)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.P1.R)) +
  geom_boxplot()

A1.LLSC <- aov(abs.res.LLSC ~ BASIN, data=form3)
summary(A1.LLSC)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.LLSC)) +
  geom_boxplot()



A1.SBLL <- aov(abs.res.SBLL ~ BASIN, data=form3)
summary(A1.SBLL)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.SBLL)) +
  geom_boxplot()

A1.BD <- aov(abs.res.BD ~ BASIN, data=form3)
summary(A1.BD)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.BD)) +
  geom_boxplot()

A1.CPD <- aov(abs.res.CPD ~ BASIN, data=form3)
summary(A1.CPD)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.CPD)) +
  geom_boxplot()

A1.CPL <- aov(abs.res.CPL ~ BASIN, data=form3)
summary(A1.CPL)

A1.PreDL <- aov(abs.res.PreDL ~ BASIN, data=form3)
summary(A1.PreDL)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.PreDL)) +
  geom_boxplot()

A1.DbL <- aov(abs.res.DbL ~ BASIN, data=form3)
summary(A1.DbL)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.DbL)) +
  geom_boxplot()

A1.HL <- aov(abs.res.HL ~ BASIN, data=form3)
summary(A1.HL)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.HL)) +
  geom_boxplot()

A1.HD <- aov(abs.res.HD ~ BASIN, data=form3)
summary(A1.HD)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.HD)) +
  geom_boxplot()

A1.HW <- aov(abs.res.HW ~ BASIN, data=form3)
summary(A1.HW)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.HW)) +
  geom_boxplot()

A1.SnL <- aov(abs.res.SnL ~ BASIN, data=form3)
summary(A1.SnL)

A1.OL <- aov(abs.res.OL ~ BASIN, data=form3)
summary(A1.OL)

ggplot(form3, aes(x=factor(BASIN), y=abs.res.OL)) +
  geom_boxplot()




```

##### Watersheds 

```{r}

A2.D <- aov(abs.res.D ~ WATERSHED, data=form3)
summary(A2.D)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.D)) +
  geom_boxplot()

A2.P1 <- aov(abs.res.P1 ~ WATERSHED, data=form3)
summary(A2.P1)

A2.P1.R <- aov(abs.res.P1.R ~ WATERSHED, data=form3)
summary(A2.P1.R)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.P1.R)) +
  geom_boxplot()

A2.LLSC <- aov(abs.res.LLSC ~ WATERSHED, data=form3)
summary(A2.LLSC)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.LLSC)) +
  geom_boxplot()



A2.SBLL <- aov(abs.res.SBLL ~ WATERSHED, data=form3)
summary(A2.SBLL)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.SBLL)) +
  geom_boxplot()

A2.BD <- aov(abs.res.BD ~ WATERSHED, data=form3)
summary(A2.BD)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.BD)) +
  geom_boxplot()

A2.CPD <- aov(abs.res.CPD ~ WATERSHED, data=form3)
summary(A2.CPD)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.CPD)) +
  geom_boxplot()

A2.CPL <- aov(abs.res.CPL ~ WATERSHED, data=form3)
summary(A2.CPL)

A2.PreDL <- aov(abs.res.PreDL ~ WATERSHED, data=form3)
summary(A2.PreDL)

A2.DbL <- aov(abs.res.DbL ~ WATERSHED, data=form3)
summary(A2.DbL)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.DbL)) +
  geom_boxplot()

A2.HL <- aov(abs.res.HL ~ WATERSHED, data=form3)
summary(A2.HL)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.HL)) +
  geom_boxplot()

A2.HD <- aov(abs.res.HD ~ WATERSHED, data=form3)
summary(A2.HD)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.HD)) +
  geom_boxplot()

A2.HW <- aov(abs.res.HW ~ WATERSHED, data=form3)
summary(A2.HW)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.HW)) +
  geom_boxplot()

A2.SnL <- aov(abs.res.SnL ~ WATERSHED, data=form3)
summary(A2.SnL)

A2.OL <- aov(abs.res.OL ~ WATERSHED, data=form3)
summary(A2.OL)

ggplot(form3, aes(x=factor(WATERSHED), y=abs.res.OL)) +
  geom_boxplot()




```

#### Sailfin


##### Zones 

```{r}
library(ggplot2)

A.D <- aov(abs.res.D ~ QUARTILE, data=lat3)
summary(A.D)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.D)) +
  geom_boxplot()


A.P1 <- aov(abs.res.P1 ~ QUARTILE, data=lat3)
summary(A.P1)

A.P1.R <- aov(abs.res.P1.R ~ QUARTILE, data=lat3)
summary(A.P1.R)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.P1.R)) +
  geom_boxplot()


A.LLSC <- aov(abs.res.LLSC ~ QUARTILE, data=lat3)
summary(A.LLSC)


A.SBLL <- aov(abs.res.SBLL ~ QUARTILE, data=lat3)
summary(A.SBLL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.SBLL)) +
  geom_boxplot()


A.BD <- aov(abs.res.BD ~ QUARTILE, data=lat3)
summary(A.BD)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.BD)) +
  geom_boxplot()


A.CPD <- aov(abs.res.CPD ~ QUARTILE, data=lat3)
summary(A.CPD)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.CPD)) +
  geom_boxplot()


A.CPL <- aov(abs.res.CPL ~ QUARTILE, data=lat3)
summary(A.CPL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.CPL)) +
  geom_boxplot()


A.PreDL <- aov(abs.res.PreDL ~ QUARTILE, data=lat3)
summary(A.PreDL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.PreDL)) +
  geom_boxplot()


A.DbL <- aov(abs.res.DbL ~ QUARTILE, data=lat3)
summary(A.DbL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.DbL)) +
  geom_boxplot()


A.HL <- aov(abs.res.HL ~ QUARTILE, data=lat3)
summary(A.HL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.HL)) +
  geom_boxplot()


A.HD <- aov(abs.res.HD ~ QUARTILE, data=lat3)
summary(A.HD)

A.HW <- aov(abs.res.HW ~ QUARTILE, data=lat3)
summary(A.HW)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.HW)) +
  geom_boxplot()


A.SnL <- aov(abs.res.SnL ~ QUARTILE, data=lat3)
summary(A.SnL)

A.OL <- aov(abs.res.OL ~ QUARTILE, data=lat3)
summary(A.OL)

ggplot(lat3, aes(x=factor(QUARTILE), y=abs.res.OL)) +
  geom_boxplot()


```

##### Basins 

```{r}

A1.D <- aov(abs.res.D ~ BASIN, data=lat3)
summary(A1.D)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.D)) +
  geom_boxplot()


A1.P1 <- aov(abs.res.P1 ~ BASIN, data=lat3)
summary(A1.P1)

A1.P1.R <- aov(abs.res.P1.R ~ BASIN, data=lat3)
summary(A1.P1.R)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.P1.R)) +
  geom_boxplot()

A1.LLSC <- aov(abs.res.LLSC ~ BASIN, data=lat3)
summary(A1.LLSC)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.LLSC)) +
  geom_boxplot()



A1.SBLL <- aov(abs.res.SBLL ~ BASIN, data=lat3)
summary(A1.SBLL)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.SBLL)) +
  geom_boxplot()

A1.BD <- aov(abs.res.BD ~ BASIN, data=lat3)
summary(A1.BD)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.BD)) +
  geom_boxplot()

A1.CPD <- aov(abs.res.CPD ~ BASIN, data=lat3)
summary(A1.CPD)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.CPD)) +
  geom_boxplot()

A1.CPL <- aov(abs.res.CPL ~ BASIN, data=lat3)
summary(A1.CPL)

A1.PreDL <- aov(abs.res.PreDL ~ BASIN, data=lat3)
summary(A1.PreDL)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.PreDL)) +
  geom_boxplot()

A1.DbL <- aov(abs.res.DbL ~ BASIN, data=lat3)
summary(A1.DbL)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.DbL)) +
  geom_boxplot()

A1.HL <- aov(abs.res.HL ~ BASIN, data=lat3)
summary(A1.HL)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.HL)) +
  geom_boxplot()

A1.HD <- aov(abs.res.HD ~ BASIN, data=lat3)
summary(A1.HD)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.HD)) +
  geom_boxplot()

A1.HW <- aov(abs.res.HW ~ BASIN, data=lat3)
summary(A1.HW)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.HW)) +
  geom_boxplot()

A1.SnL <- aov(abs.res.SnL ~ BASIN, data=lat3)
summary(A1.SnL)

A1.OL <- aov(abs.res.OL ~ BASIN, data=lat3)
summary(A1.OL)

ggplot(lat3, aes(x=factor(BASIN), y=abs.res.OL)) +
  geom_boxplot()




```

##### Watersheds 

```{r}

A2.D <- aov(abs.res.D ~ WATERSHED, data=lat3)
summary(A2.D)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.D)) +
  geom_boxplot()

A2.P1 <- aov(abs.res.P1 ~ WATERSHED, data=lat3)
summary(A2.P1)

A2.P1.R <- aov(abs.res.P1.R ~ WATERSHED, data=lat3)
summary(A2.P1.R)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.P1.R)) +
  geom_boxplot()

A2.LLSC <- aov(abs.res.LLSC ~ WATERSHED, data=lat3)
summary(A2.LLSC)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.LLSC)) +
  geom_boxplot()



A2.SBLL <- aov(abs.res.SBLL ~ WATERSHED, data=lat3)
summary(A2.SBLL)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.SBLL)) +
  geom_boxplot()

A2.BD <- aov(abs.res.BD ~ WATERSHED, data=lat3)
summary(A2.BD)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.BD)) +
  geom_boxplot()

A2.CPD <- aov(abs.res.CPD ~ WATERSHED, data=lat3)
summary(A2.CPD)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.CPD)) +
  geom_boxplot()

A2.CPL <- aov(abs.res.CPL ~ WATERSHED, data=lat3)
summary(A2.CPL)

A2.PreDL <- aov(abs.res.PreDL ~ WATERSHED, data=lat3)
summary(A2.PreDL)

A2.DbL <- aov(abs.res.DbL ~ WATERSHED, data=lat3)
summary(A2.DbL)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.DbL)) +
  geom_boxplot()

A2.HL <- aov(abs.res.HL ~ WATERSHED, data=lat3)
summary(A2.HL)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.HL)) +
  geom_boxplot()

A2.HD <- aov(abs.res.HD ~ WATERSHED, data=lat3)
summary(A2.HD)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.HD)) +
  geom_boxplot()

A2.HW <- aov(abs.res.HW ~ WATERSHED, data=lat3)
summary(A2.HW)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.HW)) +
  geom_boxplot()

A2.SnL <- aov(abs.res.SnL ~ WATERSHED, data=lat3)
summary(A2.SnL)

A2.OL <- aov(abs.res.OL ~ WATERSHED, data=lat3)
summary(A2.OL)

ggplot(lat3, aes(x=factor(WATERSHED), y=abs.res.OL)) +
  geom_boxplot()




```

#### Both

##### Zones 

```{r}
library(ggplot2)

A.D <- aov(abs.res.D ~ SPP*QUARTILE, data=raw3)
summary(A.D)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.D, fill=SPP)) +
  geom_boxplot()


A.P1 <- aov(abs.res.P1 ~ SPP*QUARTILE, data=raw3)
summary(A.P1)

A.P1.R <- aov(abs.res.P1.R ~ SPP*QUARTILE, data=raw3)
summary(A.P1.R)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.P1.R, fill=SPP)) +
  geom_boxplot()


A.LLSC <- aov(abs.res.LLSC ~ SPP*QUARTILE, data=raw3)
summary(A.LLSC)


A.SBLL <- aov(abs.res.SBLL ~ SPP*QUARTILE, data=raw3)
summary(A.SBLL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.SBLL, fill=SPP)) +
  geom_boxplot()


A.BD <- aov(abs.res.BD ~ SPP*QUARTILE, data=raw3)
summary(A.BD)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.BD, fill=SPP)) +
  geom_boxplot()


A.CPD <- aov(abs.res.CPD ~ SPP*QUARTILE, data=raw3)
summary(A.CPD)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.CPD, fill=SPP)) +
  geom_boxplot()


A.CPL <- aov(abs.res.CPL ~ SPP*QUARTILE, data=raw3)
summary(A.CPL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.CPL, fill=SPP)) +
  geom_boxplot()


A.PreDL <- aov(abs.res.PreDL ~ SPP*QUARTILE, data=raw3)
summary(A.PreDL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.PreDL, fill=SPP)) +
  geom_boxplot()


A.DbL <- aov(abs.res.DbL ~ SPP*QUARTILE, data=raw3)
summary(A.DbL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.DbL, fill=SPP)) +
  geom_boxplot()


A.HL <- aov(abs.res.HL ~ SPP*QUARTILE, data=raw3)
summary(A.HL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.HL, fill=SPP)) +
  geom_boxplot()


A.HD <- aov(abs.res.HD ~ SPP*QUARTILE, data=raw3)
summary(A.HD)

A.HW <- aov(abs.res.HW ~ SPP*QUARTILE, data=raw3)
summary(A.HW)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.HW, fill=SPP)) +
  geom_boxplot()


A.SnL <- aov(abs.res.SnL ~ SPP*QUARTILE, data=raw3)
summary(A.SnL)

A.OL <- aov(abs.res.OL ~ SPP*QUARTILE, data=raw3)
summary(A.OL)

ggplot(raw3, aes(x=factor(QUARTILE), y=abs.res.OL, fill=SPP)) +
  geom_boxplot()


```

##### Basins 

```{r}

A1.D <- aov(abs.res.D ~ SPP*BASIN, data=raw3)
summary(A1.D)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.D, fill=SPP)) +
  geom_boxplot()


A1.P1 <- aov(abs.res.P1 ~ SPP*BASIN, data=raw3)
summary(A1.P1)

A1.P1.R <- aov(abs.res.P1.R ~ SPP*BASIN, data=raw3)
summary(A1.P1.R)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.P1.R, fill=SPP)) +
  geom_boxplot()

A1.LLSC <- aov(abs.res.LLSC ~ SPP*BASIN, data=raw3)
summary(A1.LLSC)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.LLSC, fill=SPP)) +
  geom_boxplot()



A1.SBLL <- aov(abs.res.SBLL ~ SPP*BASIN, data=raw3)
summary(A1.SBLL)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.SBLL, fill=SPP)) +
  geom_boxplot()

A1.BD <- aov(abs.res.BD ~ SPP*BASIN, data=raw3)
summary(A1.BD)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.BD, fill=SPP)) +
  geom_boxplot()

A1.CPD <- aov(abs.res.CPD ~ SPP*BASIN, data=raw3)
summary(A1.CPD)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.CPD, fill=SPP)) +
  geom_boxplot()

A1.CPL <- aov(abs.res.CPL ~ SPP*BASIN, data=raw3)
summary(A1.CPL)

A1.PreDL <- aov(abs.res.PreDL ~ SPP*BASIN, data=raw3)
summary(A1.PreDL)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.PreDL, fill=SPP)) +
  geom_boxplot()

A1.DbL <- aov(abs.res.DbL ~ SPP*BASIN, data=raw3)
summary(A1.DbL)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.DbL, fill=SPP)) +
  geom_boxplot()

A1.HL <- aov(abs.res.HL ~ SPP*BASIN, data=raw3)
summary(A1.HL)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.HL, fill=SPP)) +
  geom_boxplot()

A1.HD <- aov(abs.res.HD ~ SPP*BASIN, data=raw3)
summary(A1.HD)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.HD, fill=SPP)) +
  geom_boxplot()

A1.HW <- aov(abs.res.HW ~ SPP*BASIN, data=raw3)
summary(A1.HW)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.HW, fill=SPP)) +
  geom_boxplot()

A1.SnL <- aov(abs.res.SnL ~ SPP*BASIN, data=raw3)
summary(A1.SnL)

A1.OL <- aov(abs.res.OL ~ SPP*BASIN, data=raw3)
summary(A1.OL)

ggplot(raw3, aes(x=factor(BASIN), y=abs.res.OL, fill=SPP)) +
  geom_boxplot()




```

##### Watersheds 

```{r}

A2.D <- aov(abs.res.D ~ SPP*WATERSHED, data=raw3)
summary(A2.D)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.D, fill=SPP)) +
  geom_boxplot()

A2.P1 <- aov(abs.res.P1 ~ SPP*WATERSHED, data=raw3)
summary(A2.P1)

A2.P1.R <- aov(abs.res.P1.R ~ SPP*WATERSHED, data=raw3)
summary(A2.P1.R)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.P1.R, fill=SPP)) +
  geom_boxplot()

A2.LLSC <- aov(abs.res.LLSC ~ SPP*WATERSHED, data=raw3)
summary(A2.LLSC)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.LLSC, fill=SPP)) +
  geom_boxplot()



A2.SBLL <- aov(abs.res.SBLL ~ SPP*WATERSHED, data=raw3)
summary(A2.SBLL)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.SBLL, fill=SPP)) +
  geom_boxplot()

A2.BD <- aov(abs.res.BD ~ SPP*WATERSHED, data=raw3)
summary(A2.BD)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.BD, fill=SPP)) +
  geom_boxplot()

A2.CPD <- aov(abs.res.CPD ~ SPP*WATERSHED, data=raw3)
summary(A2.CPD)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.CPD, fill=SPP)) +
  geom_boxplot()

A2.CPL <- aov(abs.res.CPL ~ SPP*WATERSHED, data=raw3)
summary(A2.CPL)

A2.PreDL <- aov(abs.res.PreDL ~ SPP*WATERSHED, data=raw3)
summary(A2.PreDL)

A2.DbL <- aov(abs.res.DbL ~ SPP*WATERSHED, data=raw3)
summary(A2.DbL)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.DbL, fill=SPP)) +
  geom_boxplot()

A2.HL <- aov(abs.res.HL ~ SPP*WATERSHED, data=raw3)
summary(A2.HL)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.HL, fill=SPP)) +
  geom_boxplot()

A2.HD <- aov(abs.res.HD ~ SPP*WATERSHED, data=raw3)
summary(A2.HD)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.HD, fill=SPP)) +
  geom_boxplot()

A2.HW <- aov(abs.res.HW ~ SPP*WATERSHED, data=raw3)
summary(A2.HW)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.HW, fill=SPP)) +
  geom_boxplot()

A2.SnL <- aov(abs.res.SnL ~ SPP*WATERSHED, data=raw3)
summary(A2.SnL)

A2.OL <- aov(abs.res.OL ~ SPP*WATERSHED, data=raw3)
summary(A2.OL)

ggplot(raw3, aes(x=factor(WATERSHED), y=abs.res.OL, fill=SPP)) +
  geom_boxplot()




```

### Standardized

DON'T KNOW IF THIS IS USEFUL

The ANOVAs above focus on differences of particular traits as a factor of species and location. If we want to get an idea of variation in general as a factor of species and location, we can standardize the residuals (essentially unitless z-scores of residuals).

```{r}

sd.res.D <- append(abs(sd.lat.D), abs(sd.form.D))
sd.res.P1 <- append(abs(sd.lat.P1), abs(sd.form.P1))
sd.res.P1.R <- append(abs(sd.lat.P1.R), abs(sd.form.P1.R))
sd.res.LLSC<- append(abs(sd.lat.LLSC), abs(sd.form.LLSC))
sd.res.SBLL<- append(abs(sd.lat.SBLL), abs(sd.form.SBLL))
sd.res.BD<- append(abs(sd.lat.BD), abs(sd.form.BD))
sd.res.CPD<- append(abs(sd.lat.CPD), abs(sd.form.CPD))
sd.res.CPL<- append(abs(sd.lat.CPL), abs(sd.form.CPL))
sd.res.PreDL <- append(abs(sd.lat.PreDL), abs(sd.form.PreDL))
sd.res.DbL <- append(abs(sd.lat.DbL), abs(sd.form.DbL))
sd.res.HL<- append(abs(sd.lat.HL), abs(sd.form.HL))
sd.res.HD<- append(abs(sd.lat.HD), abs(sd.form.HD))
sd.res.HW <- append(abs(sd.lat.HW), abs(sd.form.HW))
sd.res.SnL <- append(abs(sd.lat.SnL), abs(sd.form.SnL))
sd.res.OL <- append(abs(sd.lat.OL), abs(sd.form.OL))


raw4 <- cbind(raw3, sd.res.D, sd.res.P1, sd.res.P1.R, sd.res.LLSC, sd.res.SBLL, sd.res.BD, sd.res.CPD, sd.res.CPL, sd.res.PreDL, sd.res.DbL, sd.res.HL, sd.res.HD, sd.res.HW, sd.res.SnL, sd.res.OL)

raw5 <- cbind(raw4[1:14], stack(raw4[53:68])) 

lat.raw5 <- raw5[raw5$SPP == "p.latipinna",]

form.raw5 <- raw5[raw5$SPP == "p.formosa",]

######ZONES#####

A3.lat <- aov(values~QUARTILE, data=lat.raw5)
summary(A3.lat)

A3.form <- aov(values~QUARTILE, data=form.raw5)
summary(A3.form)

#between species

A3 <- aov(values~QUARTILE*SPP, data=raw5)
summary(A3)

ggplot(raw5, aes(x=factor(QUARTILE), y=values, fill=SPP)) +
  geom_boxplot()

######BASINS#####

A4.lat <- aov(values~BASIN, data=lat.raw5)
summary(A4.lat)

A4.form <- aov(values~BASIN, data=form.raw5)
summary(A4.form)

#between species

A4 <- aov(values~BASIN*SPP, data=raw5)
summary(A4)

ggplot(raw5, aes(x=factor(BASIN), y=values, fill=SPP)) +
  geom_boxplot()

#####WATERSHEDS#####
A5.lat <- aov(values~WATERSHED, data=lat.raw5)
summary(A5.lat)

A5.form <- aov(values~WATERSHED, data=form.raw5)
summary(A5.form)

#between species

A5 <- aov(values~WATERSHED*SPP, data=raw5)
summary(A5)

ggplot(raw5, aes(x=factor(WATERSHED), y=values, fill=SPP)) +
  geom_boxplot()



```



## PCA analysis 

### PCA {.tabset .tabset-fade .tabset-pills}

LOGAN: CHECK THAT EACH VARIABLES IS NEAR NORMALLY DISTRIBUTED. IF NOT, LOG TRANSFORM BEFORE PCA. ALSO CHECK THAT PCA CALCULATES Z SCORES AND PLOTS BASED ON THAT; IF NOT CONVERT TO Z SCORES THEN RUN PCA.

In this analysis, I will compare the principle components after centering and scaling the data. A PCA analysis will help us determine what aspects of morphology influence the variation in our data the most without worrying about differences in scales/measurements. Currently, data consists of **116 Sailfin and 186 Amazon**.

#### Chart

```{r}
(chart <- matrix(c("Variable chart:", "D: dorsal ray count", "P1: left pectoral ray count", "P2.R: right pelvic rays", "P2.L: left pelvic rays", "A: anal rays", "P1.R: right pectoral ray count", "LLSC: lateral line scale count", "SALL: scales above lateral line", "SBLL: scales below lateral line", "SBDF: scales before dorsal fin", "TL: total length", "SL: standard length", "BD: body depth", "CPD: caudal peduncle depth", "CPL: caudal peduncle length", "PreDL: pre-dorsal length", "DbL: dorsal base length", "HL/HW/HD: head length/width/depth", "SnL: snout length", "OL: ocular length"), ncol=1, byrow=TRUE))
```

#### Loadings

```{r}

raw1a <- subset(raw1, select = -c(16:18) )

PCA <- prcomp(raw1a[, 15:32], center=TRUE, scale. = TRUE) #includes all but pelvic traits, since they are the same in all species
summary(PCA)

(loadings <- PCA$rotation[, 1:5])

sorted.loadings.1 <- loadings[order(loadings[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings.1, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings.2 <- loadings[order(loadings[, 2]), 2]
myTitle <- "Loadings Plot for PC2" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings.2, main=myTitle, xlab=myXlab, cex=1.5, col="red")

sorted.loadings.3 <- loadings[order(loadings[, 3]), 3]
myTitle <- "Loadings Plot for PC3" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings.3, main=myTitle, xlab=myXlab, cex=1.5, col="red")


VM_PCA <- varimax(PCA$rotation) 

VM_PCA

```


#### Plots

##### Component extractions

```{r}

library(AMR) 
library(ggplot2)
library(ggfortify)

(PCA$sdev ^ 2)

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}


ev <- PCA$sdev^2 
evplot(ev) #according to Kaiser-Guttman criteron, we can use the first 4 PCs, even though the broken stick model shows only the first above the red bar plot... not 100% confident I know what this means, but pretty sure PC1 is body size


```


##### PCA 1v2


```{r}

plot1<- autoplot(PCA, data = raw1, colour='SPP', loadings=FALSE, loadings.label=FALSE, frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot1

```

###### By zone

```{r}

plot2<- autoplot(PCA, data = raw1, colour='QUARTILE', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot2

```


###### By Basin

```{r}

plot3<- autoplot(PCA, data = raw1, colour='BASIN', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot3

```


###### By watershed

```{r}
plot4<- autoplot(PCA, data = raw1, colour='WATERSHED', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot4

```

##### PCA 2v3

```{r}

plot5<- autoplot(PCA, x=2, y=3, data = raw1, colour='SPP', loadings=FALSE, loadings.label=FALSE, frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal()
plot5

```


###### By zone

```{r}

plot6<- autoplot(PCA, x=2, y=3, data = raw1, colour='QUARTILE', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot6

```


###### By basin

```{r}

plot7<- autoplot(PCA, x=2, y=3, data = raw1, colour='BASIN', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot7

```


###### By watershed

```{r}

plot8<- autoplot(PCA, x=2, y=3, data = raw1, colour='WATERSHED', shape="SPP", frame=TRUE, frame.type='norm')+ ggtitle("PCA Plot of Morphology traits") + theme_minimal() 
plot8

```

