#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                     ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Code for GSERM "Regression for Publishing"
# (June 2024)
#
# Day Two
#
# File last modified 6/17/2023
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Set working directory first:
#
# setwd("~/Dropbox (Personal)/GSERM/RFP2023/Slides") # or whatever...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","car","psych","lattice","arm","plotMElm",
     "stargazer","modelsummary","marginaleffects")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 7-8 times until you get all smileys. :)
#
# Gelman's -standardize- function:

url_std <- "http://www.stat.columbia.edu/~gelman/standardize/standardize.R"
eval(parse(text = getURL(url_std, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_std)

# Set R options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Multiplicative interactions...            ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Simulations for pictures....
#
# Two dummy predictors:

set.seed(7222009)
N<-400
D1<-rep(c(0,1),times=N/2)
D2<-rep(c(0,0,1,1),times=N/4)
Y <- rnorm(N,(20-10*D2+10*D1+20*D1*D2),2)
df<-data.frame(D1=D1,D2=D2,Y=Y)

pdf("TwoDummyBoxPlotsRD1-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df, boxplot(Y~D2+D1,xaxt="n",xlab="Values of D1,D2"))
axis(1,at=c(1,2,3,4),
     labels=c("D1=0, D2=0","D1=0, D2=1",
              "D1=1, D2=0","D1=1, D2=1"))
arrows(1,median(df$Y[which(df$D1==0 & df$D2==0)]),
       3,median(df$Y[which(df$D1==1 & df$D2==0)]),
       lwd=2,length=0.10,col="red")
arrows(2,median(df$Y[which(df$D1==0 & df$D2==1)]),
       4,median(df$Y[which(df$D1==1 & df$D2==1)]),
       lwd=2,length=0.10,col="red")
legend("topleft",bty="n",legend="E(Y) | change in D1",col="red")
dev.off()

pdf("TwoDummyBoxPlotsRD2-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df, boxplot(Y~D2+D1,xaxt="n",xlab="Values of D1,D2"))
axis(1,at=c(1,2,3,4),
     labels=c("D1=0, D2=0","D1=0, D2=1",
              "D1=1, D2=0","D1=1, D2=1"))
arrows(1,median(df$Y[which(df$D1==0 & df$D2==0)]),
       2,median(df$Y[which(df$D1==0 & df$D2==1)]),
       lwd=2,length=0.10)
arrows(3,median(df$Y[which(df$D1==1 & df$D2==0)]),
       4,median(df$Y[which(df$D1==1 & df$D2==1)]),
       lwd=2,length=0.10)
legend("topleft",bty="n",legend="E(Y) | change in D2")
dev.off()

# Dummy + continuous:

set.seed(7222009)
N<-200
D<-rep(c(0,1),times=N/2)
X<-rnorm(N,0,5)
color<-ifelse(D==0,"black","red")
df2<-data.frame(D=D,X=X,color=color,
                stringsAsFactors=FALSE)

df2$Y1 <- 50+2*df2$X+3*rnorm(N)
df2$Y2 <- 50+2*df2$X+30*df2$D+3*rnorm(N)
df2$Y3 <- 50+2*df2$X-(4*df2$D*df2$X)+3*rnorm(N)
df2$Y4 <- 50+2*df2$X+30*df2$D-(4*df2$D*df2$X)+3*rnorm(N)

pdf("ScatterInterSameR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y1,pch=D+16,col=color,ylab="Y"))
legend("topleft",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y1~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y1~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterInterceptR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y2,pch=D+16,col=color,ylab="Y"))
legend("topleft",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y2~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y2~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterSlopeR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y3,pch=D+16,col=color,ylab="Y"))
legend("topright",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y3~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y3~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

pdf("ScatterInterBothR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df2, plot(X,Y4,pch=D+16,col=color,ylab="Y"))
legend("topright",bty="n",legend=c("D=0","D=1"),
       pch=c(16,17),col=c("black","red"))
abline(with(df2[df2$D==0,],lm(Y4~X)),col="black",lwd=2)
abline(with(df2[df2$D==1,],lm(Y4~X)),col="red",lwd=2)
abline(v=0,lty=2)
dev.off()

# Two continuous: Wireframe plots...

df3<-expand.grid(X1=seq(0,10,1),
                 X2=seq(0,10,1))
df3$YNoInt<-10 + 2*df3$X1 + 2*df3$X2
df3$YInt  <-(10 - 2*df3$X1 - 2*df3$X2 + 4*df3$X1*df3$X2)/5

trellis.par.set("axis.line",list(col="transparent"))

pdf("TwoContinuousNoInteractiveR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df3, wireframe(YNoInt~X1+X2,
                    drape=TRUE,
                    xlab=list("X1",rot=30),
                    ylab=list("X2",rot=-40),
                    zlab=list("Y",rot=90),
                    scales=list(arrows=FALSE,col="black"),
                    zoom=0.85,pretty=TRUE,
                    col.regions=colorRampPalette(c("blue","red"))(100)))
dev.off()

pdf("TwoContinuousInteractiveR-24.pdf",6,6)
par(mar=c(4,4,2,2))
with(df3, wireframe(YInt~X1+X2,
                    drape=TRUE,
                    xlab=list("X1",rot=30),
                    ylab=list("X2",rot=-40),
                    zlab=list("Y",rot=90),
                    scales=list(arrows=FALSE,col="black"),
                    zoom=0.85,pretty=TRUE,
                    col.regions=colorRampPalette(c("blue","red"))(100)))
dev.off()

# Polynomials...

N<-200
set.seed(7222009)
df4 <- data.frame(X = runif(N,-5,5))
df4$Y2A <- 10 + 1*df4$X - 5*(df4$X^2) + rnorm(N,0,10) # Quad #1
df4$Y2B <- -50 - 1*df4$X + 3*(df4$X^2) + rnorm(N,0,10) # Quad #2
df4$Y3 <- -8 - 6*df4$X + 3*(df4$X^2) + 1*(df4$X^3) + rnorm(N,0,10) # Cubic
df4 <- df4[order(df4$X),]
fitA<-with(df4, lm(Y2A~X+I(X^2)))
fitB<-with(df4, lm(Y2B~X+I(X^2)))
fit3<-with(df4, lm(Y3~X+I(X^2)+I(X^3)))

pdf("TwoQuadraticsR-24.pdf",7,6)
par(mar=c(4,4,2,2))
with(df4, plot(X,Y2B,pch=16,col="black",ylab="Y",
               ylim=c(min(df4$Y2B),max(df4$Y2A))))
points(df4$X,df4$Y2A,pch=17,col="red")
lines(df4$X,fitted(fitA),lwd=3,col="red")
lines(df4$X,fitted(fitB),lwd=3,col="black")
dev.off()

pdf("CubicR-24.pdf",7,6)
par(mar=c(4,4,2,2))
with(df4, plot(X,Y3,pch=16,col="black",ylab="Y"))
lines(df4$X,fitted(fit3),lwd=3,col="black")
dev.off()

# Three-way interaction sim:

N <- 100
X <- runif(N,-5,5)
df00<-data.frame(X=X)
df01<-data.frame(X=X)
df10<-data.frame(X=X)
df11<-data.frame(X=X)
set.seed(7222009)
df00$Y<-0.5*df00$X+rnorm(N)
df01$Y<-2*df01$X+rnorm(N)
df10$Y<- -0.5*df10$X+rnorm(N)
df11$Y<- -2*df11$X+rnorm(N)
fit00<-lm(Y~X,data=df00)
fit01<-lm(Y~X,data=df01)
fit10<-lm(Y~X,data=df10)
fit11<-lm(Y~X,data=df11)
hi<-12
lo<- -12

pdf("TwoDummyOneContinuousR-24.pdf",7,6)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
with(df00, plot(X,Y,main="D1=0,D2=0",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit00,lwd=2)
with(df01, plot(X,Y,main="D1=0,D2=1",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit01,lwd=2)
with(df10, plot(X,Y,main="D1=1,D2=0",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit10,lwd=2)
with(df11, plot(X,Y,main="D1=1,D2=1",ylab="Y",
                ylim=c(lo,hi),pch=20))
abline(h=0,lty=2)
abline(reg=fit11,lwd=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Clinton feeling thermometer score example...      ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

CT<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/ClintonTherm.csv")

# Clinton feeling thermometer ("FT") density plot:

pdf("ClintonThermHistogram-24.pdf",7,5)
par(mar=c(4,4,2,2))
hist(CT$ClintonTherm,main="",
     xlab="Clinton Feeling Thermometer")
abline(v=mean(CT$ClintonTherm),lty=2,col="black")
text(mean(CT$ClintonTherm),225,pos=2,cex=0.9,
          labels=paste0("Mean = ",round(mean(CT$ClintonTherm),2)))
dev.off()

# Summary statistics:

describe(ClintonTherm,skew=FALSE)

# Basic regression:

fit0<-lm(ClintonTherm~RConserv+GOP,data=CT)
summary(fit0)

# Coefficient / ladder plot:

pdf("ClintonCoefPlot-24.pdf",7,6)
par(mar=c(2,14,2,2))
coefplot(fit0,main="Estimated Coefficient",cex.var=1.1,
         varnames=c("(Intercept)",
                    "Respondent's Conservatism","GOP"),
         xlim=c(-30,5))
dev.off()

# Model w/interaction:

fit1<-lm(ClintonTherm~RConserv+GOP+RConserv*GOP,data=CT)
summary(fit1)

pdf("ClintonInterCoefPlot-24.pdf",7,6)
par(mar=c(2,16,2,2))
coefplot(fit1,main="Estimated Coefficient",cex.var=1.1,
         varnames=c("(Intercept)",
                    "Respondent's Conservatism","GOP",
                    "Respondent's Conservatism x GOP"))
dev.off()

# Various interactive models -- these are all the same
# mathematically, but will be handled by R somewhat
# differently:

summary(lm(ClintonTherm~RConserv+GOP+RConserv*GOP,data=CT))
summary(lm(ClintonTherm~RConserv+GOP+RConserv:GOP,data=CT))
summary(lm(ClintonTherm~RConserv*GOP,data=CT))
summary(lm(ClintonTherm~(RConserv+GOP)^2,data=CT))
CT$GOPxRC<-CT$GOP * CT$RConserv
summary(lm(ClintonTherm~RConserv+GOP+GOPxRC,data=CT))


# Plot of thermometer scores vs. conservatism:

pdf("ClinThermScatterR-24.pdf",6,6)
scatterplot(CT$ClintonTherm~CT$RConserv|as.factor(CT$GOP),
            legend=FALSE,
            xlab="Respondent Conservatism",
            ylab="Clinton Thermometer Score",
            smooth=FALSE,boxplots=FALSE,
            pch=c(4,16),col=c("blue","red","blue","red"),
            lwd=2,grid=FALSE)
dev.off()

# Separate regressions, by value of GOP:

NonReps<-subset(CT,GOP==0)
Reps<-subset(CT,GOP==1)
split<-list("Interactive" = lm(ClintonTherm~RConserv*GOP,data=CT),
            "Non-GOP" = lm(ClintonTherm~RConserv,data=NonReps),
            "GOP" = lm(ClintonTherm~RConserv,data=Reps))

# Table:

modelsummary(split,output="SplitModels-24.tex",
             gof_omit="DF|Deviance|Log.Lik.|AIC|BIC")

# psi_1:
Psi1<-fit1$coeff[2]+fit1$coeff[4]
Psi1
SPsi1<-sqrt(vcov(fit1)[2,2] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[2,4])
SPsi1
Psi1 / SPsi1 # <-- t-statistic

# psi_2 | RConserv = 1

fit1$coeff[3]+(1 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (1)^2*vcov(fit1)[4,4] + 2*1*vcov(fit1)[3,4])

# Implies t is approximately 2


# psi_2 | RConserv = 7

fit1$coeff[3]+(7 * fit1$coeff[4])

sqrt(vcov(fit1)[3,3] + (7)^2*vcov(fit1)[4,4] + 2*7*vcov(fit1)[3,4])

# t is approximately 11

# Using linearHypothesis:

linearHypothesis(fit1,"RConserv+RConserv:GOP")

# Note: Same as t-test:
sqrt(73)

# psi_2 | RConserv = 7:

linearHypothesis(fit1,"GOP+7*RConserv:GOP")

# MFX / psi plots:

ConsSim<-seq(1,7,1)
psis<-fit1$coeff[3]+(ConsSim * fit1$coeff[4])
psis.ses<-sqrt(vcov(fit1)[3,3] + 
                 (ConsSim)^2*vcov(fit1)[4,4] + 2*ConsSim*vcov(fit1)[3,4])

pdf("ClinMFX1-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(ConsSim,psis,t="l",lwd=2,xlab="Respondent Conservatism",
     ylab="Estimated Marginal Effect of GOP",ylim=c(-40,0))
lines(ConsSim,psis+(1.96*psis.ses),lty=2,lwd=2)
lines(ConsSim,psis-(1.96*psis.ses),lty=2,lwd=2)
abline(h=0,lwd=1,lty=2)
dev.off()

# Same thing, using plot_me:

pdf("ClinMFX1Alt-24.pdf",7,6)
plot_me(fit1,"GOP","RConserv",ci_type="fdr")
dev.off()

# Interacting two continuous covariates:

fit2<-lm(ClintonTherm~RConserv+ClintonConserv+RConserv*ClintonConserv,data=CT)
summary(fit2)

# Hypothesis tests:

psi<-fit2$coef[2]+(1*fit2$coef[4])
psi
varpsi<-sqrt(vcov(fit2)[2,2] + (1)^2*vcov(fit2)[4,4] + 2*1*vcov(fit2)[2,4])
varpsi
psi/varpsi # t-test

linearHypothesis(fit2,"RConserv+1*RConserv:ClintonConserv")
(psi/varpsi)^2

# # More hypothesis tests:
# 
# # psi_1 | ClintonConserv = mean
# fit2$coef[2]+((mean(ClintonTherm$ClintonConserv))*fit2$coef[4])
# sqrt(vcov(fit2)[2,2] + (mean(ClintonTherm$ClintonConserv)^2*vcov(fit2)[4,4] +
#                           2*(mean(ClintonTherm$ClintonConserv))*vcov(fit2)[2,4]))
# pt(((fit2$coef[2]+(2.985*fit2$coef[4])) / sqrt(vcov(fit2)[2,2] + 
#                                                  (2.985)^2*vcov(fit2)[4,4] + 2*2.985*vcov(fit2)[2,4])),df=1293)
# 
# # psi_2 | RConserv = 1
# fit2$coef[3]+(1*fit2$coef[4])
# 
# # psi_2 | RConserv = 6
# fit2$coef[3]+(6*fit2$coef[4])

# Marginal Effect Plot II:

psis2<-fit2$coef[3]+(ConsSim*fit2$coef[4])
psis2.ses<-sqrt(vcov(fit2)[3,3] + (ConsSim)^2*vcov(fit2)[4,4]
                + 2*ConsSim*vcov(fit2)[3,4])

pdf("ClinMFX2-24.pdf",6,6)
plot(ConsSim,psis2,t="l",lwd=2,xlab="Respondent's Conservatism",
     ylab="Marginal Effect of Clinton's Conservatism",ylim=c(-10,20))
lines(ConsSim,psis2+(1.96*psis2.ses),lty=2,lwd=2)
lines(ConsSim,psis2-(1.96*psis2.ses),lty=2,lwd=2)
abline(h=0,lty=2,lwd=1,col="red")
dev.off()

# Same thing, using plot_me:

pdf("ClinMFX2Alt-24.pdf",7,6)
plot_me(fit2,"ClintonConserv","RConserv",ci_type="fdr")
dev.off()

# Contour Plot:

grid<-expand.grid(RConserv=seq(1,7,1),
                  ClintonConserv=seq(1,7,1))
hats<-predict(fit2,newdata=grid)

pdf("ClinContour-24.pdf",6,6)
levelplot(hats~grid$RConserv*grid$ClintonConserv,
          contour=TRUE,cuts=12,pretty=TRUE,
          xlab="Respondent's Conservatism",
          ylab="Clinton's Conservatism",
          col.regions=heat.colors)
dev.off()

# Wireframe plot:

trellis.par.set("axis.line",list(col="transparent"))

pdf("ClinWireframe-24.pdf",7,7)
wireframe(hats~grid$RConserv*grid$ClintonConserv,
          drape=TRUE,
          xlab=list("Respondent's Conservatism",rot=30),
          ylab=list("Clinton's Conservatism",
                    rot=-40),zlab=list("Predictions",rot=90),
          scales=list(arrows=FALSE,col="black"),
          zoom=0.85,pretty=TRUE,
          col.regions=colorRampPalette(c("blue","red"))(100))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Nonlinearity...                                   ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Bulging plot":

N<-500
r<-50
set.seed(7222009)
X<-runif(N,-r,r)
TL<-sqrt(r^2-X^2)+rnorm(N)
BL<- -sqrt(r^2-X^2)+rnorm(N)

pdf("BetterBulgingPlot-24.pdf",10,8)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
plot(X[X<0],TL[X<0],pch=20,xlab="X",ylab="Y",
     cex.axis=0.01)
legend("bottomright",bty="n",legend="Log X and/or Square Y")
plot(X[X>0],TL[X>0],pch=20,xlab="X",ylab="Y",
     cex.axis=0.01)
legend("bottomleft",bty="n",legend="Square X and/or Y")
plot(X[X<0],BL[X<0],pch=20,xlab="X",ylab="Y",
     cex.axis=0.01)
legend("topright",bty="n",legend="Log X and/or Y")
plot(X[X>0],BL[X>0],pch=20,xlab="X",ylab="Y",
     cex.axis=0.01)
legend("topleft",bty="n",legend="Square X and/or Log Y")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Real-data example:                          ####

WDI<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/WDI-2018-Day-Two-24.csv")

describe(WDI,fast=TRUE,skew=TRUE)

# Density plots:

pdf("PhoneWealthDensities-24.pdf",9,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(WDI$MobileCellSubscriptions),main="",
     xlab="Mobile Phone Subscriptions (per 100 people)")
plot(density(WDI$GDPPerCapita),main="",
     xlab="GDP Per Capita (Constant $US)")
dev.off()

# Ladder Plots (can also use -ladder- in the HH package):
#
# Mobile subscriptions:

pdf("LadderOfPowersDensitiesPhones-24.pdf",7,6)
par(mar=c(4,4,2,2))
par(mfrow=c(3,3))
with(WDI, plot(density(MobileCellSubscriptions^3,na.rm=TRUE),
               main="Cubic",xlab=""))
with(WDI, plot(density(MobileCellSubscriptions^2,na.rm=TRUE),
               main="Square",xlab=""))
with(WDI, plot(density(MobileCellSubscriptions,na.rm=TRUE),
               main="Identity",xlab=""))
with(WDI, plot(density(sqrt(MobileCellSubscriptions),na.rm=TRUE),
               main="Square Root",xlab=""))
with(WDI, plot(density(log(MobileCellSubscriptions),na.rm=TRUE),
               main="Log",xlab=""))
with(WDI, plot(density(1/sqrt(MobileCellSubscriptions),na.rm=TRUE),
               main="1 / Square Root",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions,na.rm=TRUE),
               main="Inverse",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions^2,na.rm=TRUE),
               main="1 / Square",xlab=""))
with(WDI, plot(density(1/MobileCellSubscriptions^3,na.rm=TRUE),
               main="1 / Cubic",xlab=""))
dev.off()

# Same for GDP Per Capita:

pdf("LadderOfPowersDensitiesGDP-24.pdf",7,6)
par(mar=c(4,4,2,2))
par(mfrow=c(3,3))
with(WDI, plot(density(GDPPerCapita^3,na.rm=TRUE),main="Cubic",xlab=""))
with(WDI, plot(density(GDPPerCapita^2,na.rm=TRUE),main="Square",xlab=""))
with(WDI, plot(density(GDPPerCapita,na.rm=TRUE),main="Identity",xlab=""))
with(WDI, plot(density(sqrt(GDPPerCapita),na.rm=TRUE),main="Square Root",xlab=""))
with(WDI, plot(density(log(GDPPerCapita),na.rm=TRUE),main="Log",xlab=""))
with(WDI, plot(density(1/sqrt(GDPPerCapita),na.rm=TRUE),main="1 / Square Root",
               xlab=""))
with(WDI, plot(density(1/GDPPerCapita,na.rm=TRUE),main="Inverse",xlab=""))
with(WDI, plot(density(1/GDPPerCapita^2,na.rm=TRUE),main="1 / Square",xlab=""))
with(WDI, plot(density(1/GDPPerCapita^3,na.rm=TRUE),main="1 / Cubic",xlab=""))
dev.off()

# Scatterplots:

dot<-20 # symbol for -pch()-

pdf("WealthPhoneScatters-24.pdf",7,6)
par(mar=c(4,4,4,2))
par(mfrow=c(2,2))
with(WDI, plot(GDPPerCapita,MobileCellSubscriptions,pch=dot,
               main="Linear-Linear"))
with(WDI, plot(log(GDPPerCapita),MobileCellSubscriptions,pch=dot,
               main="Linear-Log"))
with(WDI, plot(GDPPerCapita,log(MobileCellSubscriptions),pch=dot,
               main="Log-Linear"))
with(WDI, plot(log(GDPPerCapita),log(MobileCellSubscriptions),pch=dot,
               main="Log-Log"))
dev.off()

# Regressions:

transformz<-list("Linear Y & X"=lm(MobileCellSubscriptions~I(GDPPerCapita/1000),data=WDI),
"Linear Y, Log X"=lm(MobileCellSubscriptions~log(GDPPerCapita/1000),data=WDI),
"Log Y, Linear X"=lm(log(MobileCellSubscriptions)~I(GDPPerCapita/1000),data=WDI),
"Log Y and X"=lm(log(MobileCellSubscriptions)~log(GDPPerCapita/1000),data=WDI))

# Make a table:

modelsummary(transformz,output="TransRegs-24.tex",
             gof_omit="DF|Deviance|Log.Lik.|AIC|BIC",
             coef_rename=c("I(GDPPerCapita/1000)"="GDP Per Capita (1000s)",
                           "log(GDPPerCapita/1000)"="Logged GDP Per Capita (1000s)"))



# Residual plot...
#
# Refit regresions:

linlin<-lm(MobileCellSubscriptions~I(GDPPerCapita/1000),data=WDI)
linlog<-lm(MobileCellSubscriptions~log(GDPPerCapita/1000),data=WDI)
loglin<-lm(log(MobileCellSubscriptions)~I(GDPPerCapita/1000),data=WDI)
loglog<-lm(log(MobileCellSubscriptions)~log(GDPPerCapita/1000),data=WDI)

# Plot:


pdf("PhoneResidsDensities-24.pdf",7,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(density(linlin$residuals),xlim=c(-80,80),ylim=c(0,0.017),
     lwd=2,lty=1,col="black",main="Untransformed Y",
     xlab="Residual Values")
lines(density(linlog$residuals),lwd=2,lty=2,col="orange")
abline(v=0,lwd=1,lty=2)
legend("topright",bty="n",lwd=2,lty=c(1,2),col=c("black","orange"),
       legend=c("Untransformed X","Transformed X"),cex=0.6)
plot(density(loglin$residuals),xlim=c(-2,2),ylim=c(0,1.7),
     lwd=2,lty=1,col="black",main="Transformed Y",
     xlab="Residual Values")
lines(density(loglog$residuals),lwd=3,lty=4,col="orange")
abline(v=0,lty=2,lwd=1)
legend("topright",bty="n",lwd=2,lty=c(1,2),col=c("black","orange"),
       legend=c("Untransformed X","Transformed X"),cex=0.6)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Polynomial Things...                          ####
#
# Generate some data on X and Y where the association
# is non-monotonic (here, using a sine function):

N<-500
set.seed(7222009)
X<-runif(N,3,17)
Y<-8+2*sin(X)+rnorm(N)

# Initial scatterplot:

par(mfrow=c(1,1))

pdf("PolynomialScatter-24.pdf",6,5)
par(mar=c(4,4,4,2))
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),
     main="Y = 8+2[sin(X)]+u")
dev.off()

# (Raw) polynomial fits ("by hand"):

R.1<-lm(Y~X)
R.2<-lm(Y~X+I(X^2))
R.3<-lm(Y~X+I(X^2)+I(X^3))
R.4<-lm(Y~X+I(X^2)+I(X^3)+I(X^4))
R.5<-lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5))
R.6<-lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6))
R.12<-lm(Y~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+
           I(X^7)+I(X^8)+I(X^9)+I(X^10)+I(X^11)+I(X^12))

# Plots:

x <- seq(0,20,length.out=500)

pdf("RawPolynomialFits-24.pdf",8,6)
par(mfrow=c(2,3))
par(mar=c(4,4,4,2))
# P = 1
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Linear (P=1)")
lines(x,predict(R.1,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.1)$adj.r.squared,2)))
# P = 2
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Quadratic (P=2)")
lines(x,predict(R.2,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.2)$adj.r.squared,2)))
# # P = 3
# plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Cubic (P=3)")
# lines(x,predict(R.3,data.frame(X=x)),col="orange",lwd=2)
# text(5,15,cex=0.8,
#      paste0("Adj. R-Squared = ",round(summary(R.3)$adj.r.squared,2)))
# P = 4
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Fourth-Degree (P=4)")
lines(x,predict(R.4,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.4)$adj.r.squared,2)))
# P = 5
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Fifth-Degree (P=5)")
lines(x,predict(R.5,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.5)$adj.r.squared,2)))
# P = 6
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Sixth-Degree (P=6)")
lines(x,predict(R.6,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.6)$adj.r.squared,2)))
# P = 12
plot(X,Y,pch=20,xlim=c(0,20),ylim=c(0,16),main="Twelfth-Degree (P=12)")
lines(x,predict(R.12,data.frame(X=x)),col="orange",lwd=2)
text(5,15,cex=0.8,
     paste0("Adj. R-Squared = ",round(summary(R.12)$adj.r.squared,2)))
dev.off()

# Raw vs. orthogonal polynomials, illustrated:

summary(R.12)

P.12R<-lm(Y~poly(X,degree=12,raw=TRUE))
summary(P.12R)

P.12<-lm(Y~poly(X,degree=12))
summary(P.12)

# Orthogonal polynomial fits (P=1 to P=12):

for(degree in 1:12) {
  fit <- lm(Y~poly(X,degree))
  assign(paste("P", degree, sep = "."), fit)
}

# Compare using ANOVA:

anova(P.1,P.2,P.3,P.4,P.5,P.6,P.7,P.8,P.9,P.10,P.11,P.12)

# Gather/plot adjusted R-squareds:

ARSq<-numeric(12)
for(j in 1:12){
  fit <- lm(Y~poly(X,j))
  ARSq[j]<-summary(fit)$adj.r.squared
}

pdf("PolynomialAdjRSqPlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(seq(1:12),ARSq,t="l",xlab="Polynomial Degree",
     ylab="Adj. R-Squared",xlim=c(0,12))
points(seq(1:12),ARSq,pch=19)
dev.off()


# \fin
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=####