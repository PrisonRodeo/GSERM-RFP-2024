#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                     #####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Code GSERM "Regression for Publishing"
#
# June 2024
#
# Day Three: various things...
#
# File last modified 6/19/2024
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# setwd() first:
#
# setwd("~/Dropbox (Personal)/GSERM/Materials 2024") # or whatever...
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","car","psych","plyr","rms","plm","lmtest","aod",
     "gvlma","dplyr","gmodels","margins","ROCR","pROC",
     "RCurl","stargazer","modelsummary","marginaleffects",
     "ggplot2","MASS","dplyr","glmnet","sandwich","xtable")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 7-8 times until you get all smileys. :)
#
# Handy "robust" summary function for lm:

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
rm(url_robust)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Outliers and things...                            ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Flintstones" (made-up) data:

Flintstones<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/flintstones.csv")

fit1<-lm(Y~X,data=Flintstones[Flintstones$name!="Barney" & 
                              Flintstones$name!="Dino",])
fit2<-lm(Y~X,data=Flintstones[Flintstones$name!="Dino",])
fit3<-lm(Y~X,data=Flintstones[Flintstones$name!="Barney",])

pdf("Flintstones-24.pdf",7,5)
par(mar=c(4,4,2,2))
with(Flintstones,plot(X,Y,pch=20,xlim=c(4,30),
                      ylim=c(50,350)))
with(Flintstones,text(X,Y,pos=3,labels=Flintstones$name,cex=0.8))
abline(fit1,lwd=2)
abline(fit2,lwd=2,lty=2)
abline(fit3,lwd=2,lty=4,col="red")
dev.off()

# No Barney OR Dino:
summary(lm(Y~X,data=subset(flintstones,name!="Dino" & name!="Barney")))

# No Barney (Dino included):
summary(lm(Y~X,data=subset(flintstones,name!="Barney")))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dahl data (SCOTUS things):

NewDahl<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/Dahl2021.csv")
row.names(NewDahl)<-NewDahl$Year
psych::describe(NewDahl,fast=TRUE,skew=TRUE)

# Scatterplot matrix:

pdf("NewDahlScatterplotMatrix-24.pdf",10,8)
with(NewDahl, scatterplotMatrix(~Year+NNulls+Age+Tenure+Unified,
                                pch=19,col="black",
                                regLine=list(col="red"),
                                smooth=list(spread=FALSE)))
dev.off()

# Regression:

Fit<-lm(NNulls~Age+Tenure+Unified,data=NewDahl)
summary(Fit)

# Over-time plot:

pdf("NewDahlOverTime-24.pdf",7,6)
par(mar=c(4,4,2,4))
with(NewDahl,plot(Year,NNulls,t="l",lwd=2,ylab="Nullifications"))
par(new=TRUE)
with(NewDahl,plot(Year,Age,t="l",lwd=2,lty=5,col="red",yaxt="n",
                  ylab="",ylim=c(45,72)))
axis(4,xlim=c(45,72),col="red",col.ticks="red",col.axis="red")
mtext("Mean SCOTUS Age",side=4,line=2.5,col="red")
dev.off()

FitResid<-with(NewDahl, (Fit$model$NNulls - predict(Fit))) # residuals
FitStandard<-rstandard(Fit) # standardized residuals
FitStudent<-rstudent(Fit) # studentized residuals
FitCooksD<-cooks.distance(Fit) # Cook’s D
FitDFBeta<-dfbeta(Fit) # DFBeta
FitDFBetaS<-dfbetas(Fit) # DFBetaS
FitCOVRATIO<-covratio(Fit) # COVRATIOs

# Can also use influence.measures(Fit)
#
# Plot Studentized residuals:

pdf("StudentResids-24.pdf",7,6)
plot(NewDahl$Year,FitStudent,pch=19,
     ylab="Studentized Residual",xlab="Year")
text(NewDahl[FitStudent>3,]$Year,FitStudent[FitStudent>3],
     labels=NewDahl[FitStudent>3,]$Year,pos=1,cex=0.9)
abline(h=0,lty=2)
dev.off()

max(FitStudent)
NewDahl$Year1935<-ifelse(NewDahl$Year==1935,1,0)
summary(with(NewDahl, lm(NNulls~Age+Tenure+Unified+Year1935)))

# Bubble plot:

pdf("NewInfluencePlot-24.pdf",7,6)
par(mar=c(4,4,2,2))
influencePlot(Fit,id=list(method="noteworthy",n=4,cex=0.7,
                          labels=NewDahl$Year,col="red"),
              xlab="Leverage")
dev.off()

# DFBETAs plots:

pdf("NewDFBETASPlots-24.pdf",9,6)
par(mar=c(4,4,2,2))
dfbetasPlots(Fit,id.n=5,id.col="red",main="",pch=19,
             layout=c(1,3),labels=NewDahl$Year)
dev.off()

# COVRATIO Plot:

pdf("NewCOVRATIOPlot-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(FitCOVRATIO~NewDahl$Year,pch=19,ylim=c(0.5,1.1),
     xlab="Year",ylab="Value of COVRATIO")
abline(h=1,lty=2)
abline(v=c(1800,1850,1900,1950,2000),lty=2)
text(NewDahl[FitCOVRATIO<0.9,]$Year,FitCOVRATIO[FitCOVRATIO<0.9],
     labels=NewDahl[FitCOVRATIO<0.9,]$Year,pos=1,cex=0.9)
dev.off()

# Refitting, without "outliers":

out1<-c(1935) # one outlier
LD2<-NewDahl[!(NewDahl$Year %in% out1),]
out2<-c(1935,1968,1997,2000) # four outliers
LD3<-NewDahl[!(NewDahl$Year %in% out2),]
Fit2<-lm(NNulls~Age+Tenure+Unified,data=LD2)
Fit3<-lm(NNulls~Age+Tenure+Unified,data=LD3)
summary(Fit2)
# summary(Fit3)

stargazer(Fit,Fit2,Fit3)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dahl model, again:

Fit<-lm(NNulls~Age+Tenure+Unified,data=NewDahl)

library(gvlma)
Nope <- gvlma(Fit) # nope
display.gvlmatests(Nope)

# Better:

pdf("DefaultLMPlots-24.pdf",10,8)
par(mfrow=c(2,3))
plot(Fit,which=c(1:6))
dev.off()

# Unpacking that:
#
# #1: Residuals vs. fitted, same as:

pdf("DahlResidVsFitted-24.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Fit,which=1,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #2: QQ plot of residuals:

pdf("DahlResidQQPlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=2,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #3: Scale-Location plot:

pdf("DahlScaleLocationPlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=3,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #4: Cook's Distance (D):

pdf("DahlCooksDPlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=4,labels.id=rownames(NewDahl))
dev.off()

# #5: Residuals vs. Leverage:

pdf("DahlResidVsLeveragePlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=5,labels.id=rownames(NewDahl),pch=20)
dev.off()

# #6: Cook's D vs. Leverage:

pdf("DahlCooksDVsLeveragePlot-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=6,labels.id=rownames(NewDahl),pch=20)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Variances, "robust" things, etc.                    ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ANES 2016 pilot study aggregation example...

ANES<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/ANES-pilot-2016.csv")
ANES$ftgay<-ifelse(ANES$ftgay==998,NA,ANES$ftgay)

# Average feeling thermometers about gays and lesbians:

summary(ANES$ftgay)
summary(ANES$presjob)

# States:

ANES$State<-car::recode(ANES$state,
                        "1='AL';2='AK';4='AZ';5='AR';6='CA';8='CO';9='CT';
10='DE';11='DC';12='FL';13='GA';15='HI';16='ID';17='IL';
18='IN';19='IA';20='KS';21='KY';22='LA';23='ME';24='MD';
25='MA';26='MI';27='MN';28='MS';29='MO';30='MT';31='NE';
32='NV';33='NH';34='NJ';35='NM';36='NY';37='NC';38='ND';
39='OH';40='OK';41='OR';42='PA';44='RI';45='SC';46='SD';
47='TN';48='TX';49='UT';50='VT';51='VA';53='WA';54='WV';
55='WI';56='WY'")

# Aggregate by state:

ANES$one<-1
StateFT<-ddply(ANES,.(State),summarise,
               Nresp=sum(one),
               meantherm=mean(ftgay,na.rm=TRUE),
               meanpresapp=mean(presjob,na.rm=TRUE))
summary(StateFT)

respfit<-lm(meantherm~log(Nresp),data=StateFT)

pdf("StateThermPlot-24.pdf",6,5)
par(mar=c(4,4,2,2)) 
with(StateFT, plot(Nresp,meantherm,pch=".",col="white",log="x",
                   xlab="ln(N of Respondents)",xlim=c(0.5,200),
                   ylab="Statewide Mean Score"))
with(StateFT, text(Nresp,meantherm,log="x",labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(h=mean(ANES$ftgay,na.rm=TRUE),lwd=2)
abline(h=mean(StateFT$meantherm),lwd=2,lty=2,col="red")
abline(respfit,lwd=3,col="darkgreen")
legend("topright",bty="n",lwd=2,col=c("black","red","darkgreen"),
       lty=c(1,2,1),cex=0.7,legend=c("Individual-Level Mean",
                                     "Mean of State Averages",
                                     "Regression Line"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Weighted least squares...                   ####

pdf("WLS-Scatter-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(StateFT, plot(meanpresapp,meantherm,pch=".",col="white",
                   xlab="Mean Obama (Dis)Approval",
                   ylab="Mean Gays+Lesbians FT",
                   xlim=c(2,6)))
with(StateFT, text(meanpresapp,meantherm,labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
dev.off()

# Regressions:

ols<-lm(meantherm~meanpresapp,data=StateFT)
wls1<-lm(meantherm~meanpresapp,data=StateFT,weights=(log(StateFT$Nresp+1)))
wls2<-lm(meantherm~meanpresapp,data=StateFT,weights=(StateFT$Nresp))

stargazer(ols,wls1,wls2,
          column.labels=c("OLS","WLS [1/ln(N)]","WLS [1/N]"),
          dep.var.labels="Mean Gay/Lesbian FTs")

# Plot those:

pdf("WLS-Scatter2-24.pdf",6,5)
par(mar=c(4,4,2,2))
with(StateFT, plot(meanpresapp,meantherm,pch=".",col="white",
                   xlab="Mean Obama (Dis)Approval",
                   ylab="Mean Gays+Lesbians FT",
                   xlim=c(2,6)))
with(StateFT, text(meanpresapp,meantherm,labels=StateFT$State,
                   cex=0.3*log(StateFT$Nresp+1)))
abline(ols,lwd=2)
abline(wls1,lwd=2,lty=2,col="blue")
abline(wls2,lwd=2,lty=3,col="orange")
legend("bottomleft",bty="n",lty=c(1,2,3),lwd=2,
       col=c("black","blue","orange"),
       legend=c("OLS","WLS [weights=ln(N)]","WLS [weights=N]"))
dev.off()


# Add a "robust" variance results to the stat-level
# aggregate regressions table...

rSE<-sqrt(diag(vcovHC(ols,type="const")))
rOLS<-coeftest(ols,vcov.=vcovHC)

stargazer(ols,rOLS,wls1,wls2,se=list(rSE),
          column.labels=c("OLS","OLS (robust)","WLS [1/ln(N)]","WLS [1/N]"),
          dep.var.labels="Mean Gay/Lesbian FTs")


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Make the 2016 NES state-level data for a regression:

ANES$RConserv<-ANES$lcself
ANES$RConserv<-ifelse(ANES$RConserv==8,NA,ANES$RConserv)
ANES$BornAgain<-ANES$pew_bornagain
ANES$BornAgain<-ifelse(ANES$BornAgain==8,NA,ANES$BornAgain)
ANES$BornAgain<- 1 - (ANES$BornAgain-1)
ANES$Age<-2016-ANES$birthyr
ANES$Education<-ANES$educ

StateData<-ddply(ANES,.(State),summarise,
                 NResp=sum(one),
                 LGBTTherm=mean(ftgay,na.rm=TRUE),
                 MeanCons=mean(RConserv,na.rm=TRUE),
                 MeanAge=mean(Age/10,na.rm=TRUE),
                 MeanEducation=mean(Education,na.rm=TRUE),
                 BornAgainProp=mean(BornAgain,na.rm=TRUE))
psych::describe(StateData)

# Fit a basic multivariate model:

OLS<-lm(LGBTTherm~MeanCons+MeanAge+MeanEducation+BornAgainProp,
        data=StateData)
summary(OLS)

# Robust SEs:

hccm(OLS,type="hc3") # "HC3" var-cov matrix
sqrt(diag(hccm(OLS,type="hc3"))) # "HC3" robust SEs
coeftest(OLS,vcov.=vcovHC)

# Compare different types of robust SEs:

OLSBs<-coef(OLS)
Naive<-sqrt(diag(vcov(OLS)))
HC0<-sqrt(diag(hccm(OLS, type="hc0")))
HC1<-sqrt(diag(hccm(OLS, type="hc1")))
HC2<-sqrt(diag(hccm(OLS, type="hc2")))
HC3<-sqrt(diag(hccm(OLS, type="hc3")))

# Plot ("by-hand," sorta...):
pd<-data.frame(Beta=c("Intercept","Mean Conservatism","Mean Age / 10",
                      "Mean Education","Proportion Born-Again"),
               OLSB=coef(OLS),
               OLSSE=sqrt(diag(vcov(OLS))),
               OLSHC0=sqrt(diag(hccm(OLS,type="hc0"))),
               OLSHC1=sqrt(diag(hccm(OLS,type="hc1"))),
               OLSHC2=sqrt(diag(hccm(OLS,type="hc2"))),
               OLSHC3=sqrt(diag(hccm(OLS,type="hc3"))))
pd<-pd[2:5,]

pdf("RobustSEComparison-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(c(1:4)-0.1,pd$OLSB,pch=20,xaxt="n",
     xlim=c(0.5,4.5),ylim=c(-26,25),
     xlab="Variables",ylab="Estimates")
axis(1,at=c(1:4)+0.05,labels=pd$Beta,cex.axis=0.7)
segments(rep(1:4)-0.1,pd$OLSB-(1.96*pd$OLSSE),
         rep(1:4)-0.1,pd$OLSB+(1.96*pd$OLSSE))
points(rep(1:4),pd$OLSB,pch=15,col="red") # HC0
segments(rep(1:4),pd$OLSB-(1.96*pd$OLSHC0),
         rep(1:4),pd$OLSB+(1.96*pd$OLSHC0),col="red")
points(rep(1:4)+0.1,pd$OLSB,pch=18,col="blue") # HC1
segments(rep(1:4)+0.1,pd$OLSB-(1.96*pd$OLSHC1),
         rep(1:4)+0.1,pd$OLSB+(1.96*pd$OLSHC1),col="blue")
points(rep(1:4)+0.2,pd$OLSB,pch=4,col="darkgreen") # HC2
segments(rep(1:4)+0.2,pd$OLSB-(1.96*pd$OLSHC2),
         rep(1:4)+0.2,pd$OLSB+(1.96*pd$OLSHC2),col="darkgreen")
points(rep(1:4)+0.3,pd$OLSB,pch=25,col="orange") # HC2
segments(rep(1:4)+0.3,pd$OLSB-(1.96*pd$OLSHC3),
         rep(1:4)+0.3,pd$OLSB+(1.96*pd$OLSHC3),col="orange")
abline(h=0,lty=2)
legend("topleft",bty="n",pch=c(20,17,15,18,4,25),cex=0.9,
       legend=c("OLS","HC0","HC1","HC2","HC3"),
       col=c("black","red","blue","darkgreen","orange"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GLMS!                                                   ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Toy example

X<-c(1,1,2,2,3,3,4,4,5,5)
Y<-c(0,2,1,3,2,4,3,5,4,6)

pdf("GLMToy-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=19)
dev.off()

linmod<-lm(Y~X)
summary(linmod)
linglm<-glm(Y~X,family="gaussian")
summary(linglm)


# 2008 NES Data: 

NES08<-read_csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/NES2008.csv")

# Clean up data:

NES08$age<-as.numeric(NES08$age)

psych::describe(NES08[,4:16],fast=TRUE,skew=TRUE)

pdf("PolKnowledge-24.pdf",5,4)
par(mar=c(4,4,2,2))
barplot(xtabs(~NES08$knowledge),pch=19,lcolor="grey",
        ylab="Frequency",ylim=c(0,600),
        xlab="Knowledge Score")
dev.off()

# GLM:

nes08.binom<-glm(cbind(knowledge,4-knowledge)~age+female+white+
                   conservative+heterosexual+married+yrsofschool+
                   income,data=NES08,family=binomial)
summary(nes08.binom)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Binary response models...                             ####
#
# A picture of the LPM:

set.seed(7222009)
ystar<-rnorm(100) # 100 random N(0,1) draws
y<-ifelse(ystar>0,1,0) # Binary version of Y
x<-ystar+(0.5*rnorm(100)) # Create X from Y (w/slope=2)
data<-data.frame(ystar,y,x) # data frame

LPMfit<-lm(y~x)

pdf("LPM-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,y,pch=20,main="",xlab="X",ylab="Y",
     ylim=c(-0.2,1.2))
abline(LPMfit,lwd=2.5)
dev.off()

# Residual plot:

pdf("LPMResids-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,LPMfit$residuals,pch=20,xlab="X",
     ylab="Residuals")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Standard normal / logistic

x<-seq(-5,5, by=0.01)
logistic<-dlogis(x)
lCDF<-plogis(x)
normal<-dnorm(x)
nCDF<-pnorm(x)

pdf("LogisticNormalPDFsR-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,normal,xlab="u",t="l",lwd=2,lty=2,col="red",
     ylab="Probability")
lines(x,logistic,lwd=2)
legend("topright",bty="n",legend=c("Standard Logistic",
                                   "Standard Normal"),
       lwd=2,lty=c(1,2),col=c("black","red"))
abline(v=0,lty=3)
dev.off()

pdf("LogisticNormalCDFsR-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,nCDF,xlab="u",t="l",lwd=2,lty=2,col="red",
     ylab="Cumulative Probability")
lines(x,lCDF,lwd=2)
legend("bottomright",bty="n",legend=c("Standard Logistic",
                                      "Standard Normal"),
       lwd=2,lty=c(1,2),col=c("black","red"))
abline(v=0,lty=3)
dev.off()

# Odds and log-odds:

Z <- seq(0.02,0.98,by=0.001)
IZ <- 1 - Z
O = Z / IZ
lnO = log(O)

pdf("LogOddsR-24.pdf",8,5)
par(mar=c(5,4,2,2))
par(mfrow=c(1,2))
plot(O,Z,ylab="Probability of Z",t="l",lwd=2,
     xlab="Odds of Z: [P(Z) / (1-P(Z))]",
     ylim=c(0,1),main="Odds")
abline(h=c(0,1),lty=3)
plot(lnO,Z,ylab="Probability of Z",t="l",lwd=2,
     xlab="Log-Odds of Z",
     ylim=c(0,1),main="Log-Odds")
abline(h=c(0,1),lty=3)
dev.off()

# C-log-log

logit<-lCDF
cloglog1 <- 1 - exp(-exp(x))
cloglogneg1 <- 1 - exp(-exp(-x))

pdf("CLogLogCDFsR-24.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(5,4,2,2))
plot(x,lCDF,xlab="XB",t="l",lwd=2,lty=1,col="black",
     ylab="Pr(Y=1)")
lines(x,cloglog1,lwd=2,lty=2,col="red")
lines(x,cloglogneg1,lwd=2,lty=4,col="blue")
legend(1,0.6,bty="n",legend=c("Standard Logit (B=1)",
                              "C-Log-Log (B=1)",
                              "C-Log-Log (B=-1)"),
       lwd=2,lty=c(1,2,4),col=c("black","red","blue"),
       cex=0.8)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Toy logit / probit / c-log-log example:

set.seed(7222009)
ystar<-rnorm(100,0.5,0.5)
y<-ifelse(ystar>0.5,1,0)
x<-ystar+(0.5*rnorm(100))
data<-data.frame(ystar,y,x)
head(data)

pdf("YstarYX-R-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,ystar,pch=19,ylab="Y* / Y",xlab="X")
points(x,y,pch=4,col="red")
abline(h=c(0,0.5,1),lty=c(1,2,1))
legend("topleft",bty="n",pch=c(19,4),col=c("black","red"),
       legend=c("Y*","Y"))
dev.off()

# Probits and logits and c-log-logs...

myprobit<-glm(y~x,family=binomial(link="probit"),
              data=data)
summary(myprobit)

mylogit<-glm(y~x,family=binomial(link="logit"),
             data=data)
summary(mylogit)

mycloglog<-glm(y~x,family=binomial(link="cloglog"),
               data=data)
summary(mycloglog)

stargazer(mylogit,myprobit,mycloglog)


pdf("LogitProbitHats-24.pdf",5,5)
par(mar=c(5,4,2,2))
plot(mylogit$fitted.values,myprobit$fitted.values,
     pch=20,xlab="Logit Predictions",
     ylab="Probit Predictions")
abline(a=0,b=1,lty=1,col="red")
dev.off()

# Reverse the outcome:

data$newY <- 1 - data$y

RP<-glm(newY~x,family=binomial(link="probit"),data=data)
RL<-glm(newY~x,family=binomial(link="logit"),data=data)
RC<-glm(newY~x,family=binomial(link="cloglog"),data=data)

# Gather coefficients & print:

Bs<-matrix(c(coef(myprobit)[1],coef(mylogit)[1],coef(mycloglog)[1],
             coef(RP)[1],coef(RL)[1],coef(RC)[1],coef(myprobit)[2],
             coef(mylogit)[2],coef(mycloglog)[2],coef(RP)[2],coef(RL)[2],
             coef(RC)[2]),nrow=3,ncol=4)

print(xtable(Bs)) # <-- needs relabeling, etc. to match slides

# /fin