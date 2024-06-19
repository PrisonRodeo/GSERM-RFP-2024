#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                       #####################
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Code GSERM "Regression for Publishing"
# (June 2024)
#
# Day Four: GLMs: Binary Outcomes + Event Counts
#
# File last modified 6/20/2024
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages:
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("readr","car","psych","plyr","rms","lmtest","dplyr",
     "gmodels","margins","mfx","RCurl","msme","stargazer",
     "aod","ggplot2","ROCR","pROC","marginaleffects","MASS",
     "performance","modelsummary","easystats")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 7-8 times until you get all smileys. :)
#
# Options:

options(scipen=12)
options(digits=3)

# setwd()...
#
# setwd("~/Dropbox (Personal)/GSERM/RFP2024") # ...or whatever...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# NAFTA example...                                ####

nafta<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/NAFTA.csv")

# Rescale some things:

NAFTA <- with(nafta, data.frame(Vote=vote,
                                PropHisp=pcthispc/100,
                                Democrat=democrat,
                                COPE=cope93/100))
NAFTA$DemXCOPE<-NAFTA$Democrat*NAFTA$COPE
rm(nafta)

describe(NAFTA,fast=TRUE,skew=TRUE)

# Probit:

NAFTA.probit<-glm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
                  NAFTA,family=binomial(link="probit"))
summary(NAFTA.probit)

# Logit:

NAFTA.fit<-glm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
               NAFTA,family=binomial)
summary(NAFTA.fit)

# Alternative logit:

fit<-glm(Vote~PropHisp+Democrat*COPE,NAFTA,family=binomial)

# Assemble and compare logit vs. probit estimates in 
# a table, using -modelsummary-:

models<-list("Logit" = NAFTA.fit,
             "Probit" = NAFTA.probit)
options("modelsummary_format_numeric_latex"="plain")
modelsummary(models,title="Logits and Probits",
             coef_rename = c("PropHisp"="Proportion Hispanic",
                             "Democrat"="Democrat",
                             "COPE"="COPE Score",
                             "DemXCOPE"="Democrat x COPE Score"),
             output="ProbitLogitTable-24.tex")

# Plot estimates / CIs, using -modelplot-:

pdf("LogitsProbitsFig-24.pdf",6,4)
modelplot(models)
dev.off()

pdf("LogitsProbitsFig2-24.pdf",6,4)
modelplot(models,facet=TRUE)
dev.off()

# Plot coefficient comparison:

comparedf <-data.frame(probit = coefficients(NAFTA.probit),
                       logit = coefficients(NAFTA.fit))
lpfit<-lm(logit~probit,data=comparedf)

pdf("NAFTAProbitVsLogit-24.pdf",7,6)
par(mar=c(4,4,2,2))
with(comparedf, 
     plot(probit,logit,pch=20,xlim=c(-3,3.5),
          ylim=c(-7,8),xlab="Logit Estimates",
          ylab="Probit Estimates"))
with(comparedf, text(probit,logit,labels=rownames(comparedf),
                     pos=c(1,3,3,1,4)))
abline(lpfit,lwd=2,lty=2,col="red")
text(-1,5.5,col="red",labels=paste0("Adj. R-squared = ",
                      round(summary(lpfit)$adj.r.squared,2)))
dev.off()

#=-=-=-=-=-=-=
# For fun, let's compare those two models to a c-log-log
# and a LPM:

NAFTA.LPM<-lm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
                  data=NAFTA)
NAFTA.CLogLog<-glm(Vote~PropHisp+Democrat+COPE+DemXCOPE,
                  NAFTA,family=binomial(link="cloglog"))

# Make a table:

models2<-list("Logit" = NAFTA.fit,
             "Probit" = NAFTA.probit,
             "C-Log-Log" = NAFTA.CLogLog,
             "LPM" = NAFTA.LPM)
options("modelsummary_format_numeric_latex"="plain")
modelsummary(models2,title="All The Models",
             coef_rename = c("PropHisp"="Proportion Hispanic",
                             "Democrat"="Democrat",
                             "COPE"="COPE Score",
                             "DemXCOPE"="Democrat x COPE Score"),
             output="AllModelsTable-24.tex")

# Compare predictions:

logit.hats<-predict(NAFTA.fit,type="response")
probit.hats<-predict(NAFTA.probit,type="response")
cloglog.hats<-predict(NAFTA.CLogLog,type="response")
LPM.hats<-predict(NAFTA.LPM)

Hats<-cbind(logit.hats,probit.hats,cloglog.hats,LPM.hats)

pdf("BinaryHatComparisons-24.pdf",8,7)
par(mar=c(2,2,2,2))
scatterplotMatrix(Hats,pch=20,col="black",
                  regLine=list(method=lm,lty=1,lwd=2,col="red"),
                  var.labels=c("Logit","Probit","C-Log-Log","LPM"))
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=
# Deviances...

LLR<-NAFTA.fit$null.deviance - NAFTA.fit$deviance
LLR
pchisq(LLR,4,lower.tail=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Interpreting interactions...                    ####
#
# phi-hat:

NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]

# z-statistic:

(NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.fit)[4,4] + 
          (1)^2*vcov(NAFTA.fit)[5,5] + 
          2*1*vcov(NAFTA.fit)[4,5]))

# Square that, and it's a chi-square statistic:

((NAFTA.fit$coeff[4]+NAFTA.fit$coeff[5]) / 
    (sqrt(vcov(NAFTA.fit)[4,4] + 
            (1)^2*vcov(NAFTA.fit)[5,5] + 
            2*1*vcov(NAFTA.fit)[4,5])))^2

# Same thing, using -linear.hypothesis- in -car-:

linearHypothesis(NAFTA.fit,"COPE+DemXCOPE=0")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Marginal effects (not generally recommended):   ####

summary(margins(NAFTA.fit))

pdf("NAFTAMarginalEffects-24.pdf",7,6)
plot(margins(NAFTA.fit),
     labels=c("COPE","Democrat","Interaction","Percent Hispanic"))
dev.off()

# More, using -marginaleffects-:

avg_slopes(fit)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Odds Ratios:                                    ####

P<-qnorm(0.975)

lreg.or <- function(model)
{
  coeffs <- coef(summary(model))
  lowerCI <- exp(coeffs[ ,1] - P * coeffs[ ,2])
  OR <- exp(coeffs[ ,1])
  upperCI <- exp(coeffs[ ,1] + P * coeffs[ ,2])
  lreg.or <- cbind(OR,lowerCI,upperCI)        
  lreg.or
}

lreg.or(NAFTA.fit)

# Or you can use this:

exp(cbind(OR=coef(NAFTA.fit),confint.default(NAFTA.fit)))

# Or use -modelsummary- and / or -modelplot-, like we
# did above:

modelsummary(fit,title="Odds Ratios",
             exponentiate=TRUE,
             output="ORTable-24.tex")

pdf("OddsRatiosFig-24.pdf",5,5)
modelplot(fit,exponentiate=TRUE)
dev.off()

# Plot of odds ratios against size of change in PropHispc:

Change<-seq(0.01,0.60,by=0.01)
PctChange<-((exp(2.09*Change)-1)*100)
PctChangeLB<-((exp(0.54*Change)-1)*100)
PctChangeUB<-((exp(3.64*Change)-1)*100)

pdf("ORIllustration-24.pdf",7,6)
plot(Change,PctChange,t="l",lwd=2,
     ylim=c(0,800),
     xlab="Size of Change in Proportion Hispanic",
     ylab="Percentage Change in Odds of a Pro-NAFTA Vote")
lines(Change,PctChangeUB,lwd=1,lty=2)
lines(Change,PctChangeLB,lwd=1,lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Predicted values...                             ####
#
# In-sample:

preds<-NAFTA.fit$fitted.values
hats<-predict(NAFTA.fit,se.fit=TRUE)

# Plotting in-sample predictions:

XBUB<-hats$fit + (1.96*hats$se.fit) 
XBLB<-hats$fit - (1.96*hats$se.fit)
plotdata<-cbind(as.data.frame(hats),XBUB,XBLB)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))

pdf("LogitInSampleHatsR-24.pdf",8,6)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
with(NAFTA, 
     plotCI(COPE[Democrat==1],plotdata$fit[Democrat==1],ui=plotdata$XBUB[Democrat==1],
            li=plotdata$XBLB[Democrat==1],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))
with(NAFTA, 
     plotCI(COPE[Democrat==0],plotdata$fit[Democrat==0],ui=plotdata$XBUB[Democrat==0],
            li=plotdata$XBLB[Democrat==0],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)"))
dev.off()

# Out-of-sample predictions:

sim.data<-data.frame(PropHisp=mean(NAFTA$PropHisp),Democrat=rep(0:1,101),
                     COPE=seq(from=0,to=1,length.out=101))
sim.data$DemXCOPE<-sim.data$Democrat*sim.data$COPE

OutHats<-predict(NAFTA.fit,se.fit=TRUE,newdata=sim.data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

both<-cbind(sim.data,OutHats)
both<-both[order(both$COPE,both$Democrat),]
bothD<-both[both$Democrat==1,]
bothR<-both[both$Democrat==0,]

pdf("COPEHatsR-24.pdf",8,6)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
plot(bothD$COPE,bothD$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(bothD$COPE,bothD$OutHatsUB,lty=2)
lines(bothD$COPE,bothD$OutHatsLB,lty=2)
text(0.3,0.2,label="Democrats")
plot(bothR$COPE,bothR$fit,t="l",lwd=2,ylim=c(0,1),
     xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(bothR$COPE,bothR$OutHatsUB,lty=2)
lines(bothR$COPE,bothR$OutHatsLB,lty=2)
text(0.7,0.9,label="Republicans")
dev.off()

# Simpler version (for when there aren't interactions,
# and you're interested in a single continuous
# predictor), using -margins-:

pdf("PropHispPredPlot-24.pdf",7,6)
par(mar=c(4,4,2,2))
cplot(NAFTA.fit,"PropHisp",xlab="Proportion Hispanic")
dev.off()

# Similar thing, using -marginaleffects- package tool
# called "plot_predictions":

pdf("PropHispPredPlot2-24.pdf",7,6)
plot_predictions(fit,condition="PropHisp") + 
  theme_classic()
dev.off()

# Now plot the predictions from the interactive part of
# the model, using marginaleffects::plot_predictions. Note
# that we use the model called "fit" here, so that the 
# software recognizes that there's an interactive term:

pdf("NAFTAInterPredProbs-24.pdf",6,4)
plot_predictions(fit,condition=c("COPE","Democrat")) + theme_classic()
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Goodness of fit...                              ####
#
# PRE (with tau=0.5):

table(NAFTA.fit$fitted.values>0.5,NAFTA$Vote==1)
chisq.test(NAFTA.fit$fitted.values>0.5,NAFTA$Vote==1)

# PREs, with different cutoffs / taus...
#
# Tau = 0.2:

Hats02<-ifelse(NAFTA.fit$fitted.values>0.2,1,0)
CrossTable(NAFTA$Vote,Hats02,prop.r=FALSE,prop.c=FALSE,
           prop.t=FALSE,prop.chisq=FALSE)

# Tau = 0.8:

Hats08<-ifelse(NAFTA.fit$fitted.values>0.8,1,0)
CrossTable(NAFTA$Vote,Hats08,prop.r=FALSE,prop.c=FALSE,
           prop.t=FALSE,prop.chisq=FALSE)


# ROC curves, plots, etc. (using -ROCR-):

NAFTA.hats<-predict(NAFTA.fit,type="response")
preds<-ROCR::prediction(NAFTA.hats,NAFTA$Vote)

pdf("ROCCurve-Good-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(ROCR::performance(preds,"tpr","fpr"),lwd=2,lty=2,
     col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)
dev.off()

# "Bad" model:

NAFTA.bad<-with(NAFTA,
                glm(Vote~PropHisp,family=binomial(link="logit")))
NAFTA.bad.hats<-predict(NAFTA.bad,type="response")
bad.preds<-ROCR::prediction(NAFTA.bad.hats,NAFTA$Vote)

pdf("ROCCurve-Bad-24.pdf",6,5)
plot(ROCR::performance(bad.preds,"tpr","fpr"),lwd=2,lty=2,
     col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)
dev.off()

# Comparing ROCs:

GoodROC<-roc(NAFTA$Vote,NAFTA.hats,ci=TRUE)
GoodAUC<-pROC::auc(GoodROC)
BadROC<-roc(NAFTA$Vote,NAFTA.bad.hats)
BadAUC<-pROC::auc(BadROC)

GoodAUC

BadAUC

# Comparison plot:

pdf("TwoROCs-24.pdf",5,5)
par(mar=c(4,4,2,2))
plot(GoodROC)
lines(BadROC,col="red",lty=2)
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Event Count Models                                  ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Various Poisson histograms                          ####

set.seed(7222009)
N<-1000
LP05<-rpois(N,0.5)
LP1<-rpois(N,1)
LP5<-rpois(N,5)
LP10<-rpois(N,10)

pdf("PoissonHistogramsR-24.pdf",7,6)
par(mfrow=c(2,2))
hist(LP05,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 0.5")
hist(LP1,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 1.0")
hist(LP5,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 5")
hist(LP10,col="grey",xlim=c(0,25),breaks=seq(0,25,by=1),
     ylim=c(0,1000),xlab="Count",main="Lambda = 10")
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Dahl / Poisson example...                      ####

NewDahl<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/Dahl2021.csv")

NewDahl<-NewDahl[complete.cases(NewDahl),]
row.names(NewDahl)<-NewDahl$Year
psych::describe(NewDahl,fast=TRUE,skew=TRUE)

pdf("NewDahlScatterplotMatrix2-24.pdf",10,8)
scatterplotMatrix(~NNulls+Age+Tenure+Unified,data=NewDahl,
                  pch=19,col="black",
                  regLine=list(col="red"),
                  smooth=list(spread=FALSE))
dev.off()

# Poisson regression:

nulls.poisson<-glm(NNulls~Age+Tenure+Unified,family="poisson",
                   data=NewDahl)
summary(nulls.poisson)

# Plot coefficients (using -modelsummary-):

pdf("PoissonCoefPlot-24.pdf",6,5)
p<-modelplot(nulls.poisson,coef_omit="Intercept") + 
  geom_vline(xintercept=0,linetype="dashed") +
  theme_classic()
p
dev.off()


#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Offsets (REVISED):                             ####
#
# Plot (cases per year):

pdf("AnnualExposure-24.pdf",6,4)
par(mar=c(4,4,2,2))
with(NewDahl,plot(Year,NConstDecisions,t="l",lwd=1.5,
                  ylab="N of Constitutional Decisions"))
dev.off()

# Add offset:

nulls.poisson2<-glm(NNulls~Age+Tenure+Unified,family="poisson",
                    offset=log(NConstDecisions+1),data=NewDahl)
summary(nulls.poisson2)

# Add "flexible" offset:

nulls.poisson3<-glm(NNulls~Age+Tenure+Unified+log(NConstDecisions+1),
                    family="poisson",data=NewDahl)
summary(nulls.poisson3)

# Wald test:

wald.test(b=coef(nulls.poisson3),Sigma=vcov(nulls.poisson3),Terms=4,H0=1)

# Coefficient plots:

pdf("OffsetsCoefPlot-24.pdf",6,4)
p<-modelplot(list("No Offset"=nulls.poisson,
                  "With Offset"=nulls.poisson2,
                  "Flexible"=nulls.poisson3),
             coef_omit=c(-2,-3,-4)) + 
  geom_vline(xintercept=0,linetype="dashed") +
  theme_classic()
p
dev.off()


# Incident Rate Ratios:

nulls.poisson.IRR<-poissonirr(NNulls~Age+Tenure+Unified,
                              data=NewDahl)
nulls.poisson.IRR

# Predictions:

simdata<-data.frame(Age=seq(from=45,to=71,by=1),
                    Tenure=mean(NewDahl$Tenure,na.rm=TRUE),
                    Unified=1)
nullhats<-predict(nulls.poisson,newdata=simdata,se.fit=TRUE)

# NOTE: These are XBs, not predicted counts.
# Transforming:

nullhats$Yhat<-exp(nullhats$fit)
nullhats$UB<-exp(nullhats$fit + 1.96*(nullhats$se.fit))
nullhats$LB<-exp(nullhats$fit - 1.96*(nullhats$se.fit))

# Plot...

pdf("NewNullsOutOfSampleHatsR-24.pdf",6,5)
par(mar=c(4,4,2,2))
plot(simdata$Age,nullhats$Yhat,t="l",lwd=3,ylim=c(0,4),ylab=
       "Predicted Count", xlab="Mean Court Age")
lines(simdata$Age,nullhats$UB,lwd=2,lty=2)
lines(simdata$Age,nullhats$LB,lwd=2,lty=2)
dev.off()

# Same thing, using -marginaleffects-:

pdf("AltOutOfSampleHats-24.pdf",6,5)
plot_predictions(nulls.poisson,condition="Age") +
  theme_classic()
dev.off()

# Predicted probabilities...
#
# Generate predicted probabilities for each possible 
# value of Y in {0,10} for two hypothetical cases:

LambdaLo<-exp(nullhats$fit[[13]]) # Age = 57
LambdaHi<-exp(nullhats$fit[[24]]) # Age = 67
PredPrs<-data.frame(Count=0:10,
                    Age57=dpois(0:10,LambdaLo),
                    Age67=dpois(0:10,LambdaHi))

# Plots:

pdf("NewPoissonPredProbs-24.pdf",10,5)
par(mfrow=c(1,2))
par(mar=c(4,4,4,2))
barplot(PredPrs$Age57,names.arg=0:10,
        xlab="Number of Nullifications",
        ylab="Predicted Probability",
        main="Mean Court w/Age = 57",
        ylim=c(0,0.9))
barplot(PredPrs$Age67,names.arg=0:10,
        xlab="Number of Nullifications",
        ylab="Predicted Probability",
        main="Mean Court w/Age = 67",
        ylim=c(0,0.9))
dev.off()

# Changes in predicted probabilities:

PredPrs$Change<-with(PredPrs,Age67-Age57)

pdf("PoissonChangesInPredPrs-24.pdf",8,5)
par(mfrow=c(1,1))
par(mar=c(4,4,4,2))
foo<-barplot(PredPrs$Change,horiz=TRUE,
             xlab="Change in Predicted Probability of Count",
             ylab="Count",xlim=c(-0.7,0.35),
             main="Changes: Mean Age = 57 to Mean Age = 67")
axis(2,foo,labels=0:10,cex=0.8)
text(PredPrs$Change,foo,labels=round(PredPrs$Change,3),
     cex=0.6,pos=c(2,rep(4,times=10)))
dev.off()

#\fin