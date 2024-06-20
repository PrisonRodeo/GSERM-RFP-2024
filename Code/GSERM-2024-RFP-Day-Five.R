#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                       #####################
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Code GSERM "Regression for Publishing"
# (June 2024)
#
# Day Five: Text Analysis Potpourri...
#
# File last modified 6/21/2024
#
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                     ####
#
# The following bit of code takes a list of packages
# ("P") and (a) checks  for whether the package is 
# installed or not, (b) installs it if it is not, 
# and then (c) loads each of them.

P<-c("devtools","readr","haven","plyr","dplyr",
     "statmod","lubridate","stringr","MASS","httr",
     "pdftools","RSelenium","rvest","tesseract",
     "readtext","rtweet","wordcloud","text",
     "wordcloud2","tm","stopwords","SnowballC","car",
     "tokenizers","lsa","arm","stm","SentimentAnalysis",
     "quanteda","topicmodels","quanteda.textmodels",
     "quanteda.textplots","reticulate","here","psych")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P,i)

# ^^ Run that code 15-20 times, until you get 
# all smiley faces...
#
#=-=-=-=-=-=-=-=-=-=-=-=
# Setting more options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Finally be sure to create a project or -setwd- or
# whatever somewhere in here...

# setwd("~/Dropbox (Personal)/GSERM/RFP2024") # ...or whatever...

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bags and Bags of Words!                           ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Sentiment Analysis...
#
# Example: Speeches by every UN High Commissioner
# for Refugees, 1970-2016...
#
# Get the data from the Github repo:

UN<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/UNHCRSpeeches.csv")

# Now do a bunch of text cleaning and preprocessing
# to get it in shape for analysis:

UN$content <- removeNumbers(UN$content) # no numbers
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$content <- removeWords(UN$content,stopwords("en")) # remove stopwords
UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$foo <- NULL
# Speech authors:
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Create a corpus:

UN2 <- with(UN, data.frame(doc_id = id,
                           text = content))
ds <- DataframeSource(UN2) 
UNC <- Corpus(ds)
meta(UNC)

UNCount<-countWords(UNC,removeStopwords=FALSE)
summary(UNCount$WordCount)

pdf("UNHCR-Hist.pdf",6,5)
par(mar=c(4,4,2,2))
hist(UNCount$WordCount,main=" ",xlab="Word Count",
     col="grey32")
dev.off()

# Do a simple sentiment analysis on those speeches:

UNSent <- analyzeSentiment(UNC)
psych::describe(UNSent,fast=TRUE,skew=TRUE)   # STOPPED!!!!!!

# Plot: sentiment vs. word count: Are longer speeches
# happier or sadder?
#
# Plots, etc.:

rSC<-with(UNSent, cor(log(WordCount),SentimentGI))

pdf("UNHCRSentVsCount.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~WordCount,data=UNSent,log="x",
            pch=20,grid=FALSE,xlab="ln(Word Count)",
            ylab="Sentiment",spread=FALSE)
abline(h=0,lty=2,lwd=1.5)
text(100,0.25,paste0("r = ",round(rSC,2)))
dev.off()

# How does speech sentiment vary over time?

pdf("UNHCRSentOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNSent$SentimentGI,t="l",lwd=1.5,
     xlab="Date",ylab="Speech Sentiment")
lines(lowess(UN$Date,UNSent$SentimentGI),lwd=2,col="red")
abline(h=0,lty=2)
dev.off()

# Same plot, but aggregated by year...

AnnMeans<-aggregate(UNSent$SentimentGI,list(UN$Year),mean)

pdf("UNHCRAnnMeans.pdf",6,5)
par(mar=c(4,4,2,2))
plot(AnnMeans$Group.1,AnnMeans$x,t="l",lwd=1.5,
     xlab="Year",ylab="Average Sentiment",ylim=c(0.04,0.17))
lines(lowess(AnnMeans$Group.1,AnnMeans$x),lwd=2,col="red")
dev.off()

# Sentiment by speech author / speaker; do different
# UNHCRs have different levels of sentiment?:

UN$Author<-ordered(UN$Author,levels=c("Goedhart","Lindt",
                                      "Schnyder","Khan","Hartling",
                                      "Hocké","Stoltenberg","Ogata",
                                      "Lubbers","Guterres"))

pdf("UNHCR-by-Author.pdf",6,5)
par(mar=c(6,4,2,2))
boxplot(UNSent$SentimentGI~UN$Author,las=2,
        xlab="",ylab="Sentiment")
abline(h=0,lty=2)
dev.off()

# Next: Does the choice of sentiment dictionary 
# "matter"?
#
# Compare "General Inquirer" to "QDAP":

GI<-loadDictionaryGI()
QD<-loadDictionaryQDAP()

compareDictionaries(GI,QD)

# Plot the differences:

r.GI.QDAP <- with(UNSent, cor(SentimentGI,SentimentQDAP))

pdf("UNHCR-Dict-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
scatterplot(SentimentGI~SentimentQDAP,data=UNSent,
            xlab="QDAP",ylab="General Inquirer",pch=20,
            grid=FALSE)
text(0,0.20,paste0("r = ",round(r.GI.QDAP,2)))
dev.off()

# "Custom" dictionary -- here, useful for topic
# identification:

# Custom dictionary by-hand:

YugoWords <- c("yugoslavia","serbia","bosnia","herzegovina",
               "kosovo","montenegro","macedonia","croatia",
               "vojvodina","balkans")

FmrYugo <- SentimentDictionaryWordlist(YugoWords)

UNHCRYugo <- analyzeSentiment(UNC,
                              rules=list("YugoTopic"=list(
                                ruleRatio,FmrYugo)))

summary(UNHCRYugo$YugoTopic)

# Plot the extent of Fmr. Yugoslavia content over
# time:

pdf("UNHCRYugoOverTime.pdf",6,5)
par(mar=c(4,4,2,2))
plot(UN$Date,UNHCRYugo$YugoTopic,t="l",lwd=1.5,
     xlab="Date",ylab="Fmr. Yugoslavia Content")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inductive Methods: Topic Models                 ####
#
# Once again, get the UNHCR data (we could use the
# same file we did above, but this way I can demon-
# strate a different text processor):

UN<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/UNHCRSpeeches.csv")

# Clean things up a bit:

UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Process text (this time using textProcessor from stm):
#
# Note that default to converting cases, removing stopwords / 
# punctuation / words < 3 characters / extra white space, 
# and stemming the words.

UNHCR <- textProcessor(UN$content, metadata=UN) 

# Create stm corpus. Note that this defaults to dropping
# words that only appear in one document:

UNCorp <- prepDocuments(UNHCR$documents,UNHCR$vocab,UNHCR$meta)

# Next: Basic LDA (latent dirichlet allocation) 
# using the -topicmodels- package...
#
# Convert the corpus format:

UNLDACorp <- convertCorpus(UNCorp$documents,UNCorp$vocab,
                           type="slam")

# Basic LDA, with six topics:

UN.LDAV.6 <- LDA(UNLDACorp,6,method="VEM",
                 seed=7222009)

# Check out the terms / topics:

terms(UN.LDAV.6,10)

# Which of the six topics is the highest-probability
# one for each of the 703 documents?

pdf("UNTopicBarplot.pdf",8,4)
par(mar=c(4,4,2,2))
barplot(table(topics(UN.LDAV.6,10)[1,]),
        xlab="Topic Number",ylab="Frequency")
dev.off()

# Generate posterior probabilities of the topics 
# for each document and the terms for each topic:

V.6.Post <- posterior(UN.LDAV.6)
# cor(V.6.Post$topics)

# Plot those:

pdf("LDA-Posteriors.pdf",9,7)
scatterplotMatrix(V.6.Post$topics,pch=".",smooth=FALSE,
                  col="black",regLine=FALSE,
                  var.labels=paste0("Topic ",
                                    colnames(V.6.Post$topics)))
dev.off()

# How do topics vary across authors / speech-givers?

pdf("LDA-By-Author.pdf",8,6)
par(mar=c(6,4,2,2))
par(mfrow=c(2,3))
boxplot(V.6.Post$topic[,1]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic One")
boxplot(V.6.Post$topic[,2]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Two")
boxplot(V.6.Post$topic[,3]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Three")
boxplot(V.6.Post$topic[,4]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Four")
boxplot(V.6.Post$topic[,5]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Five")
boxplot(V.6.Post$topic[,6]~UN$Author,xlab="",ylab="Proportion",
        las=2,main="Topic Six")
dev.off()


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inductive Methods: Text Scaling                 ####
#
# We'll stick with the UNHCR speech data for some 
# minimal examples of text scaling. 
#
# Reread + clean the data again:

UN<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/UNHCRSpeeches.csv")

UN$Year <- as.numeric(str_sub(UN$by, -4)) # Year of the speech
UN$foo <- str_extract(UN$by, '\\b[^,]+$')
UN$Date <- as.Date(UN$foo, format="%d %B %Y") # date of speech
UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks
UN$foo <- NULL
UN$Author <- "Goedhart"  # Fix names
UN$Author <- ifelse(UN$author=="lindt",paste("Lindt"),UN$Author)
UN$Author <- ifelse(UN$author=="schnyder",paste("Schnyder"),UN$Author)
UN$Author <- ifelse(UN$author=="khan",paste("Khan"),UN$Author)
UN$Author <- ifelse(UN$author=="hartling",paste("Hartling"),UN$Author)
UN$Author <- ifelse(UN$author=="hocké",paste("Hocké"),UN$Author)
UN$Author <- ifelse(UN$author=="stoltenberg",paste("Stoltenberg"),UN$Author)
UN$Author <- ifelse(UN$author=="ogata",paste("Ogata"),UN$Author)
UN$Author <- ifelse(UN$author=="lubbers",paste("Lubbers"),UN$Author)
UN$Author <- ifelse(UN$author=="guterres",paste("Guterres"),UN$Author)

# Create a quanteda corpus:

UNs<-data.frame(doc_id=1:nrow(UN),
                text=UN$content,
                Author=UN$Author,
                Year=UN$Year)

UNC<-corpus(UNs)       # corpus
UN.token <- tokens(UNC) # "tokenize"
UN.DFM <- dfm(UN.token)

# Wordscores require "training" (human-coded)
# data, which we don't have for the UNHCR 
# speeches; and Wordfish doesn't seem to like
# these data very much. In the interest of having
# *something* here, I'll fit a couple alter-
# ative models instead.
#
# Estimate a latent semantic analysis model:

UN.LSM<-textmodel_lsa(UN.DFM,nd=0)

# Estimate correspondence analysis scaling:

UN.CA<-textmodel_ca(UN.DFM)

# etc.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Going Bagless: Transformers, LLMs, etc.         ####
#
# There isn't much here for code... a good place
# to start is by uncommenting and running this code,
# taken from https://github.com/OscarKjell/text

devtools::install_github("oscarkjell/text")
library(text)
textrpp_install(python_path="/Users/cuz10/foo/bin") # installs other things...
2
textrpp_initialize(save_profile = TRUE)
1

# That installs a bunch of things, including a 
# small version of the CONDA environment. It also
# probably fails at the end, albeit with a bunch of 
# helpful verbiage about why it did so (probably 
# because you need a working Python environment
# installed first, including the pip() installer).
#
# As I note in class, if you're going to get
# deep into BERT and GPT models of language, R
# is probably not (right now) the language to do 
# it with; Python is best, and Java or C/C++ are
# also viable options.
#
# Here's my very minimal example of calling
# transformers in Python via R. To do this, 
# you first need to install Python on your
# machine; you can see how to do that here:
#
# https://packaging.python.org/en/latest/guides/installing-using-pip-and-virtual-environments/
#
# Once you've done that, you need to install 
# PyTorch (or Tensorflow) and set up a virtual
# environment. On my recent M1 Mac, I did this
# by running these things **in the shell** (that
# is, in the "Terminal" app if you're on a Mac).
# You can also do that using -system-:

system("source '$HOME/.cargo/env'")
system("python3 -m pip install --user --upgrade pip")
system("python3 -m pip install --user virtualenv")
system("python3 -m venv foo")
system("source foo/bin/activate")
system("pip3 install torch torchvision torchaudio tokenizers")

# Then, and only then, you can start Python (note that
# you'll want to change the path...):

use_python("/Users/cuz10/Library/r-miniconda-arm64/envs/r-reticulate/bin/python") # or whatever
py_config()

# ... and install the transformers:

py_install("transformers",pip=TRUE)

# Next, bring the transformers into the current
# R session:

trans<-reticulate::import("transformers")

# ...and create a "pipeline," in this case
# for sentiment classification:

sentimentGPT<-trans$pipeline(task="text-classification") 

# Now go get the text of the Gettysburg address
# (again) and pass it to the transformer:

GBA<-read_file("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/GBA.txt")

GBA.sentiment<-sentimentGPT(GBA)
GBA.sentiment

# Now, do the same with the UN speeches:

UN<-read.csv("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/UNHCRSpeeches.csv")

# Minimal cleaning and preprocessing to get it in 
# shape for analysis:

UN$content <- gsub("\\\\n", " ", UN$content) # remove line breaks

# Break into sentences:

UN.sent<-unlist(tokenize_sentences(UN$content))

# Sentiment for each sentence...

bar<-list() # a place to put the results

# Pass each sentence of the 703 speeches through
# our pipeline, and store the result (note that
# this can take 20-30 MINUTES to run):

for(i in 1:length(UN.sent)){
  bar[[i]]<-sentimentGPT(UN.sent[[i]])
}

# Now take the thing called "bar" (that is a long
# names list of the sentiment scores) and turn 
# it into data that we can use:

df.BERT<-as.data.frame(matrix(unlist(bar),ncol=2,byrow=TRUE))
colnames(df.BERT)<-c("Direction","Score")
df.BERT$Score<-as.numeric(df.BERT$Score)
df.BERT$SignScore<-ifelse(df.BERT$Direction=="NEGATIVE",
                          -df.BERT$Score,df.BERT$Score)

#... e.g., to make a plot (in this case, over time /
# sentences):

pdf("BERT-UN-Sentiment.pdf",7,5)
par(mar=c(4,4,2,2))
plot(lowess(df.BERT$SignScore),t="l",lwd=2,
     ylim=c(0.22,0.38),
     xlab="Sentence Number",ylab="Sentiment (Lowess smooth)")
dev.off()

# OK, that's all. :)

# final /fin