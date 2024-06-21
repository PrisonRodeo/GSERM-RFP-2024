#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...                                   ####
#
# GSERM "Regression for Publishing" - June 2024
#
# Introduction to the Analysis of Text...
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
     "readtext","rtweet","wordcloud",
     "wordcloud2","tm","stopwords","SnowballC","car",
     "tokenizers","lsa","arm","stm","SentimentAnalysis",
     "quanteda","topicmodels","quanteda.textmodels",
     "quanteda.textplots","reticulate","here")

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
# whatever somewhere in here.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Text Data                                         ####
# 
# Reading plain-text from a local file:

GBA<-read_file("~/Dropbox/PLSC 504/Data/GBA.txt")
GBA<-stripWhitespace(GBA)
GBA

# Same thing, but a PDF:

GBA.2<-pdf_text("~/Dropbox/PLSC 504/Data/GBA.pdf")
GBA.2

# Note to scolds: This code *will not* work without 
# changing the filepath. That's the point.
#
# Plain-text Gettysburg Address from the Github repo:

GBA.3<-read_file("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/GBA.txt")
GBA.3

# And the same from the web-version of the PDF:

GBA.4<-pdf_text("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/GBA.pdf")
GBA.4

# Getting data from a standard / HTML webpage is
# often a bit more complex; see (e.g.) the reference
# pages for the -rvest- package, at 
# https://rvest.tidyverse.org/reference/html.html.
#
# Next: getting text from an image file... printed text:

eng<-tesseract("eng")
GBA.5<-tesseract::ocr("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/GBA.png",
                      engine = eng)
cat(GBA.5)

# Now reading from a hand-written image file:

GBA.6<-tesseract::ocr("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/GBA-CZ.png",
                       engine = eng)
cat(GBA.6)

# That is hilariously wrong.
#
# Next: Doing more than one document at a time. We'll 
# illustrate this with the text of the speeches from
# the famous Lincoln-Douglas debates, which appear in
# PDF form on the workshop Github repository.
#
# Get filenames:

req<-GET("https://api.github.com/repos/PrisonRodeo/GSERM-RFP-2024/contents/Data/LD-Debates")
files<-httr::content(req)
fnames<-sapply(files,function(x) x$name)
fnames

# Loop over file name list to get files; name each file
# "LD#" where "#" is the number of the file (so they'll 
# end up read in alphabetically):

for(i in 1:length(fnames)){
  fn<-paste0("https://github.com/PrisonRodeo/GSERM-RFP-2024/raw/main/Data/LD-Debates/",
             fnames[i])
  f<-pdf_text(fn)
  out<-paste0("LD",i)
  assign(out,f)
  rm(f)
}

# We now have seven text objects, labeled "LD1" to "LD7," 
# each with the text of one of the Lincoln-Douglas 
# debates.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Working With Text                               ####
#
# Various tools for processing text.... this illustrates
# how we can work with and manage text using basic 
# commands in R.
#
# Make a character object:

GBAC<-"It is rather for us to be here dedicated to the 
       great task remaining before us -- that from these 
       honored dead we take increased devotion to that 
       cause for which they gave the last full measure of 
       devotion -- that we here highly resolve that these 
       dead shall not have died in vain -- that this nation, 
       under God, shall have a new birth of freedom -- 
       and that government of the people, by the people, 
       for the people, shall not perish from the earth."

GBAC

# Some basic operations... Make everything lower-case:

tolower(GBAC)

#... and upper-case:

toupper(GBAC)

# and "title case":

str_to_title(GBAC)

# Replace characters (ex: "a" with "A"):

chartr("a","A",GBAC)

# Punctuation removal:

removePunctuation(GBAC)

# Remove the words "us" and "that":

removeWords(GBAC, c("us","that"))

# Strip whitespace:

stripWhitespace(GBAC)

# Next, we'll taker the entire Gettysburg address
# and "tokenize" it, breaking it into sentences:

GBA<-stripWhitespace(GBA)

GBA.sent<-tokenize_sentences(GBA)
GBA.sent

# How many sentences?

length(GBA.sent[[1]])

# Tokenize words:

GBA.words <- tokenize_words(GBA)
GBA.words
length(GBA.words[[1]]) # total word count

# Turn sentences into a nested list of words:

GBA.sw <- tokenize_words(GBA.sent[[1]])
GBA.sw

# Count words per sentence:

sapply(GBA.sw, length)

# Stop-word removal:

removeWords(GBA,stopwords("en"))

# Stemming:

stemDocument(GBA)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# TF-IDF things...
#
# OK, cool. Now we talk about TF-IDF in the slides. Here
# are the toy example documents:

A<-"red blue red"
B<-"green blue orange"
C<-"yellow blue yellow"

# Term-Document matrix:

TDM<-TermDocumentMatrix(c(A,B,C))
TDM2<-as.matrix(TDM)
TDM2

# TF-IDF

TFIDF<-as.matrix(weightTfIdf(TDM))
TFIDF

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, let's work with something bigger, albeit
# from a similar era: "The Adventures of Huckleberry 
# Finn":

AHF<-pdf_text("https://contentserver.adobe.com/store/books/HuckFinn.pdf")

# Clean up that document...
#
# First: remove everything that isn't an
# alphanumeric symbol:

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
AHF<-removeSpecialChars(AHF)

# Now text processing:

AHF.C<-VCorpus(VectorSource(AHF)) # create a "corpus"
AHF.C<-tm_map(AHF.C,content_transformer(tolower))
AHF.C<-tm_map(AHF.C,content_transformer(removeNumbers))
AHF.C<-tm_map(AHF.C,content_transformer(removePunctuation))
AHF.C<-tm_map(AHF.C,removeWords,stopwords('en'))
AHF.C<-tm_map(AHF.C,stripWhitespace)
AHF.C<-tm_map(AHF.C,removeWords,"H U C K L E B E R R Y F I N N") # strip page headings

# TDM --> 100 most common words:

AHF.TDM<-TermDocumentMatrix(AHF.C)
findFreqTerms(AHF.TDM,100,Inf)

# Barplot of the most frequently-used 50 words:

AHF.freqs<-rowSums(as.matrix(AHF.TDM))
HiFreq<-sort(tail(sort(AHF.freqs),40))

pdf("WordFreqs.pdf",7,6)
par(mar=c(4,4,2,2))
barplot(HiFreq,las=2,cex.names=0.6,
        xlab="Words",ylab="Frequency")
abline(h=c(100,200,300,400,500),lty=2)
dev.off()

# So now... Let's make a word cloud! That's always kinda 
# fun (in a completely nerdy way)...

pdf("AHF-Wordcloud.pdf",7,6) 
set.seed(7222009) 
wordcloud(AHF,min.freq=40,random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))
dev.off() 

# TF-IDF (for word importance...)

AHF.TFIDF<-weightTfIdf(AHF.TDM)
str(AHF.TFIDF)
M<-as.matrix(AHF.TFIDF)
M2<-data.frame(M)

# Now we can make a simple line plot of the importance
# of different words over each page of the book:

pdf("AHF-TFIDF-Plot.pdf",8,6)
par(mar=c(4,4,2,2))
plot(M[rownames(M)=="river"],t="l",lwd=2,
     ylim=c(0,0.12),xlab="Page",ylab="Word Importance (TF-IDF)")
lines(M[rownames(M)=="jim"],t="l",lwd=2,lty=2,col="orange")
lines(M[rownames(M)=="duke"],t="l",lwd=2,lty=3,col="navy")
lines(M[rownames(M)=="grangerford"],t="l",lwd=2,lty=4,col="green")
lines(M[rownames(M)=="pap"],t="l",lwd=2,lty=5,col="red")
legend("topright",bty="n",lwd=2,lty=c(1,2,3,4,5),
       col=c("black","orange","navy","green","red"),
       legend=c("\"River\"","\"Jim\"","\"Duke\"",
                "\"Grangerford\"","\"Pap\""))
dev.off()

# \fin