###########################################################################
#
# WORD CLOUD TRIP ADVISOR
#
# Inspired by: Print a word cloud with opinions
# 
# Author: Ana Valdivia
# Date: June 2016
###########################################################################
install.packages("RXKCD")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("NLP")
install.packages("data.table")
install.packages("SnowballC")


# Packages & Libraries
library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(data.table)
library(SnowballC)


# Merge all opinions
# Load data
Messages <- read.csv("./data/Dataset.csv")

# **********************************************************************************
# Function for bag of words
# **********************************************************************************

# FREQUENCY
word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word frequency df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

# **********************************************************************************
# **********************************************************************************

# TF*IDF
word.tfidf <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus,
                                control = list(stopwords = stopwords("SMART"), stemming=TRUE, removePunctuation = TRUE, removeNumbers = TRUE, weighting = function(x) weightTfIdf(x, normalize = FALSE)))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  docTerm.df <- as.data.frame(temp.tf)
  # construct word frequency df
  freq.df <- colMeans(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  list(Freq = freq.df, Temp = docTerm.df)
}

# **********************************************************************************
# **********************************************************************************


for( i in levels(Messages$Corpus))
{
  word.freq.pos <- word.freq(Messages$Text[Messages$Corpus == i])
  word.freq.neg <- word.freq(Messages$Text[Messages$Corpus == i])
  word.freq.neutral <- word.freq(Messages$Text[Messages$Corpus == i])
  
  word.freq.pos <- as.data.table(word.freq.pos)
  word.freq.pos <- word.freq.pos[order(freq, decreasing = TRUE),]
  word.freq.neg <- as.data.table(word.freq.neg)
  word.freq.neg <- word.freq.neg[order(freq, decreasing = TRUE),]
  word.freq.neutral <- as.data.table(word.freq.neutral)
  word.freq.neutral <- word.freq.neutral[order(freq, decreasing = TRUE),]
  
  # NSDI
  # merge by word
  freq.all <- merge(word.freq.neg, word.freq.pos, by = "word", all = T)
  # clean up
  freq.all$freq.x[is.na(freq.all$freq.x)] <- 0
  freq.all$freq.y[is.na(freq.all$freq.y)] <- 0
  # compute difference
  freq.all$diff <- abs(freq.all$freq.x - freq.all$freq.y)
  
  #smoothing term
  alpha <- 150
  freq.all$ndsi <- abs(freq.all$freq.x - freq.all$freq.y)/(freq.all$freq.x + freq.all$freq.y + 2*alpha)
  
  word.tfidf.pos <- word.tfidf(Messages$Text[Messages$Corpus == i])$Freq
  word.tfidf.neg <- word.tfidf(Messages$Text[Messages$Corpus == i])$Freq
  word.tfidf.neutral <- word.tfidf(Messages$Text[Messages$Corpus == i])$Freq
  
  word.tfidf.pos <- as.data.table(word.tfidf.pos)
  word.tfidf.pos <- word.tfidf.pos[order(freq, decreasing = TRUE),]
  word.tfidf.neg <- as.data.table(word.tfidf.neg)
  word.tfidf.neg <- word.tfidf.neg[order(freq, decreasing = TRUE),]
  word.tfidf.neutral <- as.data.table(word.tfidf.neutral)
  word.tfidf.neutral <- word.tfidf.neutral[order(freq, decreasing = TRUE),]
  
  
   # *******************************************************************************************
  
  # NSDI
  # merge by word
  freq.all <- merge(word.tfidf.neg, word.tfidf.pos, by = "word", all = TRUE)
  # clean up
  freq.all$freq.x[is.na(freq.all$freq.x)] <- 0
  freq.all$freq.y[is.na(freq.all$freq.y)] <- 0
  # compute difference
  freq.all$diff <- abs(freq.all$freq.x - freq.all$freq.y)
  
  #smoothing term
  alpha <- 150
  freq.all$ndsi <- abs(freq.all$freq.x - freq.all$freq.y)/(freq.all$freq.x + freq.all$freq.y + 2*alpha)
  
  # Select tf*idf pos and neg
  wordNeg <- merge(word.tfidf.neg, word.freq.neg, by="word", all = TRUE)
  wordPos <- merge(word.tfidf.pos, word.freq.pos, by="word", all = TRUE)
  wordNeutral <- merge(word.tfidf.neutral, word.freq.neutral, by="word", all = TRUE)
  
  setnames(wordNeg, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
  setnames(wordPos, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
  setnames(wordNeutral, old=c("freq.x", "freq.y"), new=c("tfidf", "freq"))
  
  # Delete STOPWORDS
  wordNeg <- wordNeg[!(wordNeg$word %in% stopwords("SMART"))]
  wordPos <- wordPos[!(wordPos$word %in% stopwords("SMART"))]
  wordNeutral <- wordNeutral[!(wordNeutral$word %in% stopwords("SMART"))]
  
  # Order and select most 500 popular words
  wordNeg <- wordNeg[order(tfidf, decreasing = TRUE),]
  wordNeg10 <- wordNeg[1:10,]
  wordPos <- wordPos[order(tfidf, decreasing = TRUE),]
  wordPos10 <- wordPos[1:10,]
  wordNeutral <- wordNeutral[order(tfidf, decreasing = TRUE),]
  
  wordNegPos10 <- merge(wordNeg10, wordPos10, by = "word", all = TRUE)
  setnames(wordNegPos10, old=c("tfidf.x", "freq.x", "tfidf.y", "freq.y"), 
           new=c("tfidfNeg", "freqNeg", "tfidfPos", "freqPos"))
  wordNegPos10 <- wordNegPos10[order(tfidfNeg, decreasing = TRUE),]
  wordNegPos10Vector <- wordNegPos10$word                
  
  wordNegSelect <- wordNeg[!(wordNeg$word %in% wordPos$word),]
  wordPosSelect <- wordPos[!(wordPos$word %in% wordNeg$word),]
  wordCommonPosNeg <- wordNeg[(wordNeg$word %in% wordPos$word),]
  
  write.table(wordPos, file = toString(paste("./data/word", i ,".csv", sep = " ")), row.names = FALSE, sep = ",")  

  # plot wordcloud
  #NEG
  # name_png <- c("Negative image ", i, ".png")
  # nombre <- toString(name_png)
  # png(nombre, width=12, height=8, units="in", res=300)
  # 
  # wordcloud(wordNeg$word, wordNeg$freq, scale=c(5.5,0.5), min.freq = 3, max.words = 100, random.order = FALSE, rot.per=0.15, 
  #       use.r.layout=FALSE, colors=brewer.pal(6, "Set1"))
  # dev.off()
  #POS
  
  # name_png <- toString(paste("./imagenes", i ,".png", sep = " "))
  # nombre <- toString(name_png)
  # png(nombre, width=12, height=8, units="in", res=300)
  # 
  # wordcloud(wordPos$word, wordPos$tfidf, max.words=100, scale=c(5.5,0.5), random.order=FALSE,
  #          rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))
  # dev.off()
  
}

