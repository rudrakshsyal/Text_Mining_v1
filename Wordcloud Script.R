SS <- read.csv("../../../../../../FINAL.csv")
qwerty2 <- subset(SS, SS$type == "sales_blockers_text" | SS$type == "extra_notes")
qwerty2$X <- qwerty2$interaction_id <- qwerty2$word <- qwerty2$bi_gram <- qwerty2$tri_gram <- NULL
# temp1 <- qwerty2
temp1 <- subset(qwerty2, qwerty2$type == "sales_blockers_text")
temp2 <- subset(qwerty2, qwerty2$type == "extra_notes")
temp1$type <-temp1$correct <-  NULL
temp2$type <-temp2$correct <-  NULL

library(tm)
# qwerty <- subset(SS, SS$Freq. > 10)
# qwerty$X <- qwerty$AP <- qwerty$EN <- qwerty$SBT <- qwerty$SP <- qwerty$Freq. <- NULL
corp <- Corpus(VectorSource(temp1))
summary(corp2)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, c("-"))
corp <- tm_map(corp, stripWhitespace)


dtm <- DocumentTermMatrix(corp)
tdm <- TermDocumentMatrix(corp)
temp <- as.matrix(tdm)
# write.csv(temp, "../temp.csv")
freq <- as.matrix(dtm)
length(freq)
ord <- order(freq)

freq <- colSums(as.matrix(dtm))
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
findFreqTerms(dtm, lowfreq = 15)
wf <- data.frame(word=names(freq),freq=freq)
wf
head(wf)
library(ggplot2)   
p <- ggplot(subset(wf, freq>15), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

library(wordcloud)
#Criteria of Min. Frequency ---
set.seed(142)
wordcloud(names(freq), freq, min.freq = 15)
wordcloud(names(freq), freq, min.freq = 5)

set.seed(142)
wordcloud(names(freq), freq, max.words=100)
wordcloud(names(freq), freq, max.words=150)

set.seed(142)
wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, max.words = 250, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))



