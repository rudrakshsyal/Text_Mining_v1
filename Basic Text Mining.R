setwd("../../BI Shared Folder/Rudraksh/Data Mining/2. Basic Text Mining in R")

#Installing Packages ----

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = T)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#--------------------- Data Upload & Clean-up ------------------------------------------

#Loading Texts ----

cname <- ("../Text Cleanup/v14 - Applying Final Model/Lists/Word_Blurbs/")
cname

# f4 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f4-21,854.csv")
# f5 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f5-22,243.csv")
# f6 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f6-23,139.csv")
# f7 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f7-21,535.csv")
# f8 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f8-21,839.csv")
# f9 <- read.csv("../Text Cleanup/v14 - Applying Final Model/f9-22,049.csv")
# 
# f4 <- f4[,4]
# f5 <- f5[,4]
# f6 <- f6[,4]
# f7 <- f7[,4]
# f8 <- f8[,4]
# f9 <- f9[,4]
# 
# write.csv(f4, "../Text Cleanup/v14 - Applying Final Model/Lists/t4.csv", row.names = F)
# write.csv(f5, "../Text Cleanup/v14 - Applying Final Model/Lists/t5.csv", row.names = F)
# write.csv(f6, "../Text Cleanup/v14 - Applying Final Model/Lists/t6.csv", row.names = F)
# write.csv(f7, "../Text Cleanup/v14 - Applying Final Model/Lists/t7.csv", row.names = F)
# write.csv(f8, "../Text Cleanup/v14 - Applying Final Model/Lists/t8.csv", row.names = F)
# write.csv(f9, "../Text Cleanup/v14 - Applying Final Model/Lists/t9.csv", row.names = F)



dir(cname)

library(tm)
docs <- Corpus(DirSource(cname))
summary(docs)
inspect(docs[2])
meta(docs[1])

#Preprocessing ----

#Removing punctuation ---
docs <- tm_map(docs, removePunctuation)
inspect(docs[3])
# meta(docs[1])
# print(docs[1])
# inspect(docs[1])
# lapply(docs, as.character)
# writeLines(as.character(docs[1]))

#Removing Special characters ---
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}   
inspect(docs[1])

#Removing Numbers ---
docs <- tm_map(docs, removeNumbers)
inspect(docs[2])

#Converting to lowercase ---
docs <- tm_map(docs, tolower)
inspect(docs[1])

#Removing Stopwords ---
docs <- tm_map(docs, removeWords, stopwords("english"))
inspect(docs[1])
# length(stopwords("english"))
# stopwords("english") - Careful!!!!! - Has "very", "not", "too", etc. which might have helped in Sentiment Analysis.

#Removing particular Words ---
docs <- tm_map(docs, removeWords, c("null"))
inspect(docs[1])

#Combining words that should stay together ---
for (j in seq(docs))
{
  docs[[j]] <- gsub("crude oil", "CO", docs[[j]])
  docs[[j]] <- gsub("oil prices", "OP", docs[[j]])
  docs[[j]] <- gsub("market", "mkt.", docs[[j]])
}
inspect(docs[1])

#Stemming Document (Removing common word endings) --- (eg. "ing", "es", "s")
library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs[1])

#Stripping Unnecessary White Spaces ---
docs <- tm_map(docs, stripWhitespace)
inspect(docs[1])

#Final step of Preprocessing - treat "docs" as text documents ---
docs <- tm_map(docs, PlainTextDocument)
inspect(docs[1])

#Stage the Data ----

docs <- Corpus(DirSource(docs))
summary(docs)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("null"))
docs <- tm_map(docs, stripWhitespace)

corp <- Corpus(VectorSource(docs))


docs <- unlist(docs)
word_count <- table(docs)
big_wc <- data.frame(word_count)


#Create a document term matrix ---
dtm <- DocumentTermMatrix(docs)
# dtm
# inspect(dtm)
inspect(dtm[1:2, 1:20]) #No. of Documents, First 20 word's frequency
dim(dtm) # Total Documents, Total Terms
findFreqTerms(dtm, 10) # Words of Frequency 10 in the document
findAssocs(dtm, "act", 0.90) # Associated words of "act" with 0.90 accuracy

#Transpose of Matrix ---
tdm <- TermDocumentMatrix(docs)
tdm
inspect(tdm)
inspect(tdm[1:10, 1:2]) #First 10 word's frequency, No. of Documents

#Explore the Data ----

freq <- colSums(as.matrix(dtm)) #Organise the terms by their frequency
length(freq)
ord <- order(freq)

#Export the matrix to Excel ---
m <- as.matrix(tdm)
dim(m)
write.csv(m, "../tdm.csv")

#Focus!! ----

#Removing Sparse terms ---
dtms <- removeSparseTerms(dtm, 0.7) #Makes a matrix that is 70% empty space, maximum.
dtms
inspect(dtms)
inspect(dtms[1:4,])

#Word Frequency ----
freq[head(ord)] #Least frequently occuring words
freq[tail(ord)] #Most frequently occuring words

head(table(freq), 20) #Check the frequency of frequencies
#The resulting output is two rows of numbers. The top number is the frequency with which words appear and the bottom number reflects how many words appear that frequently. Here, considering only the 20 lowest word frequencies, we can see that 589 terms appear only once. There are also a lot of others that appear very infrequently.

tail(table(freq), 20) #Check the frequency of frequencies
#Considering only the 20 greatest frequencies, we can see that there is a huge disparity in how frequently some terms appear.

freq <- colSums(as.matrix(dtms))
freq
#For a less, fine-grained look at term freqency we can view a table of the terms we selected when we removed sparse terms, above. (Look just under the word "Focus".)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)

findFreqTerms(dtm, lowfreq = 15)

wf <- data.frame(word=names(freq),freq=freq)
wf
head(wf)

#Plot Word Frequencies ----
library(ggplot2)   
p <- ggplot(subset(wf, freq>15), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

#--------------------- Relationship between Terms --------------------------------------

#Term Correlations ----
findAssocs(dtm, c("oil", "prices"), corlimit = 0.70) #specifying a correlation limit of 0.70
findAssocs(dtm, c("opec"), corlimit = 0.85) #specifying a correlation limit of 0.85

#Word Clouds! ----
library(wordcloud)

#Criteria of Min. Frequency ---
set.seed(142)
wordcloud(names(freq), freq, min.freq = 15)
wordcloud(names(freq), freq, min.freq = 10)
#Note: The set.seed() function just makes the configuration of the layout of the clouds consistent each time you plot them. You can omit that part if you are not concerned with preserving a particular layout.

#Criteria of Max. Words ---
set.seed(142)
wordcloud(names(freq), freq, max.words=100)
wordcloud(names(freq), freq, max.words=70)

#Add Some colour and plot words occuring at least 10 times ---
set.seed(142)
wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, max.words = 70, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#--------------------- Clustering by Term Similarity -----------------------------------

#Remove Sparse Terms ----
dtmss <- removeSparseTerms(dtm, 0.75) #Matrix that has only 75% empty space, maximum.   
inspect(dtmss)
inspect(dtmss[1:5,])

#Hierarchial Clustering ----
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit
plot(fit, hang = -1)

#Reading the Cluster Dendrogram ---
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters

#K-Means Clustering ----
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

#---------------------------------------------------------------------------------------




