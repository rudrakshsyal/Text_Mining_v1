setwd("../../BI Shared Folder/Rudraksh/Data Mining/2. Basic Text Mining in R")
library(stringr)

##### Loading Cosmic Interaction File -----
dump <- read.csv("../Cosmic_Corpus/Intr_dump2.csv")

dump <- read.csv("../Practo_Words/P232.csv")
df <- data.frame(Input = c(""), Model = c(""), stringsAsFactors = FALSE)
i=j=k=1
for(i in 1:nrow(dump)){
  line <- dump[i,2]
  for(j in 1:length(line[[1]]))
  {
    word <- line[[1]][j]
    df[k,1] <- as.character(word)
    df[k,2] <- correct(word)
    k <- k+1
    print(k)
  }
}

write.csv(df, "../Practo_Words/df.csv")

# split <- unlist(strsplit(tolower(dump$AP...EN...SB...SP), "[^a-z]+"))
# count <- table(split)
# write.csv(count, "../Cosmic_Corpus/count.csv")

##### Creating Random clusters of Interactions (150) -----
cluster1 <- dump[sample(nrow(dump), 150),]

##### Splitting clusters into individuals words mapped to Interaction_ID -----
split_cluster1 <- cluster1$id
split_cluster2 <- strsplit(tolower(cluster1$AP...EN...SB...SP), "[^a-z]+")
split_cluster <- data.frame(cbind(split_cluster1,split_cluster2))
colnames(split_cluster) <- c("Interaction_ID", "Interaction")

##### Creating a df with Interaction_ID, Training Words & Model Corrected Words -----
Interaction_ID <- c("")
Input <- c("")
Model <- c("")
df <- data.frame(Interaction_ID, Input, Model, stringsAsFactors = FALSE)

##### Double Loop for taking each word from interaction and mapping model corrected word along with mapping of Interaction_ID -----

i=j=k=1
for(i in 1:(nrow(split_cluster)))
{
  line <- split_cluster[i,2]
  for(j in 1:length(line[[1]]))
  {
    word <- line[[1]][j]
    df[k,1] <- split_cluster[i,1]
    df[k,2] <- word
    df[k,3] <- correct(word)
    k <- k+1
    # if(k%%100 == 0)
    {print(paste0(i," - ",k, " - ", Sys.time() ))}
  }
}

##### Writing the file for Testing & Correction purposes -----
write.csv(df, file = "df1.csv")

#-------------------------------------------Training------------------------------------------------

# Preprocessing & Clean-up of Raw Data ----

# Read in big.txt, a 6.5 mb collection of different English texts.
raw_text <- paste(readLines("../../../../../../Desktop/Norvig_big.txt"), collapse = " ")
# Make the text lowercase and split it up creating a huge vector of word tokens.
raw_text <- gsub("n't","nt",raw_text)
split_text <- strsplit(tolower(raw_text), "[^a-z]+")

# Count the number of different type of words.
word_count <- table(split_text)

big_wc <- data.frame(word_count)
p_dump <- read.csv("../Practo_Words/Practo_Corpus.csv", stringsAsFactors = FALSE)
p_wc <- p_dump[,c("Name", "Freq")]
colnames(p_wc) <- c("split_text", "Freq")
wc <- rbind(big_wc, p_wc)
colnames(wc) <- c("Words", "Frequency")
# word_count <- table(wc)
# write.csv(wc, "./word_count.csv")

# Sort the words and create an ordered vector with the most common type of words first.
sorted_words <- wc
sw <- sorted_words[order(-sorted_words$Frequency),]
sorted_words <- as.character(sw[,1])
# sorted_words <- names(sort(word_count, decreasing = TRUE))

write.csv(sorted_words, "../sw.csv")

correct <- function(word) {
  # Calculate the edit distance between the word and all other words in sorted_words.
  edit_dist <- adist(word, sorted_words)
  # Calculate the minimum edit distance to find a word that exists in big.txt 
  # with a limit of two edits.
  min_edit_dist <- min(edit_dist, 2)
  # Generate a vector with all words with this minimum edit distance.
  # Since sorted_words is ordered from most common to least common, the resulting
  # vector will have the most common / probable match first.
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
  # In case proposals_by_prob would be empty we append the word to be corrected...
  proposals_by_prob <- c(proposals_by_prob, "UNKNOWN")
  # ... and return the first / most probable word in the vector.
  proposals_by_prob[1]
}

correct("didnt")
correct("hasnt")
correct("doesnt")
correct("isnt")

#-------------------------------------------Training-------------------------------------------------

#Training the Words (v1) ----

train1 <- c("abouit", "aboout", "abot", "abou", "abougt", "abour", "abouth", "aboutk", "aboyt", "abpout")

correct1 <- data.frame(train1)
train1 <- data.frame(train1)
len <- length(train1[,1])

for(i in 1:len)
{
  correct1[i,2] <- correct(train1[i,1])
}

#Training the Words (v2) ----

inspect(tdm[,1:1])
q <- as.matrix(tdm[,1:1])
dim(q)
write.csv(q, file = "cosmic_tdm.csv")

train2 <- read.csv("./cosmic_tdm.csv")
colnames(train2) <- c("Train_Words", "Freq-AP", "Freq-EN","Freq-SB","Freq-SP")
train2$Correct_Words <- 0
len <- length(train2[,1])
correct2 <- train2

for(i in 1:len)
{
  print(i)
  correct2[i,6] <- correct(train2[i,1])
}

write.csv(correct2, file = "cosmic_correct2.csv")

#Training the Words (v3) ----

train2 <- read.csv("./unidentified.csv")
train2$Correct_Words <- 0
len <- length(train2[,1])
correct2 <- train2

for(i in 1:len)
{
  print(i)
  correct2[i,6] <- correct(train2[i,1])
}

write.csv(correct2, file = "cosmic_correct2.csv")








