library(RecordLinkage)
library(hunspell)
library(tm)

#### Preparing word corpus to train the text cleaning model -----

# Reading big.txt to get all english words
raw_text <- paste(readLines("../Norvig Corpus - Big.txt/Norvig_big.txt"), collapse = " ")

# Substituting n't with nt to correct for words like couldn't, didn't
raw_text <- gsub("n't","nt",raw_text)

# Splitting the sentences into individual words
split_text <- strsplit(tolower(raw_text), "[^a-z]+")

# Calculating word occurrence frequency in big.txt
word_count <- table(split_text)
big_wc <- data.frame(word_count)

# Importing Practo specific words with artificial frequency to be added to big.txt
p_dump <- read.csv("../Practo_Words/Practo_Corpus(2).csv", stringsAsFactors = FALSE)
p_wc <- p_dump[,c("Name", "Freq")]
colnames(p_wc) <- c("split_text", "Freq")

# Merging the two word lists to make final training data-set
wc <- rbind(big_wc, p_wc)
colnames(wc) <- c("Words", "Frequency")
wc$Words <- tolower(wc$Words)
wc <- unique(wc)
sorted_words <- wc

# Arranging words in decreaseing order of their occurrence
sw <- sorted_words[order(-sorted_words$Frequency),]
sorted_words <- as.character(sw[,1]) #Preparing list of words from dataframe

#### Writing the model function to correct words in the text -----
correct_final <- function(word)
{
  # Calculating distance of word with each word from training set
  edit_dist <- levenshteinDist(word, sorted_words)
  # Taking all words where distance is favourable (maximum allowed = 2)
  proposals_by_prob <- c(sorted_words[edit_dist <= min(edit_dist, 2)])
  # Including "Not Found" flag for words unknown to training set
  proposals_by_prob <- c(proposals_by_prob, "Not Found")
  # Running Hunspell suggest function & picking the 1st word suggested on unknown words
  print(ifelse(proposals_by_prob[1] == "Not Found", (hunspell_suggest(word))[[1]][1], proposals_by_prob[1]))
}

#### Importing all interactions data -----
dump <- read.csv("../Cosmic_Corpus/FINAL_Interactions_DATA.csv")
dump$PSW.Email <- NULL

# Solutions-provided
temp <- dump[c(2,3)]
s <- strsplit(tolower(temp$Solutions.Provided), "[^a-z]+")
# Converting list to dataframe after splitting sentences
SP <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
SP$type <- "solutions_provided"

# Sales-blockers-text
temp <- dump[c(2,4)]
s <- strsplit(tolower(temp$Sale.Blockers.Text), "[^a-z]+")
# Converting list to dataframe after splitting sentences
SBT <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
SBT$type <- "sales_blockers_text"

# Sale-Blockers
temp <- dump[c(2,5)]
s <- strsplit(tolower(temp$Sale.Blockers), "[^a-z]+")
# Converting list to dataframe after splitting sentences
SB <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
SB$type <- "sales_blockers"

# Needs-Identified
temp <- dump[c(2,6)]
s <- strsplit(tolower(temp$Needs.Identified), "[^a-z]+")
# Converting list to dataframe after splitting sentences
NI <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
NI$type <- "needs_identified"

# Action-Plan
temp <- dump[c(2,7)]
s <- strsplit(tolower(temp$Action.Plan), "[^a-z]+")
# Converting list to dataframe after splitting sentences
AP <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
AP$type <- "action_plan"

# Remarks
temp <- dump[c(2,8)]
s <- strsplit(tolower(temp$Remarks), "[^a-z]+")
# Converting list to dataframe after splitting sentences
R <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
R$type <- "remarks"

# Extra-Notes
temp <- dump[c(2,9)]
s <- strsplit(tolower(temp$Extra.Notes), "[^a-z]+")
# Converting list to dataframe after splitting sentences
EN <- data.frame(interaction_id = rep(temp$Interaction.ID, sapply(s, length)), word = unlist(s))
EN$type <- "extra_notes"

# Merging all components to buil final list of words to be corrected
dump <- rbind(SP, SBT, SB, NI, AP, R, EN)
# Converting to vector form
s <- as.character(dump[['word']])

all_words <- unlist(s,recursive=F)
# Taking only words where exact match is not found with training set
remaining <- setdiff(all_words,sorted_words)
correct <- data.frame(remaining)

# Applying the correct function on all such words -----
system.time(model <- sapply(remaining, correct_final))
correct[,2] <- model

#### Indexing the corrected words back to the initial set of interaction data -----
dump$correct <- correct$V2[match(dump$word, correct$remaining)]
dump$correct[is.na(dump$correct)] <- as.character(dump$word)[is.na(dump$correct)]
# write.csv(dump,"../CLEAN.csv")

qwerty3 <- dump

write.csv(qwerty3, "../../../../../../FINAL_CLEAN.csv")

# qwerty <- read.csv("../../../../../../CLEAN (2).csv")

action_plan <- subset(qwerty3, qwerty3$type == "action_plan" & qwerty3$word != "null")
extra_notes <- subset(qwerty3, qwerty3$type == "extra_notes" & qwerty3$word != "null")
sales_blockers <- subset(qwerty3, qwerty3$type == "sales_blockers" & qwerty3$word != "null")
sales_blockers_text <- subset(qwerty3, qwerty3$type == "sales_blockers_text" & qwerty3$word != "null")
remarks <- subset(qwerty3, qwerty3$type == "remarks" & qwerty3$word != "null")
solutions_provided <- subset(qwerty3, qwerty3$type == "solutions_provided" & qwerty3$word != "null")
needs_identified <- subset(qwerty3, qwerty3$type == "needs_identified" & qwerty3$word != "null")

write.csv(action_plan, "../../../../../../FINAL_AP.csv")
write.csv(extra_notes, "../../../../../../FINAL_EN.csv")
write.csv(sales_blockers, "../../../../../../FINAL_SB.csv")
write.csv(sales_blockers_text, "../../../../../../FINAL_SBT.csv")
write.csv(remarks, "../../../../../../FINAL_R.csv")
write.csv(solutions_provided, "../../../../../../FINAL_SP.csv")
write.csv(needs_identified, "../../../../../../FINAL_NI.csv")

docs1 <- action_plan[,5]
docs2 <- extra_notes[,5]
docs3 <- sales_blockers[,4]
docs4 <- sales_blockers_text[,5]
docs5 <- remarks[,4]
docs6 <- solutions_provided[,5]
docs7 <- needs_identified[,4]

word_count1<- table(docs1)
word_count2<- table(docs2)
word_count3<- table(docs3)
word_count4<- table(docs4)
word_count5<- table(docs5)
word_count6<- table(docs6)
word_count7<- table(docs7)

big_wc1 <- data.frame(word_count1)
big_wc2 <- data.frame(word_count2)
big_wc3 <- data.frame(word_count3)
big_wc4 <- data.frame(word_count4)
big_wc5 <- data.frame(word_count5)
big_wc6 <- data.frame(word_count6)
big_wc7 <- data.frame(word_count7)

write.csv(big_wc1, "../final_ap.csv")
write.csv(big_wc2, "../final_en.csv")
write.csv(big_wc3, "../final_sb.csv")
write.csv(big_wc4, "../final_sbt.csv")
write.csv(big_wc5, "../final_r.csv")
write.csv(big_wc6, "../final_sp.csv")
write.csv(big_wc7, "../final_ni.csv")

docs <- qwerty3[,5]
word_count<- table(docs)
big_wc <- data.frame(word_count)
write.csv(big_wc, "../Term-frequencies/v4 - bi-gram tf/final.csv")

# action_plan2 <- action_plan
# action_plan2$word <- action_plan2$type <- action_plan2$X <- NULL
# 
# library(reshape)
# tuna <- melt(action_plan2,id.vars = c("interaction_id", "correct"))
# cast(tuna, interaction_id~correct)
# 
# i=1
# intr <- function(id)
# {
#   i=1
#   if(action_plan2$interaction_id[i] == action_plan2$interaction_id[i+1]){
#     temp <- subset(action_plan2, action_plan2$interaction_id == action_plan2$interaction_id[i])
#     temp1 <- list(temp$interaction_id)
#     temp2 <- list(temp$correct)
#     temp3 <- temp[[2]]
#     temp3 <- as.character(temp3)
#     
#     
#   } else {
#     i=i+1
#   }
#   
#   
#   temp[action_plan2$interaction_id[i] == action_plan2$interaction_id[i+1]] <- action_plan2$correct[action_plan2$interaction_id[i] == action_plan2$interaction_id[i+1]]
# }
# 
# 
# 

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("null"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)



tdm <- TermDocumentMatrix(docs)
m <- as.matrix(tdm)
dim(m)
write.csv(m, "../tdm.csv")

correct_final("sanjeevini")

