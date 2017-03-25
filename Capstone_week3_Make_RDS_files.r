rm(list = ls())
library(stringr)
library(stringi)
library(tm)
library(RWeka)

source("Capstone_week3_functions.R")
#if (!file.exists("Coursera-SwiftKey.zip")) {download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")}
#if (!dir.exists("final")) {unzip("Coursera-SwiftKey.zip")}


readline_twitter<-readLines("U:/Users/BS/R_practice/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
readline_blogs<-readLines("U:/Users/BS/R_practice/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
readline_news<-readLines("U:/Users/BS/R_practice/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
all_data<-c(readline_twitter,readline_blogs,readline_news)

#sampled_raw_data <- c(sample(all_data, length(all_data) * 0.1, replace = FALSE))
sampled_raw_data <- all_data
sampled_data <- iconv(sampled_raw_data, "latin1", "ASCII", sub="")


gram_1 <- make_corpus(sampled_data,1) 
colnames(gram_1)[1] <- "Phrase"
rownames(gram_1) <- 1:dim(gram_1)[1]
head(gram_1)
gram_1<-KNprep(gram_1,1)
saveRDS(gram_1, "gram_1.rds")
rm(gram_1)

gram_2 <- make_corpus(sampled_data,2)
colnames(gram_2)[1] <- "Phrase"
rownames(gram_2) <- 1:dim(gram_2)[1]
gram_2_words <- as.data.frame(str_split_fixed(gram_2$Phrase, " ", 2))
gram_2$Word.1 <- as.character(gram_2_words$V1)
gram_2$Word.2 <- as.character(gram_2_words$V2)
head(gram_2)
gram_2<-KNprep(gram_2,2)
saveRDS(gram_2, "gram_2.rds")
rm(gram_2)

gram_3 <- make_corpus(sampled_data,3)
colnames(gram_3)[1] <- "Phrase"
rownames(gram_3) <- 1:dim(gram_3)[1]
gram_3_words<- as.data.frame(str_split_fixed(gram_3$Phrase, " ", 3))
gram_3$Word.1 <- as.character(gram_3_words$V1)
gram_3$Word.2 <- as.character(gram_3_words$V2)
gram_3$Word.3 <- as.character(gram_3_words$V3)
head(gram_3)
gram_3<-KNprep(gram_3,3)
saveRDS(gram_3, "gram_3.rds")
rm(gram_3)



gram_1 <- make_corpus_nostopword(sampled_data,1) 
colnames(gram_1)[1] <- "Phrase"
rownames(gram_1) <- 1:dim(gram_1)[1]
head(gram_1)
gram_1<-KNprep(gram_1,1)
saveRDS(gram_1, "gram_1_nostopword.rds")
rm(gram_1)

gram_2 <- make_corpus_nostopword(sampled_data,2)
colnames(gram_2)[1] <- "Phrase"
rownames(gram_2) <- 1:dim(gram_2)[1]
gram_2_words <- as.data.frame(str_split_fixed(gram_2$Phrase, " ", 2))
gram_2$Word.1 <- as.character(gram_2_words$V1)
gram_2$Word.2 <- as.character(gram_2_words$V2)
head(gram_2)
gram_2<-KNprep(gram_2,2)
saveRDS(gram_2, "gram_2_nostopword.rds")
rm(gram_2)

gram_3 <- make_corpus_nostopword(sampled_data,3)
colnames(gram_3)[1] <- "Phrase"
rownames(gram_3) <- 1:dim(gram_3)[1]
gram_3_words<- as.data.frame(str_split_fixed(gram_3$Phrase, " ", 3))
gram_3$Word.1 <- as.character(gram_3_words$V1)
gram_3$Word.2 <- as.character(gram_3_words$V2)
gram_3$Word.3 <- as.character(gram_3_words$V3)
head(gram_3)
gram_3<-KNprep(gram_3,3)
saveRDS(gram_3, "gram_3_nostopword.rds")
rm(gram_3)



gram_1 <- make_corpus_nostopword_stemmed(sampled_data,1) 
colnames(gram_1)[1] <- "Phrase"
rownames(gram_1) <- 1:dim(gram_1)[1]
head(gram_1)
gram_1<-KNprep(gram_1,1)
saveRDS(gram_1, "gram_1_nostopword_stemmed.rds")
rm(gram_1)

gram_2 <- make_corpus_nostopword_stemmed(sampled_data,2)
colnames(gram_2)[1] <- "Phrase"
rownames(gram_2) <- 1:dim(gram_2)[1]
gram_2_words <- as.data.frame(str_split_fixed(gram_2$Phrase, " ", 2))
gram_2$Word.1 <- as.character(gram_2_words$V1)
gram_2$Word.2 <- as.character(gram_2_words$V2)
head(gram_2)
gram_2<-KNprep(gram_2,2)
saveRDS(gram_2, "gram_2_nostopword_stemmed.rds")
rm(gram_2)

gram_3 <- make_corpus_nostopword_stemmed(sampled_data,3)
colnames(gram_3)[1] <- "Phrase"
rownames(gram_3) <- 1:dim(gram_3)[1]
gram_3_words<- as.data.frame(str_split_fixed(gram_3$Phrase, " ", 3))
gram_3$Word.1 <- as.character(gram_3_words$V1)
gram_3$Word.2 <- as.character(gram_3_words$V2)
gram_3$Word.3 <- as.character(gram_3_words$V3)
head(gram_3)
gram_3<-KNprep(gram_3,3)
saveRDS(gram_3, "gram_3_nostopword_stemmed.rds")
rm(gram_3)


#gram_4 <- make_corpus(sampled_data,4)
#colnames(gram_4)[1] <- "Phrase"
#rownames(gram_4) <- 1:dim(gram_4)[1]
#gram_4_words <- as.data.frame(str_split_fixed(gram_4$Phrase, " ", 4))
#gram_4$Word.1 <- as.character(gram_4_words$V1)
#gram_4$Word.2 <- as.character(gram_4_words$V2)
#gram_4$Word.3 <- as.character(gram_4_words$V3)
#gram_4$Word.4 <- as.character(gram_4_words$V4)
#head(gram_4)
#KNprep(gram_4,4)
#saveRDS(gram_4, "gram_4.rds")
#rm(gram_4)

#gram_5 <- make_corpus(sampled_data,5)
#colnames(gram_5)[1] <- "Phrase"
#rownames(gram_5) <- 1:dim(gram_5)[1]
#gram_5_words <- as.data.frame(str_split_fixed(gram_5$Phrase, " ", 5))
#gram_5$Word.1 <- as.character(gram_5_words$V1)
#gram_5$Word.2 <- as.character(gram_5_words$V2)
#gram_5$Word.3 <- as.character(gram_5_words$V3)
#gram_5$Word.4 <- as.character(gram_5_words$V4)
#gram_5$Word.5 <- as.character(gram_5_words$V5)
#head(gram_5)
#KNprep(gram_5,5)
#saveRDS(gram_5, "gram_5.rds")
#rm(gram_5)


