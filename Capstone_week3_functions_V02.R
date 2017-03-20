library(stringr)
library(tm)
library(stringi)
library(RWeka)

make_corpus <- function (textdata,gram){
  temp <- Corpus(VectorSource((textdata)))
  temp <- tm_map(temp, content_transformer(tolower))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, removeNumbers)
  temp <- tm_map(temp, PlainTextDocument)
  #temp <- tm_map(temp, removeWords, stopwords(kind="en"))
  #temp <- tm_map(temp, stemDocument,language = ("english"))
  temp <- tm_map(temp, stripWhitespace)
  temp <- data.frame(sapply(temp,as.character),stringsAsFactors = FALSE)
  temp <- data.frame(table(NGramTokenizer(temp,Weka_control(min=gram, max=gram))))
  temp$Var1 <- as.character(temp$Var1)
  temp$Prediction <- "n/a"
  temp <- temp[order(-temp$Freq),]
}


make_corpus_nostopword <- function (textdata,gram){
  temp <- Corpus(VectorSource((textdata)))
  temp <- tm_map(temp, content_transformer(tolower))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, removeNumbers)
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, removeWords, stopwords(kind="en"))
  #temp <- tm_map(temp, stemDocument,language = ("english"))
  temp <- tm_map(temp, stripWhitespace)
  temp <- data.frame(sapply(temp,as.character),stringsAsFactors = FALSE)
  temp <- data.frame(table(NGramTokenizer(temp,Weka_control(min=gram, max=gram))))
  temp$Var1 <- as.character(temp$Var1)
  temp$Prediction <- "n/a"
  temp <- temp[order(-temp$Freq),]
}

make_corpus_nostopword_stemmed <- function (textdata,gram){
  temp <- Corpus(VectorSource((textdata)))
  temp <- tm_map(temp, content_transformer(tolower))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, removeNumbers)
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, removeWords, stopwords(kind="en"))
  temp <- tm_map(temp, stemDocument,language = ("english"))
  temp <- tm_map(temp, stripWhitespace)
  temp <- data.frame(sapply(temp,as.character),stringsAsFactors = FALSE)
  temp <- data.frame(table(NGramTokenizer(temp,Weka_control(min=gram, max=gram))))
  temp$Var1 <- as.character(temp$Var1)
  temp$Prediction <- "n/a"
  temp <- temp[order(-temp$Freq),]
}

get_last_words <- function(x, n) {
  y <- as.data.frame(strsplit(x, " "))
  colnames(y) <- c("words")
  len <- nrow(y)
  if(len<n) n<-len 
  x <- paste(y[ (len-(n-1)) : len ,], collapse=" ")
  x <- as.character(str_split_fixed(x, " ", n))
  return(x)
}

clean_string <- function(s) {
  s <- Corpus(VectorSource(s))
  s <- tm_map(s, content_transformer(tolower))
  s <- tm_map(s, removePunctuation)
  s <- tm_map(s, removeNumbers)
  s <- tm_map(s, PlainTextDocument)
  #s <- tm_map(s, removeWords, stopwords(kind="en"))
  #s <- tm_map(s, stemDocument,language = ("english"))
  s <- tm_map(s, stripWhitespace)
  s <- data.frame(sapply(s,as.character),stringsAsFactors = FALSE)
  s <- as.character(s)
}


clean_string_nostopwords <- function(s) {
  s <- Corpus(VectorSource(s))
  s <- tm_map(s, content_transformer(tolower))
  s <- tm_map(s, removePunctuation)
  s <- tm_map(s, removeNumbers)
  s <- tm_map(s, PlainTextDocument)
  s <- tm_map(s, removeWords, stopwords(kind="en"))
  #s <- tm_map(s, stemDocument,language = ("english"))
  s <- tm_map(s, stripWhitespace)
  s <- data.frame(sapply(s,as.character),stringsAsFactors = FALSE)
  s <- as.character(s)
}

clean_string_nostopwords_stemmed <- function(s) {
  s <- Corpus(VectorSource(s))
  s <- tm_map(s, content_transformer(tolower))
  s <- tm_map(s, removePunctuation)
  s <- tm_map(s, removeNumbers)
  s <- tm_map(s, PlainTextDocument)
  s <- tm_map(s, removeWords, stopwords(kind="en"))
  s <- tm_map(s, stemDocument,language = ("english"))
  s <- tm_map(s, stripWhitespace)
  s <- data.frame(sapply(s,as.character),stringsAsFactors = FALSE)
  s <- as.character(s)
}


KNprep <- function(dt, n) {
  dt$Prediction<-dt$Freq/sum(dt$Freq)
  Nk<-table(dt$Freq)
  Y<-Nk[1]/(Nk[1]+2*Nk[2])
  Dk<-numeric(0)
  for (i in 1:length(Nk)){
    Dk[i] <- i-(i+1)*Y*Nk[i+1]/Nk[i]
  }
  Nk_df<-as.data.frame(Nk)
  D<-numeric(0)
  for (i in 1:length(dt$Freq)){
    D[i]<-Dk[Nk_df$Var1==dt$Freq[i]]
  }
  dt$D<-D
  Disc_coeff<-numeric(0)
  for (i in 1:length(dt$Freq)){
    Disc_coeff[i]<-(max(dt$Freq[i]-D[i],0))/sum(dt$Freq)
  }
  dt$Fraction<-Disc_coeff
  return(dt)
}



KN3_predict<-function(inputstring,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){
  cleanedinputstring<-clean_string(inputstring)
  inputstring_vec<-get_last_words(cleanedinputstring,2)
  index<-which(gram_3$Word.1==inputstring_vec[1]&gram_3$Word.2==inputstring_vec[2])
  if (length(index)!=0){
    word_pred_list<-rbind(word_pred_list,data.frame(gram_3$Word.3[index],gram_3$Prediction[index]))
    names(word_pred_list)<-c("pred_word","probability")
    N3<-length(index)
    KN3_prob<-gram_3$Fraction[index]+gram_3$D[index]*(N3/sum(gram_3$Freq))*KN2_predict(word_pred_list$pred_word,inputstring_vec[2])
  } 
  word_pred_list$KN3_prob<-KN3_prob
  word_pred_list<-word_pred_list[order(-KN3_prob),]
  return(word_pred_list)
  #else {
    #KN2_predict(inputstring)
#}
}



KN2_predict<-function(pred_string_vec,last_input_word){
KN1_prob<-numeric(0)
KN2_prob<-numeric(0)
index<-which(gram_2$Word.1==last_input_word)
for (i in 1:length(pred_string_vec)){
    index_full<-which(gram_2$Word.1==last_input_word&gram_2$Word.2==pred_string_vec[i])
    N2<-length(index)
    KN1_prob[i]<-sum(gram_2$Word.2==pred_string_vec[i])/length(gram_2$Word.2)
    KN2_prob[i]<-gram_2$Fraction[index_full]+gram_2$D[index_full]*N2/sum(gram_2$Freq)*KN1_prob[i]
}
    return(KN2_prob)

}

KN3_predict_withstopwords<-function(inputstring,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){
  cleanedinputstring<-clean_string(inputstring)
  inputstring_vec<-get_last_words(cleanedinputstring,2)
  index<-which(gram_3$Word.1==inputstring_vec[1]&gram_3$Word.2==inputstring_vec[2])
  if (length(index)!=0){
    word_pred_list<-rbind(word_pred_list,data.frame(gram_3$Word.3[index],gram_3$Prediction[index]))
    names(word_pred_list)<-c("pred_word","probability")
    N3<-length(index)
    KN3_prob<-gram_3$Fraction[index]+gram_3$D[index]*(N3/sum(gram_3$Freq))*KN2_predict_withstopwords(word_pred_list$pred_word,inputstring_vec[2])
    word_pred_list$KN3_prob<-KN3_prob
    word_pred_list<-word_pred_list[order(-KN3_prob),]
    return(word_pred_list)
    
    } else {
    KN2_predict_no_KN3(inputstring_vec[2])
  }
  
}



KN2_predict_withstopwords<-function(pred_string_vec,last_input_word){
  KN1_prob<-numeric(0)
  KN2_prob<-numeric(0)
  index<-which(gram_2$Word.1==last_input_word)
  for (i in 1:length(pred_string_vec)){
    index_full<-which(gram_2$Word.1==last_input_word&gram_2$Word.2==pred_string_vec[i])
    N2<-length(index) #N2<-length(index_full)?????????????????
    KN1_prob[i]<-sum(gram_2$Word.2==pred_string_vec[i])/length(gram_2$Word.2)
    KN2_prob[i]<-gram_2$Fraction[index_full]+gram_2$D[index_full]*N2/sum(gram_2$Freq)*KN1_prob[i]
  }
  return(KN2_prob)
  
}


KN3_predict_nostopwords<-function(inputstring,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){

  cleanedinputstring<-clean_string_nostopwords(inputstring)
  inputstring_vec<-get_last_words(cleanedinputstring,2)
  index<-which(gram_3$Word.1==inputstring_vec[1]&gram_3$Word.2==inputstring_vec[2])
  if (length(index)!=0){
    word_pred_list<-rbind(word_pred_list,data.frame(gram_3$Word.3[index],gram_3$Prediction[index]))
    names(word_pred_list)<-c("pred_word","probability")
    N3<-length(index)
    KN3_prob<-gram_3$Fraction[index]+gram_3$D[index]*(N3/sum(gram_3$Freq))*KN2_predict_nostopwords(word_pred_list$pred_word,inputstring_vec[2])
    
    word_pred_list$KN3_prob<-KN3_prob
    word_pred_list<-word_pred_list[order(-KN3_prob),]
    return(word_pred_list)
  } else {
    KN2_predict_no_KN3(inputstring_vec[2])
  }
}





KN2_predict_nostopwords<-function(pred_string_vec,last_input_word){
  KN1_prob<-numeric(0)
  KN2_prob<-numeric(0)
  index<-which(gram_2$Word.1==last_input_word)
  for (i in 1:length(pred_string_vec)){
    index_full<-which(gram_2$Word.1==last_input_word&gram_2$Word.2==pred_string_vec[i])
    N2<-length(index)
    KN1_prob[i]<-sum(gram_2$Word.2==pred_string_vec[i])/length(gram_2$Word.2)
    KN2_prob[i]<-gram_2$Fraction[index_full]+gram_2$D[index_full]*N2/sum(gram_2$Freq)*KN1_prob[i]
  }
  return(KN2_prob)
  
}



KN3_predict_nostopwords_stemmed<-function(inputstring,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){
  cleanedinputstring<-clean_string_nostopwords_stemmed(inputstring)
  inputstring_vec<-get_last_words(cleanedinputstring,2)
  index<-which(gram_3$Word.1==inputstring_vec[1]&gram_3$Word.2==inputstring_vec[2])
  if (length(index)!=0){
    word_pred_list<-rbind(word_pred_list,data.frame(gram_3$Word.3[index],gram_3$Prediction[index]))
    names(word_pred_list)<-c("pred_word","probability")
    N3<-length(index)
    KN3_prob<-gram_3$Fraction[index]+gram_3$D[index]*(N3/sum(gram_3$Freq))*KN2_predict_nostopwords_stemmed(word_pred_list$pred_word,inputstring_vec[2])
   
  word_pred_list$KN3_prob<-KN3_prob
  word_pred_list<-word_pred_list[order(-KN3_prob),]
  return(word_pred_list)
} else {
  KN2_predict_no_KN3(inputstring_vec[2])
}
}



KN2_predict_nostopwords_stemmed<-function(pred_string_vec,last_input_word){
  KN1_prob<-numeric(0)
  KN2_prob<-numeric(0)
  index<-which(gram_2$Word.1==last_input_word)
  for (i in 1:length(pred_string_vec)){
    index_full<-which(gram_2$Word.1==last_input_word&gram_2$Word.2==pred_string_vec[i])
    N2<-length(index)
    KN1_prob[i]<-sum(gram_2$Word.2==pred_string_vec[i])/length(gram_2$Word.2)
    KN2_prob[i]<-gram_2$Fraction[index_full]+gram_2$D[index_full]*N2/sum(gram_2$Freq)*KN1_prob[i]
  }
  return(KN2_prob)
  
}




#KN2_predict<-function(inputstring,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){
#  inputstring<-"and a case of"
#  cleanedinputstring<-clean_string(inputstring)
#  inputstring_vec<-get_last_words(cleanedinputstring,1)
#  index<-which(gram_2$Word.1==inputstring_vec[1])
#  if (length(index)!=0){
#    word_pred_list<-rbind(word_pred_list,data.frame(gram_2$Word.2[index],gram_2$Prediction[index]))
#    names(word_pred_list)<-c("pred_word","probability")
#    N2<-length(index)
#    KN1_Prob<-sum(gram_2$Word.2==word_pred_list$pred_word)/length(gram_2$Word.2)
#    KN2_Prob<-gram_2$Fraction[index]+gram_2$D[index]*N2/sum(gram_2$Freq)*KN1_Prob
#  } else {
#    KN1_predict(inputstring)
#  }
#}

KN2_predict_no_KN3<-function(last_input_word,word_pred_list=data.frame(data.frame(pred_word=character(0),probability=numeric(0)))){
  last_input_word<-inputstring_vec[2]
  index<-which(gram_2$Word.1==last_input_word)
if (length(index)!=0){
  word_pred_list<-data.frame(gram_2$Word.2[index],gram_2$Prediction[index])
  names(word_pred_list)<-c("pred_word","probability")
  N2<-length(index)
  KN1_prob<-numeric(0)
  for (i in 1:length(index)){
  KN1_prob[i]<-sum(gram_2$Word.2==word_pred_list$pred_word[i])/length(gram_2$Word.2)
  }
  KN2_prob<-gram_2$Fraction[index]+gram_2$D[index]*(N2/sum(gram_2$Freq))*KN1_prob
  word_pred_list$KN2_prob<-KN2_prob
  word_pred_list<-word_pred_list[order(-KN2_prob),]
  return(word_pred_list)
} else {
  word_pred_list<-data.frame(gram_1$Word.1,gram_1$Prediction)
  names(word_pred_list)<-c("pred_word","probability")
  return(word_pred_list)
}
}
