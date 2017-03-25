shiny.maxRequestSize=50*1024^2

suppressPackageStartupMessages(c(library(markdown),library(stringi),library(RWeka),library(shiny),library(tm),library(stringr)))


gram_1<-readRDS("./gram_1.rds")
gram_2<-readRDS("./gram_2.rds")
gram_3<-readRDS("./gram_3.rds")
#gram_1<-readRDS("./gram_1_nostopword.rds")
#gram_2<-readRDS("./gram_2_nostopword.rds")
#gram_3<-readRDS("./gram_3_nostopword.rds")
#gram_1<-readRDS("./gram_1_nostopword_stemmed.rds")
#gram_2<-readRDS("./gram_2_nostopword_stemmed.rds")
#gram_3<-readRDS("./gram_3_nostopword_stemmed.rds")
source("./Capstone_week3_functions_V04.R")

shinyServer(function(input, output) {
  output$inp <- renderText({input$inpstr})
  output$pred <- renderText({as.character(KN3_predict_withstopwords(gram_1,gram_2,gram_3,input$inpstr)[1,1])})
  #output$pred <- renderText({as.character(KN3_predict_nostopwords(gram_1,gram_2,gram_3,input$inpstr)[1:10,1])})
  #output$pred <- renderText({as.character(KN3_predict_nostopwords_stemmed(gram_1,gram_2,gram_3,input$inpstr)[1:10,1])})
})

