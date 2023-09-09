
library(tidyr)               
library(tidytext)            
       
library(dplyr)              
library(stringr)  
            
library(ggplot2)             
library("topicmodels")
library(reshape2)    


setwd("C:/Users/Nicole/Desktop/dataset")

# review=read.csv("product reviews.csv")

review=read.csv("product reviews.csv", stringsAsFactors = FALSE)

#review=read_csv("product reviews.csv")

#review_docs=tibble(line = 1:length(review$ReviewText), text=review$ReviewText)

review_docs=tibble(line = 1:length(review$ReviewText), text=review$ReviewText)

word_review=review_docs%>%
   unnest_tokens(word, text) 

word_fre=word_review%>%
 count(word,sort=TRUE)%>%
 anti_join(stop_words) 

head(word_fre,20)

library(wordcloud)# pacakage for wordcloud

wordcloud(word_fre$word, word_fre$n, col=terrain.colors(length(word_fre$word), alpha=0.9), random.order=FALSE)# add color



nrc <- get_sentiments("bing")  #get sentiment words from NRC emotion lexicon; note: when you first use this function, you may be asked to agree with reference. For this, you just select "1".

nrc

docs<-word_review%>%
   inner_join(nrc)%>%
   count(sentiment,sort=TRUE) 
 
ggplot(docs, aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE)  



#########################################



