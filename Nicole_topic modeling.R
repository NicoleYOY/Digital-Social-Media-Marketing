# topic modeling  
install.packages("tidytext")   # provide functions and supporting data sets to allow conversion of text to and from tidy formats, and to switch seamlessly between tidy tools and existing text mining packages
install.packages("tidyr")      # text mining on tidy data
install.packages("janeaustenr")# dataset for the books of Janeausten
install.packages("dplyr")      # provide functions for data manipulation
install.packages("stringr")    #  manipulates individual characters within the strings.
install.packages("topicmodels") #packages for topic modeling
install.packages("reshape2")    #restructure and aggregate data 

#load packages

library(tidyr)               
library(tidytext)            

library(janeaustenr)        
library(dplyr)              
library(stringr)              
library(ggplot2)             
library("topicmodels")
library(reshape2)    

# here, we first create a collection of documents which are chapters from austen's six books. 
#In the following, we suppose that we do not know which book each document comes from
#So we use topic modeling to help us discover the topics of these documents and help us group and classify the document.

docs<- austen_books()%>% # the operator "%>%" allows the function in the next line to keep working on the object we got from last line.
      mutate(doc_index= cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE))))%>% #create index for the chapter/document
      select(-"book")  # In this case, we want to discover the topics of these chapters so we remove the "book" column from the dataset.

# tokenize the docs and count the frequency of words

docs<-docs%>%
   unnest_tokens(word, text) %>%    #tokenize the text
   count(doc_index, word, sort = TRUE) %>% #count the frequency of each word in each document
   anti_join(stop_words)#remove the stop_words


##### identifying the topics

dtm <- docs %>%
  cast_dtm(doc_index, word, n) #create the document-term matrix which we can use in LDA function

lda <- LDA(dtm, k = 6, control = list(seed = 1234)) #create a 6-topic LDA model because we know there are 6 books; 

topics <- tidy(lda, matrix = "beta") #create a table for probabilities of each word in each topic

topics

top_terms <- topics %>% 
  group_by(topic) %>%   # group by the column topic
  top_n(20, beta) %>%   # count the top 10 words in terms of beta, in each group 
  ungroup()        # remember ungroup

# plot the topics

ggplot(top_terms,aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#### the probabilities of each topic for a certain document

classification <- tidy(lda, matrix = "gamma") 

classification 

subset(classification,document=="14")#check which topic/book the document 14 belongs to

######

