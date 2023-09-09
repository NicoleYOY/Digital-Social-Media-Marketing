
# install packages 

install.packages("tidytext") #provide functions and supporting data sets to allow conversion of text to and from tidy formats, and to switch seamlessly between tidy tools and existing text mining packages
install.packages("tidyr")    # text mining on tidy data
install.packages("janeaustenr")# dataset for the books of Janeausten
install.packages("dplyr")      #  provide functions for data manipulation
install.packages("stringr")    #  manipulates individual characters within the strings.
install.packages("textdata")   # to get the sentiment lexicon when using get_sentiment function.
install.packages("ggplot2")    # for plot
install.packages("wordcloud")  # for wordcloud

#load packages

library(tidyr)               
library(tidytext)            

library(janeaustenr)        
library(dplyr)              
library(stringr)                    
library(ggplot2)             
library(textdata) 
library(wordcloud)

# basics of tidy data format
  # first create a character vector called "text"

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

text_df <- tibble(line = 1:4, text = text) #create a dataframe where each line is for one element in the previous variable "text".

text_df


  text_df %>%   #The operator %>% allows the function in the next line to keep working on the object we got from last line.
  unnest_tokens(word, text) #tranform the data frame "text_df" into structured tidy data format

  text_df %>%   
  unnest_tokens(word, text)%>%
  mutate(rownumber=row_number()) %>% # add one more column "rownumber". Note this function can also change the value of existing columns.
  count(word, sort=TRUE)%>%  # we use count() to count the number of each factor in the designated column and put them into a new table
  filter(n>1) #only select those tokens which has a frequency larger than 1.

text_df_new= text_df %>%   
  unnest_tokens(word, text)%>%
  mutate(rownumber=row_number()) %>% # add one more column "rownumber". Note this function can also change the value of existing columns.
  count(word, sort=TRUE)

#library(wordcloud)

wordcloud(text_df_new$word, text_df_new$n, col=terrain.colors(length(text_df_new$word), alpha=0.9), random.order=FALSE)# add color


#########################Case study:text mining on Jane austen's books.

# load the data about the Jane austen's books into the variable "tidy_books"

tidy_books <- austen_books()

# you can check its structure

str(tidy_books)

# Task 1: Draw the wold cloud for the book "Emma"

emma=tidy_books%>%        
  filter(book == "Emma")%>%  #only select the book "Emma"
  unnest_tokens(word, text)%>% #Tokenization
  count(word, sort=TRUE)%>% #count the frequency for each word
  anti_join(stop_words) # remove the stopwords
   
#library(wordcloud)# pacakage for wordcloud

wordcloud(emma$word, emma$n, col=terrain.colors(length(text_df_new$word), alpha=0.9), random.order=FALSE)# add color

# Task 2: conduct the sentiment analysis for the book "Emma"


nrc <- get_sentiments("nrc")  #get sentiment words from NRC emotion lexicon; note: when you first use this function, you may be asked to agree with reference. For this, you just select "1".

nrc

emma_sentiment=tidy_books%>%        
  filter(book == "Emma")%>%
  unnest_tokens(word, text)%>%
  inner_join(nrc)%>% # merge the last object dataframe with the object dataframe "nrc"; note thse two objects have the same column "word"
  count(sentiment, sort=TRUE)%>% #count the frequency for each sentiment
  subset(sentiment!=c("positive","negative")) #remove the sentiment "positive" and "negative"
  
ggplot(emma_sentiment, aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE)  

# Task 3: examine how sentiment changes throughout each novel.

tidy_books<- austen_books()%>% # the operator "%>%" allows the function in the next line to keep working on the object we got from last line.
  group_by(book) %>%             # group the tidy_books by the "book" name so that the following "mutate" function will be applied to each book respectively.
  mutate(section =  row_number()%/% 80)%>% #add a column "linenumber"                                                            
  ungroup() %>% #ungroup so that the following operation would apply to the whole object instead of on each group respectively
  unnest_tokens(word, text)    #tokenize the text

bing=get_sentiments("bing") # get "bing" sentiment lexicon 

jane_austen_sentiment <- tidy_books %>% 
  inner_join(bing) %>% # inner_join function combines the table "jane_austen_sentiment" with the table "nrc_joy"; note they both have the colume "word".
  count(book, section, sentiment) %>% # transfrom the table into a new table by counting the positive and negative words in each section per book; note that we use integer division (%/%) to define larger sections of text that span multiple lines
  spread(sentiment, n, fill = 0) %>% # tranfrom the dataframe by putting the positive sentiment and negative sentiment in separate columns
  mutate(sentiment = positive - negative) # add a new column with net sentiment (positive-negative)

ggplot(jane_austen_sentiment, aes(section, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x") # draw the graphs to show how sentiment changes throught each novel
  

