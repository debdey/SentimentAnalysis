#import the data from CSV File
mydata=read.csv("CustomerData.csv",header=TRUE,stringsAsFactors = FALSE)
#create data frame
df<-data.frame(mydata)
#read top 5 lines
head(df,5)
rows.vec <- as.vector(t(df))
# package to manipulate text
library(dplyr)
library(tidytext)
library(tidyverse)
library(tidytext)
library(tibble)
library(forcats)
library(scales)
library(tm)
#Breakdown the sentences to word one-token-per-row format
sentences <- data_frame(sentence = 1:nrow(df),
                        text =rows.vec)
tidy.sentences <- sentences %>%
  unnest_tokens(word,text)
#Join with sentiment
tidy.sentences %>%
  inner_join(get_sentiments("bing"),by="word") 
#
tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word")
#Tally the total sentiment for each sentence
df_total<-tidy.sentences %>%
  count(sentence,word) %>%
  inner_join(get_sentiments("bing"),by="word") %>%
  group_by(sentence,sentiment) %>%
  summarize(total=sum(n))
# calculate a net-sentiment by subtracting positive and negative sentiment:
final_r<-df_total %>%
  spread(sentiment,total,fill = 0) %>%
  mutate(net.positve=positive-negative)
#merge back with sentence
Merge_Sentence<- sentences %>%  inner_join(final_r,by="sentence") 
#What about Bigrams.Following is the calculation For Bigram.

#Bi-Gram Calculation

austen_bigrams<-sentences %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams %>%
  count(sentence,bigram, sort = TRUE)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(sentence,word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
#senti
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(sentence,word1, word2, sort = TRUE)
#afinn
AFINN <- get_sentiments("afinn")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(sentence,word2, score, sort = TRUE) %>%
  ungroup()
#plot

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(200) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
#

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(sentence, score, sort = TRUE) %>%
  ungroup()

#put BI-GRAM VAluations.
Merge_Overall<-Merge_Sentence %>% left_join(negated_words,by="sentence") 
#Replace All NA with Zeroes
Merge_Final<-Merge_Overall %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
#Get the Final Score
Final_Score<-Merge_Final %>% mutate(final=net.positve-score) %>% count(sentence,text,final)
#Merge Back to Intial List
Final_Table<-sentences %>% left_join(Final_Score,by="sentence") %>% count(sentence,text.x,final)
#Replace Zero 
Final_Table_Complete<- Final_Table %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0.5, .)))


Final_Table_Complete1<-Final_Table_Complete %>% 
  mutate(Classify = case_when(final <= 0.5 ~ "Negative",
                              final > 0  ~ "Positive"))

#Export the data in CSV
write.csv(Final_Table_Complete1, "filename.csv") 
