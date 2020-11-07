setwd(".....wordcloud_with_r")
speech <- readLines("nehru_speech.txt")
#importing the libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(wordcloud2)
library(ggplot2)


#creating a corpus
corpus <- Corpus(VectorSource(speech))

#data cleaning
corpus <- corpus %>%
  tm_map(removePunctuation) %>% #removing punctuation
  tm_map(stripWhitespace) #striping whitespaces
corpus <- tm_map(corpus, content_transformer(tolower)) #transform the corpus into lowercase to make uniformity 
corpus <- tm_map(corpus,removeWords,stopwords(kind = "en")) #remove common english words

#creating a document termes marix
dtm <- TermDocumentMatrix(corpus)
mat <- as.matrix(dtm) #creating a matrix
words <- sort(rowSums(mat))
df <- data.frame(word = names(words), freq = words)
df <- df[order(df$freq, decreasing = TRUE),]
rownames(df) <- NULL

#creating wordcloud
set.seed(1234)

wordcloud2(data = df,color = "random-dark",backgroundColor = "black")
#rows subsettting for freq greater than 2
df_plot <- filter(df, df$freq > 2)

#plot
barplot(height=df_plot$freq, names=df_plot$word, 
        col="#69b3a2",
        horiz=T, las=1)
