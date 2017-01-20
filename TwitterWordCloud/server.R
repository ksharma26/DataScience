
library(shiny)
library(wordcloud)
library(twitteR) 
library(ROAuth)
library(ggplot2)
library(tm)
library(SnowballC)

api_key <- "54QkRv0GOnlsucwqjOCGxHBEd"

api_secret <- "HFlUXq6gTAaYN5eSiHe1EiRz7zEwe7pXNzcVIWXKVTRWBUVewF"

setup_twitter_oauth(api_key,api_secret)

wordCloudMatric <- function(twitterWord, freq, maxWords){
  
  rawTweets <- searchTwitter(twitterWord, n=20, lang="en")
  
  data.frame.tweets <-twListToDF(rawTweets)
  
  tweet.corpus <- Corpus(DataframeSource(data.frame(data.frame.tweets$text)))
  
  tweet.corpus <- tm_map(tweet.corpus,
                         content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                         mc.cores=1)
  
  tweet.corpus <- tm_map(tweet.corpus, content_transformer(tolower), lazy=TRUE)
  tweet.corpus <- tm_map(tweet.corpus, removePunctuation, lazy=TRUE)
  tweet.corpus <- tm_map(tweet.corpus, removeNumbers, lazy=TRUE)
  tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords('english'), lazy=TRUE)
  tweet.corpus <- tm_map(tweet.corpus, removeWords, c(twitterWord,"https"), lazy=TRUE)
  tweet.corpus <- tm_map(tweet.corpus, stemDocument, lazy=TRUE)
  
  myDTM = TermDocumentMatrix(tweet.corpus,control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  v = sort(rowSums(m), decreasing = TRUE)
  
  wordcloud(names(v), v, min.freq = freq, max.words = maxWords, scale=c(4,.5), colors=brewer.pal(8, "Dark2"), 
            random.order = FALSE)
  
}


shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    input$update
    inputWord <- isolate(input$inputWord)
    freq <- isolate(input$freq)
    maxWords <- isolate(input$maxWords)
    wordCloudMatric(inputWord, freq, maxWords)
    
  })
  
})
