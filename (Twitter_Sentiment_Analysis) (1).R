#Clear R Environment
rm(list=ls())

# Load the required R libraries

install.packages("RColorBrewer")
install.packages("tm")
install.packages("wordcloud")
install.packages('base64enc')
install.packages('ROAuth')
install.packages('plyr')
install.packages('stringr')
install.packages('twitteR')
install.packages("SnowballC")

library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
library(wordcloud)
library(SnowballC)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#Set constant request URL
requestURL <- "https://api.twitter.com/oauth/request_token"

# Set constant access URL
accessURL <- "https://api.twitter.com/oauth/access_token"

# Set constant auth URL
authURL <- "https://api.twitter.com/oauth/authorize"


# Put the both Consumer Key and Consumer Secret key from Twitter App.
consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE"  
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"

#Create the authorization object by calling function OAuthFactory
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)

consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE" 
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"
access_Token <- "3060838521-u5eXreDFHOqaxUcvTYMFyuEXImu5RlpdiY436h8" 
access_Secret <- "Q55FxITLmzlJWW4xpNbwnsW2UPXQZL4KiOWf9QdsDlYKt"

# Create Twitter connection
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)



amazon <- searchTwitter('AmazonIndia', n=1000, lang="en")
flipkart <- searchTwitter('flipkart', n=3000, lang="en")
snapdeal <- searchTwitter('snapdeal', n=3000, lang="en")




#amazon
amazon
amazon_text <- sapply(amazon, function(x) x$getText())
amazon_text_corpus <- iconv(amazon_text, 'UTF-8', 'ASCII')
amazon_text_corpus <- Corpus(VectorSource(amazon_text))
amazon_text_corpus <- tm_map(amazon_text_corpus, removePunctuation)
amazon_text_corpus <- tm_map(amazon_text_corpus, content_transformer(tolower))
amazon_text_corpus <- tm_map(amazon_text_corpus, function(x)removeWords(x,stopwords()))
amazon_text_corpus <- tm_map(amazon_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
amazon_text_corpus <- tm_map(amazon_text_corpus, content_transformer(removeURL))

insta_2 <- TermDocumentMatrix(amazon_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)
insta_2
d_2 <- data.frame(word = names(insta_2), freq = insta_2)
View(d_2)
wordcloud(words = d_2$word, freq = d_2$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))



#flipkart
flipkart
flipkart_text <- sapply(flipkart, function(x) x$getText())
flipkart_text_corpus <- iconv(flipkart_text, 'UTF-8', 'ASCII')
flipkart_text_corpus <- Corpus(VectorSource(flipkart_text))
flipkart_text_corpus <- tm_map(flipkart_text_corpus, removePunctuation)
flipkart_text_corpus <- tm_map(flipkart_text_corpus, content_transformer(tolower))
flipkart_text_corpus <- tm_map(flipkart_text_corpus, function(x)removeWords(x,stopwords()))
flipkart_text_corpus <- tm_map(flipkart_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
flipkart_text_corpus <- tm_map(flipkart_text_corpus, content_transformer(removeURL))

insta_1 <- TermDocumentMatrix(flipkart_text_corpus)
insta_1 <- as.matrix(insta_1)
insta_1 <- sort(rowSums(insta_1),decreasing=TRUE)
insta_1
d_1 <- data.frame(word = names(insta_1), freq = insta_1)
View(d_1)
wordcloud(words = d_1$word, freq = d_1$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))



#snapdeal
snapdeal
snapdeal_text <- sapply(snapdeal, function(x) x$getText())
snapdeal_text_corpus <- iconv(snapdeal_text, 'UTF-8', 'ASCII')
snapdeal_text_corpus <- Corpus(VectorSource(snapdeal_text))
snapdeal_text_corpus <- tm_map(snapdeal_text_corpus, removePunctuation)
snapdeal_text_corpus <- tm_map(snapdeal_text_corpus, content_transformer(tolower))
snapdeal_text_corpus <- tm_map(snapdeal_text_corpus, function(x)removeWords(x,stopwords()))
snapdeal_text_corpus <- tm_map(snapdeal_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
snapdeal_text_corpus <- tm_map(snapdeal_text_corpus, content_transformer(removeURL))

insta_3 <- TermDocumentMatrix(snapdeal_text_corpus)
insta_3 <- as.matrix(insta_3)
insta_3 <- sort(rowSums(insta_3),decreasing=TRUE)
insta_3
d_3 <- data.frame(word = names(insta_3), freq = insta_3)
View(d_3)
wordcloud(words = d_3$word, freq = d_3$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))



##############Sentiment Analysis###########

getwd()
setwd('C:/Users/Dixita/Documents')

pos.words <- read.csv('positive.csv')
neg.words <- read.csv('negative.csv')

pos.words <- scan('positive.csv',what = 'character')
neg.words <- scan('negative.csv',what = 'character')

pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'wtf', 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 
              'freaking','sucks','horrible')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



amazon_1 <- ldply(amazon,function(t) t$toDataFrame())
flipkart_1 <- ldply(flipkart,function(t) t$toDataFrame())
snapdeal_1 <- ldply(snapdeal,function(t) t$toDataFrame())


result1 <- score.sentiment(amazon_1$text,pos.words,neg.words)
result2 <- score.sentiment(flipkart_1$text,pos.words,neg.words)
result3 <- score.sentiment(snapdeal_1$text,pos.words,neg.words)

summary(result1$score)
summary(result2$score)
summary(result3$score)


hist(result1$score,col = 'blue', main = 'Sentiment Analysis for Amazon ', ylab = 'Count of tweets')
count(result1$score)
hist(result2$score,col = 'dark orange', main = 'Sentiment Analysis for Flipkart ', ylab = 'Count of tweets')
count(result2$score)
hist(result3$score,col = 'green', main = 'Sentiment Analysis for Snapdeal ', ylab = 'Count of tweets')
count(result3$score)


library(xlsx)
write.xlsx(result1, "flipkart.xlsx")
write.xlsx(result1, "amazon.xlsx")
write.xlsx(result1, "snapdeal.xlsx")










