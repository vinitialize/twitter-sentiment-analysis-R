library(RColorBrewer)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(base64enc)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE"  
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)
consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE" 
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"
access_Token <- "3060838521-u5eXreDFHOqaxUcvTYMFyuEXImu5RlpdiY436h8" 
access_Secret <- "Q55FxITLmzlJWW4xpNbwnsW2UPXQZL4KiOWf9QdsDlYKt"
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)
namo <- searchTwitter('narendra modi', n=3000, lang="en")
length (namo)
no.of.tweets <- 1000
DS <- searchTwitter('#Datascience', n=no.of.tweets, lang="en")
ktk2 <- searchTwitter('#CoronaVirusUpdates', n=no.of.tweets, lang="en")
ktk3 <- searchTwitter('#IPL2020', n=no.of.tweets, lang="en")
ktk4 <- searchTwitter('#DelhiBurns', n=no.of.tweets, lang="en")
ktk5 <- searchTwitter('#DelhiGenocide', n=no.of.tweets, lang="en")
bjp <- searchTwitter('bjp', n=50, lang="en")
congress <- searchTwitter('congress', n=50, lang="en")
namo <- searchTwitter('narendra modi', n=50, lang="en")
raga <- searchTwitter('rahul gandhi', n=50, lang="en")
install.packages("SnowballC")
library(wordcloud)
library(SnowballC)
library(tm)
namo
ds_text <- sapply(DS, function(x) x$getText())
ds_text_corpus <- iconv(ds_text, 'UTF-8', 'ASCII')

ds_text_corpus <- Corpus(VectorSource(ds_text))
ds_text_corpus <- tm_map(ds_text_corpus, removePunctuation)
ds_text_corpus <- tm_map(ds_text_corpus, content_transformer(tolower))
ds_text_corpus <- tm_map(ds_text_corpus, function(x)removeWords(x,stopwords()))
ds_text_corpus <- tm_map(ds_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
ds_text_corpus <- tm_map(ds_text_corpus, content_transformer(removeURL))

insta_5 <- TermDocumentMatrix(ds_text_corpus)
insta_5 <- as.matrix(insta_5)
insta_5 <- sort(rowSums(insta_5),decreasing=TRUE)
insta_5
dfs <- data.frame(word = names(insta_5), freq = insta_5)
View(dfs)
wordcloud(words = dfs$word, freq = dfs$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8,"Dark2"))

namo_text_corpus=str_replace_all(namo_text_corpus,"[^[:graph:]]", " ") 
tm_map(namo_text_corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
namo_text <- sapply(bjp, function(x) x$getText())
namo_text_corpus <- Corpus(VectorSource(namo_text))
namo_text_corpus <- tm_map(namo_text_corpus, removePunctuation)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(tolower))
namo_text_corpus <- tm_map(namo_text_corpus, function(x)removeWords(x,stopwords()))
namo_text_corpus <- tm_map(namo_text_corpus, removeWords, c('RT', 'are','that'))

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
namo_text_corpus <- tm_map(namo_text_corpus, content_transformer(removeURL))
insta_2 <- TermDocumentMatrix(namo_text_corpus)
insta_2 <- as.matrix(insta_2)
insta_2 <- sort(rowSums(insta_2),decreasing=TRUE)

getwd()
setwd('C:/Users/vinit/Downloads/Data Science/Text mining/Sentiment Analysis')

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
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
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
namog <- ldply(namo,function(t) t$toDataFrame() )
result1 <- score.sentiment(namog$text,pos.words,neg.words)
summary(result1$score)
hist(result1$score,col = 'dark orange', main = 'Sentiment Analysis for Narendra Modi ', ylab = 'Count of tweets')
count(result1$score)
library(xlsx)
write.xlsx(result1, "myResults.xlsx")
