library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results
library(wordcloud2) #for interactive word cloud & letter cloud
library(tm) #for text corpus
library(tmap) #for text document matrix



reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
api_key <- "QLe07IOYsnbePWJDJ4brzQjJp" 
api_secret <- "EuVyA5qg2fQyGS7yYrS5ToQ07PamM5yAv0x3brwSeL2HeGhazn" 
access_token <- "890799206775926784-Uk6vuPRJPfc13IawMSP5HSlBlVS1qFY" 
access_token_secret <- "iGRT6Op5a24JqqEY5A4zu36z9Lk0EN8AaNuFwtyAvNcXv" 

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

Deloitte_tweets = searchTwitter('@DeloitteIndia', n=5000)

#tweets to df
deloitte.df = twListToDF(Deloitte_tweets)

#tweets to text
deloitte.text = sapply(Deloitte_tweets, function(t)t$getText())

#pre-processing text function
clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

#passing text files for cleaning to above func
deloitte.clean = clean.text(deloitte.text)


#loading positive and negative wordfiles
posText <- read.delim(file.choose(), header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim(file.choose(), header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText,'upgrade','Congrats','prizes','prize','thanks','thnx',
              'Grt','gr8','plz','trending','recovering','brainstorm','leader')
neg.words = c(negText,'wtf','wait','waiting','epicfail','Fight','fighting',
              'arrest','no','not')

#sentiment scoring function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
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
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#calculating sentiment score & plotting histogram of sentiment score
deloitte.analysis <- score.sentiment(deloitte.clean, pos.words, neg.words)
deloitte.analysis %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "turquoise")+ 
  ylab("Frequency") + 
  xlab("Sentiment Score") + 
  ggtitle("Distribution of Sentiment scores of Deloitte tweets") 

#barplot of sentiment type
neutral <- length(which(deloitte.analysis$score == 0))
positive <- length(which(deloitte.analysis$score > 0))
negative <- length(which(deloitte.analysis$score < 0))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of Sentiment type of 4000 tweets")

#wordcloud
text_corpus <- Corpus(VectorSource(deloitte.clean))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("english")))
text_corpus <- tm_map(text_corpus, removeWords, c("global","globalwarming"))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)
View(tdm)
set.seed(123)
wordcloud2(tdm, size=0.3, shape= "square", 
           color='random-light', backgroundColor="black")

letterCloud(tdm, size=0.5, "D", color="random-light", backgroundColor="black")

letterCloud(tdm, size=0.2, "DELOITTE", color="random-light", backgroundColor="black")

  
