
setwd("C:/One-Drive/OneDrive - Tredence/Training/Text Analytics/twitter")

library(twitteR)
library(rtweet)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
library(wordcloud)
library(RWeka)
library(syuzhet) # Sentiment
library(topicmodels)
library(tidytext)
library(slam)

#======== User defined functions ====================

tweets_downloader <- function(tag, n, lang='en', retryonratelimit = TRUE){
  
  twitter_token <- create_token(
    app = 'Murali - Extracting Tweets',
    consumer_key <- "glD6ScAGhciTuy9u5Szz9w2Z6",
    consumer_secret <- "omaeCCtA59KcEexnzvl9nHw3tEKvpYqKTLfsWPSZSL35DYFiLE",
    access_token <- "372777040-M2RWaNZtny7CrD54IxTdsd6mYMAhiplpBVdy2rXE",
    access_secret <- "uramRxk6C6rt1xlgywe6ccIjWF3mpdYCLeya4N8zmVLC2",
    set_renv = F
  )
  
  tweet.df <- search_tweets(tag, n = n, include_rts = FALSE, lang = lang, token = twitter_token, retryonratelimit = retryonratelimit)
  print(paste0("Total Tweets downloaded for - ",tag,": ",length(tweet.df$text)))
  print(paste0("Total Unique Texts downloaded for - ",tag,": ",length(unique(tweet.df$text))))
  
  tweet.df$hashtags <- as.character(tweet.df$hashtags)
  tweet.df$symbols <- as.character(tweet.df$symbols)
  tweet.df$urls_url <- as.character(tweet.df$urls_url)
  tweet.df$urls_t.co <- as.character(tweet.df$urls_t.co)
  tweet.df$urls_expanded_url <- as.character(tweet.df$urls_expanded_url)
  tweet.df$media_url <- as.character(tweet.df$media_url)
  tweet.df$media_t.co <- as.character(tweet.df$media_t.co)
  tweet.df$media_expanded_url <- as.character(tweet.df$media_expanded_url)
  tweet.df$media_type <- as.character(tweet.df$media_type)
  tweet.df$ext_media_url <- as.character(tweet.df$ext_media_url)
  tweet.df$ext_media_t.co <- as.character(tweet.df$ext_media_t.co)
  tweet.df$ext_media_expanded_url <- as.character(tweet.df$ext_media_expanded_url)
  tweet.df$mentions_user_id <- as.character(tweet.df$mentions_user_id)
  tweet.df$mentions_screen_name <- as.character(tweet.df$mentions_screen_name)
  tweet.df$geo_coords <- as.character(tweet.df$geo_coords)
  tweet.df$coords_coords <- as.character(tweet.df$coords_coords)
  tweet.df$bbox_coords <- as.character(tweet.df$bbox_coords)
  
  tweet.df
}

tweets_cleaner <- function(tweet.df){
  
  tweets_txt <- unique(tweet.df$text)
  clean_tweet = gsub("&amp", "", tweets_txt) # Remove Amp
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) # Remove Retweet
  clean_tweet = gsub("@\\w+", "", clean_tweet) # Remove @
  clean_tweet = gsub("#", " ", clean_tweet) # Before removing punctuations, add a space before every hashtag
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet) # Remove Punct
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet) # Remove Digit/Numbers
  clean_tweet = gsub("http\\w+", "", clean_tweet) # Remove Links
  clean_tweet = gsub("[ \t]{2,}", " ", clean_tweet) # Remove tabs
  clean_tweet = gsub("^\\s+|\\s+$", " ", clean_tweet) # Remove extra white spaces
  clean_tweet = gsub("^ ", "", clean_tweet)  # remove blank spaces at the beginning
  clean_tweet = gsub(" $", "", clean_tweet) # remove blank spaces at the end
  clean_tweet = gsub("[^[:alnum:][:blank:]?&/\\-]", "", clean_tweet) # Remove Unicode Char
  
  
  clean_tweet <- str_replace_all(clean_tweet," "," ") #get rid of unnecessary spaces
  clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","") # Get rid of URLs
  clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
  clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","") # Take out retweet header, there is only one
  clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","") # Get rid of hashtags
  clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","") # Get rid of references to other screennames
  
  clean_tweet
}

tweets_cleaner_tm <- function(clean_tweet, custom_stopwords = c("bla bla")){
  
  docs <- Corpus(VectorSource(clean_tweet))
  #inspect(docs)
  
  
  docs <- tm_map(docs, content_transformer(tolower)) # Convert the text to lower case
  docs <- tm_map(docs, removeNumbers) # Remove numbers
  docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
  docs <- tm_map(docs, removeWords, custom_stopwords)  # Remove your own stop word
  docs <- tm_map(docs, removePunctuation) # Remove punctuations
  docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
  # docs <- tm_map(docs, stemDocument) # Text stemming
  docs
}



#============ Get The data for all hashtags ==================


#=== Data Science 
Sys.sleep(15*60)
tweet_df_ds <- tweets_downloader(tag="#DataScience OR #MachineLearning OR #DeepLearning", n=50000, lang='en', 
                                 retryonratelimit = TRUE)
saveRDS(tweet_df_ds, file = "tweet_df_ds.rds")

#=== IPhone XS
Sys.sleep(15*60)
tweet_df_iphone <- tweets_downloader(tag="#iPhoneXS OR #iPhoneXSMax", n=50000, lang='en', 
                                 retryonratelimit = TRUE)
saveRDS(tweet_df_iphone, file = "tweet_df_iphone.rds")

#=== Captain Marvel
Sys.sleep(15*60)
tweet_df_capmarvel <- tweets_downloader(tag="#CaptainMarvelTH OR #CaptainMarvelTrailer OR #captainmarvel", n=50000, lang='en', 
                                     retryonratelimit = TRUE)
saveRDS(tweet_df_capmarvel, file = "tweet_df_capmarvel.rds")

#=== Section 377
Sys.sleep(15*60)
tweet_df_sec377 <- tweets_downloader(tag="#Section377Verdict OR #Section377 OR #377Verdict OR #section377scrapped", n=50000, lang='en', 
                                        retryonratelimit = TRUE)
saveRDS(tweet_df_sec377, file = "tweet_df_sec377.rds")

#=== Robo 2.0
tweet_df_robo2 <- tweets_downloader(tag="#2Point0Teaser OR #2Point0 OR #2point0trailer", n=50000, lang='en', 
                                     retryonratelimit = TRUE)
saveRDS(tweet_df_robo2, file = "tweet_df_robo2.rds")

#=== Kerala Floods
tweet_df_keralafloods <- tweets_downloader(tag="#KeralaFloods OR #KeralaFloodRelief OR #keralaflood", n=50000, lang='en', 
                                    retryonratelimit = TRUE)
saveRDS(tweet_df_keralafloods, file = "tweet_df_keralafloods.rds")

#=== Indian Railways
tweet_df_indrail <- tweets_downloader(tag="#IndianRailways OR #indianrailway OR #irctc OR #IRCTC", n=50000, lang='en', 
                                           retryonratelimit = TRUE)
saveRDS(tweet_df_indrail, file = "tweet_df_indrail.rds")

#==== Android Pie
tweet_df_androidpie <- tweets_downloader(tag="#AndroidPie OR #Android9 OR #android9pie", n=50000, lang='en', 
                                      retryonratelimit = TRUE)
saveRDS(tweet_df_androidpie, file = "tweet_df_androidpie.rds")

#==== Emmy
tweet_df_emmy <- tweets_downloader(tag="#emmy2018 OR #EmmyAwards2018 OR #EmmyAwards OR #Emmy", n=50000, lang='en', 
                                         retryonratelimit = TRUE)
saveRDS(tweet_df_emmy, file = "tweet_df_emmy.rds")

#=== Reliance Dassault
tweet_df_reliance <- tweets_downloader(tag="#Reliance OR #reliance OR #Dassault OR #Rafale", n=50000, lang='en', 
                                   retryonratelimit = TRUE)
saveRDS(tweet_df_reliance, file = "tweet_df_reliance.rds")



#================= Sentiment Analysis =========================
setwd("C:/One-Drive/OneDrive - Tredence/Training/Text Analytics/twitter/Twitter Talks Shiny Sparks/data")
tweet_df_ds <- readRDS(file = "tweet_df_ds.rds")
tweet_df_iphone <- readRDS(file = "tweet_df_iphone.rds")
tweet_df_capmarvel <- readRDS(file = "tweet_df_capmarvel.rds")
tweet_df_sec377 <- readRDS(file = "tweet_df_sec377.rds")
tweet_df_robo2 <- readRDS(file = "tweet_df_robo2.rds")
tweet_df_reliance <- readRDS(file = "tweet_df_reliance.rds")


word.df <- as.vector(tweets_cleaner(tweet_df_ds))
emotion_df_ds <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_ds, file = "emotion_df_ds.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_iphone))
emotion_df_iphone <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_iphone, file = "emotion_df_iphone.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_capmarvel))
emotion_df_capmarvel <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_capmarvel, file = "emotion_df_capmarvel.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_sec377))
emotion_df_sec377 <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_sec377, file = "emotion_df_sec377.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_robo2))
emotion_df_robo2 <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_robo2, file = "emotion_df_robo2.rds")

word.df <- as.vector(tweets_cleaner(tweet_df_reliance))
emotion_df_reliance <- get_nrc_sentiment(word.df)
saveRDS(emotion_df_reliance, file = "emotion_df_reliance.rds")





