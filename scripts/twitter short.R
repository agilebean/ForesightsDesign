############################################################################
# 3.b) STREAM twitter tweets to JSON - streamR / twitteR package
############################################################################
library(streamR)
library(twitteR)
library(dplyr)
library(magrittr)
library(comfort)
# devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesign)

#####################################################################
#  1. Example for Trend Analysis
#####################################################################

demo_twitter(0)

tweets
map.data <- map_data("world2")

#####################################################################
#  2. Get Twitter Access
#####################################################################

# Authenticating browserless: Go to Twitter App Page
setup_twitter_oauth(consumer_key = "",
                    consumer_secret = "",
                    access_token = "",
                    access_secret = "")


# save authentication token for twitter
save(my_oauth, file="my_oauth")
load(file="my_oauth")

#####################################################################
#  3. Stream Twitter Tweets
#####################################################################
setwd(inputDV)

# # get Trump tweets: just retrieves ~428 tweets :(
# twitter.name <- "realDonaldTrump"
# tweets.raw <- twitteR::userTimeline(twitter.name, n = 3200)
# tweets.raw %>% head
# tweets.raw %>% length

# # get tweets across US
# json.name <- "tweetsUS.json"
# streamR::filterStream(json.name, locations = c(-125, 25, -66, 50), 
#                       timeout = 60,
#                       oauth = my_oauth)

# compare tweet activity
json.name <- "tweetsPresidents.json"
tweets.presidents <- streamR::parseTweets(json.name, simplify = TRUE)
streamR::filterStream(json.name, track = c("Obama", "Trump"), 
                      timeout = 120, 
                      my_oauth)

#####################################################################
#  4. Parse Twitter Tweets
#####################################################################

tweets <- streamR::parseTweets(json.name, verbose = FALSE)

#####################################################################
#  5. Plot Twitter Tweets
#####################################################################

map <- ggmap::get_map(location = "United States", zoom = 4)

plot_tweets_country(tweets, map)

#####################################################################
#  6. Plot word clouds
#####################################################################
plot_wordcloud(tweets, 5, "Set1")
plot_wordcloud(tweets, 9, "Dark2")

