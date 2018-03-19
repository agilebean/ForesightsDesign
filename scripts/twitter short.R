############################################################################
# 3.b) STREAM twitter tweets to JSON - streamR / twitteR package
############################################################################
library(streamR)
library(twitteR)
library(dplyr)
library(magrittr)
library(comfort)
devtools::install_github("agilebean/ForesightsDesign")
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
setwd(credentialsDir)
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

# get tweets from South Korea
json.name <- "tweetsSK.json"

# compare tweet activity
json.name <- "tweetsPresidents.json"

streamR::filterStream(json.name, track = c("Obama", "Trump"),
                      timeout = 120,
                      oauth = my_oauth)

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

words <- tweets$text

plot_wordcloud(tweets, 5, "Set1")
plot_wordcloud(tweets, 8, "Dark2")

plot_wordcloud(words, n_colors = 8)

plot_wordcloud(words, max_words = 70,
               remove_words = c("https", "the", "and"),
               n_colors = 8, palette = "Set1")

plot_wordcloud(words, max_words = 70,
               remove_words = c("https", "the", "and", "are", "this"),
               n_colors = 8, palette = "Set1")
