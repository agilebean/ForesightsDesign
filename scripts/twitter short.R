############################################################################
# Twitter Analysis
############################################################################

vignette("intro", package = "rtweet")
library(rtweet)
library(dplyr)
library(ggplot2)
devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesign)

# Enter Callback URL in Twitter settings:
# Callback URL="http://127.0.0.1:1410" in https://apps.twitter.com/app/14453776/settings

# get twitter credentials
appname <- "<your appname>"
key     <- "<your consumer key>"
secret  <- "<your consumer secret>"

# generate twitter token
twitter_token <- create_token(
                    app = appname,
                    consumer_key = key,
                    consumer_secret = secret)

# todo - save token: https://rud.is/b/2017/10/22/a-call-to-tweets-blog-posts/

############################################################################
# A) SEARCH TWEETS (past)
############################################################################

# search tweets by hashtag
tweets <- search_tweets("#maga",
                        n=18000,
                        # retryonratelimit=TRUE,
                        type="recent",
                        include_rts = FALSE)


# search tweets by timeline
tweets <- get_timeline("realDonaldTrump", n = 3200)

tweets %>% ts_plot("12 hours")
tweets %>% ts_plot("7 days")


############################################################################
# B) STREAM TWEETS (present)
############################################################################


# 1. stream tweets by coordinates
coordinates <- lookup_coords("new york")

tweets_streamed <- stream_tweets(coordinates, timeout = 60)
tweets_streamed

tweets_streamed %>% ts_plot("mins")
tweets_streamed %>% print


# 2. stream tweets by keyword
keyword <- "kim jong un"
file.name <- "kimjongun.json"

stream_tweets(keyword,
              timeout = 60,
              file_name = file.name,
              parse = FALSE
              )

# read in the data as a tidy tbl data frame
tweets_parsed <- parse_stream(file.name)
tweets_parsed
tweets_parsed %>% ts_plot("mins")



############################################################################
# C) PLOT TWEETS
#  provided by library(ForesightsDesign)
############################################################################

## Plot tweets by keyword
plot_tweets_by_keyword(search_term = "kim jong un",
                       no_tweets = 10000,
                       time_interval = "hours")


## Plot top hashtags
# requires tweets object from search or streaming function
plot_top_hashtags(rtweet_object = tweets,
                  no_hashtags = 12,
                  bar_color = "darkgreen",
                  chart_title = "This is the title")

##
# Plot tweets by several timelines
plot_twitter_timelines(timeline_names = c("cnn", "BBCWorld", "foxnews"),
                       from_date = "2018-04-21",
                       time_interval = "days",
                       chart_title = "Frequency of Twitter Tweets by News Organization")

############################################################################
# D) PLOT WORDCLOUD
#  provided by library(ForesightsDesign)
############################################################################

words <- tweets$text

plot_wordcloud(words, n_colors = 8, "Set1")

plot_wordcloud(words, max_words = 70,
               remove_words = c("https", "the", "and", "are", "you",
                                "this", "that","for", "have", "but"),
               n_colors = 8, palette = "Dark2")

