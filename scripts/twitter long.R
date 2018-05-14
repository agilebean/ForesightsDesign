############################################################################
# 3.d) STREAM twitter tweets fresh - rtweet package
############################################################################

# Readings in descending order:
# https://colinfay.me/collect-rtweet/
# https://earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# https://blog.ouseful.info/2012/02/06/visualising-activity-round-a-twitter-hashtag-or-search-term-using-r/
# http://www.gettinggeneticsdone.com/2012/07/plotting-frequency-of-twitter-hashtag.html
# https://rstudio-pubs-static.s3.amazonaws.com/1211_0417278bb8174e26bd0dcd396d639b3b.html

vignette("intro", package = "rtweet")
library(rtweet)
library(dplyr)
library(ggplot2)
devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesign)

# Error in init_oauth1.0 >> https://github.com/mkearney/rtweet/issues/204#issuecomment-379776644
# Enter Callback URL in Twitter settings:
# Callback URL="http://127.0.0.1:1410" in https://apps.twitter.com/app/14453776/settings

# get twitter credentials
appname <- "design-thinking-research"
key     <- "FEnqC69UzJIuNpiMGzALa8c7d"
secret  <- "kl0FvOCK4UU0Rhl7vQHhoNmiepQVLYJx24BhyCSfqvpag4tUfo"

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

tweets_name <- "tweets.metoo.rds"
saveRDS(tweets, tweets_name)
tweets <- readRDS(tweets_name)
tweets %>% names
tweets %>% select(hashtags, text, ends_with("count")) 
# %>% tally() %>% 
#     arrange(desc)

# time interval: Desired interval of time expressed as numeral plus one of 
# "secs", "mins", "hours", "days", "weeks", "months", or "years"

# time_interval <- "mins"
# time_interval <- "hours"
# time_interval <- "2 hours"
time_interval <- "days"

tweets %>% select(created_at) %>% head

ts_plot(tweets, time_interval)

ts_plot(tweets, time_interval) +
  theme_minimal() +
  theme(plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #metoo Twitter statuses from past days",
    subtitle = paste0("Twitter status (tweet) counts aggregated using a time interval of ", time_interval),
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#### convenience function for search tweets by hashtag
#### provided by library(ForesightsDesign)
##
#
plot_tweets_by_keyword(search_term = "kim jong un", 
                       no_tweets = 10000, 
                       time_interval = "hours")


## tweets most retweeted 
## boring: doesn't show hashtag bec. it's a list
# tweets %>% select(hashtags, retweet_count) %>% arrange(desc(retweet_count))

# tweets by earliest tweet date
tweets %>% select(created_at) %>% arrange() %>% head(10)

# you can group + count = tally() 
# BUT then you cannot plot because tallying removes the individual entries!
tweets %>% 
    select(created_at, source, screen_name, text) %>%
    group_by(source) %>%  
    tally() %>%
    arrange(desc(n))

# you can get the highest counts
tweets %>% 
    select(created_at, source, screen_name, text) %>%
    group_by(source) %>%  
    add_tally() %>%
    arrange(desc(n))  %>% 
    head(11887) %>%
    ts_plot("hours")

### get top hashtags
tweets$hashtags %>% as.vector %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(desc(Freq)) %>%  
    top_n(10) %>% 
    ggplot(aes(x=reorder(., Freq), y=Freq)) +
    geom_bar(stat = "identity", aes(fill="red")) +
    coord_flip() +
    ggtitle("Top Hashtags")

plot_top_hashtags(rtweet_object = tweets, 
                  no_hashtags = 12, 
                  bar_color = "darkgreen", 
                  chart_title = "This is the title")

############################################################################
# B) STREAM TWEETS (present)
############################################################################

############################################################################
## 1. stream tweets by coordinates
############################################################################
lookup_coords("seoul, south korea")
lookup_coords("seoul")
coordinates <- lookup_coords("seoul")
coordinates <- lookup_coords("new york")

tweets_streamed <- stream_tweets(coordinates, timeout = 60)
tweets_streamed

tweets_streamed %>% ts_plot("mins")
tweets_streamed %>% print

############################################################################
## 2. stream tweets by keyword
############################################################################
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
## 3. stream tweets by timeline
############################################################################
tweets.trump <- get_timeline("realDonaldTrump", n = 3200)
filename <- "tweets.trump.rds"
# saveRDS(tweets.trump, filename)
readRDS(filename)
tweets.trump 

tweets.trump %>% ts_plot("12 hours")
tweets.trump %>% ts_plot("7 days")

# # doesn't work
# tweets.trump %>% ts_plot("2 days") + facet_wrap(~source)
# # doesn't work
# tweets.trump %>% ts_plot("24 hours", aes(color=.$source))

############################################################################
## 4. stream tweets by several timelines
############################################################################
timelines_news <- c("cnn", "BBCWorld", "foxnews")
tweets.news <- get_timelines(timelines_news, n = 3200)
filename <- "tweets.news.rds"
# saveRDS(tweets.news, filename)
readRDS(filename)

### examine all twitter activity using weekly intervals
tweets.news %>% ts_plot("weeks")

## plot the frequency of tweets for each user over time
## group by screen name and plot each time series
tweets.news %>%
  dplyr::filter(created_at > "2018-04-21") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  scale_color_brewer(palette = "Set1") +
  geom_point() +
  # theme_minimal() +
  ggthemes::theme_economist() +
  theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#### convenience function for 4. stream tweets by several timelines
#### provided by library(ForesightsDesign)
##
#
plot_twitter_timelines(timeline_names = c("cnn", "BBCWorld", "foxnews"),
                       from_date = "2018-04-21", 
                       time_interval = "days",
                       chart_title = "Frequency of Twitter Tweets by News Organization")
