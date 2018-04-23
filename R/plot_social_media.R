######################################################################################
#
# Plot Google Trends
# display color palettes > RColorBrewer::display.brewer.all()
#
######################################################################################
plot_gtrends <- function(search_terms, gtrends_object = NULL,
                         palette = "Set1", title = NULL)
{
    require(ggplot2)
    require(dplyr)
    require(tidyr)

    # create gtrends object from search_terms
    if (is.null(gtrends_object))
    {
        require(gtrendsR)
        gtrends_object <- gtrends(search_terms)
        gtrends_object$interest_over_time$hits <-
            as.numeric(gtrends_object$interest_over_time$hits)
    }

    # set default title
    if (is.null(title)) title <- "Interest over time"

    # create template for: geom_line(aes(y=`var`, color ="<var>"))
    string.template <- "geom_line(aes(y=`<VAR>`, color ='<VAR>'))"
    # create geom_line for each search term
    geom.line.code <- search_terms %>%
        lapply(function(x) {
            gsub("<VAR>", x , string.template)
        }) %>%
        as.character()

    # create trend chart
    gg <- gtrends_object$interest_over_time %>%
        spread(keyword, hits) %>%
        # aes(color) specifies the colored geom's label, not the color!!
        ggplot(aes(x=date, color = `search term`)) +
        scale_color_brewer(palette = palette) +
        ggtitle(title) +
        {
            lapply(geom.line.code,
                   function(search_term)
                       eval(parse(text=search_term)))
        }
    # add gtrends object to output to enable geom_smooth
    gg$gtrends <- gtrends_object
    return(gg)
}

######################################################################################
#
# Demo twitter
# Plots twitter activities
#
######################################################################################
demo_twitter <- function(mode = NULL)
{
    require(dplyr)
    require(ggplot2)

    # get tweets object
    data("tweetsSK") # 2033 - 1738 NA = 295

    # get map.data: replaced ggplot2::map_data("world2") with saved data
    data("world2")

    if (is.null(mode))
    {
        mode = 5
    }

    # world map full
    gg <- ggplot(map.data) +
        geom_map(aes(map_id = region), map = map.data, fill = "white",
                 color = "grey20", size = 0.25) +
        expand_limits(x = map.data$long, y = map.data$lat) +
        ggtitle("Twitter on Trump")

    if (mode >= "1") # korean map
    {
        gg <- gg +
            coord_cartesian(xlim = c(123,131), ylim = c(34,38))
    }
    if (mode >= "2") # korean map with trump tweets
    {
        gg <- gg +
            geom_point(data = tweets,
                       aes(x = place_lon, y = place_lat),
                       size = 8,
                       alpha = 0.03,
                       color = "blue")
    }
    if (mode >= "3") # korean map with trump tweets blank
    {
        gg <- gg +
            theme(
                axis.line = element_blank(),
                # axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.background = element_blank(), panel.border = element_blank(),
                panel.grid.major = element_blank(), plot.background = element_blank(),
                plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")
            )
    }

    return(gg)
}

######################################################################################
#
# plot_tweets_country()
# tweet_object generated from streamR::filterStream(json.name) %>%
#                             streamR::parseTweets()
# Plots twitter activities
#
######################################################################################
plot_tweets_country <- function(tweet_object, map_object,
                              size=7, alpha=0.1, color="blue")
{
    require(ggmap) # contains get_map from Google Maps!
    require(comfort)
    setwd(inputDV)

    gg <- ggmap(map_object) +
        theme(
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank(), panel.border = element_blank(),
            panel.grid.major = element_blank(), plot.background = element_blank(),
            plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")
        ) +
        geom_point(data = tweet_object,
                   aes(x = place_lon, y = place_lat),
                   size = size,
                   alpha = alpha,
                   color = color)
    gg
}


######################################################################################
#
# Plot worldcloud
# display color palettes > RColorBrewer::display.brewer.all()
#
######################################################################################
plot_wordcloud <- function(words, max_words = 70, remove_words ="",
                           n_colors = 5, palette = "Set1")
{
    require(dplyr)
    require(wordcloud)
    require(RColorBrewer)
    require(tm)

    # remove all non-printable characters in UTF-8
    # Reason: http://www.textasdata.com/2015/02/encoding-headaches-emoticons-and-rs-handling-of-utf-816/
    words <- iconv(words, "ASCII", "UTF-8", sub="")

    words.corpus <- Corpus(VectorSource(words))
    words.corpus <- tm_map(words.corpus, removeWords, remove_words)
    words.corpus <- tm_map(words.corpus, tolower)

    wc <- wordcloud(words=words.corpus, max.words=max_words,
                    random.order=FALSE,
                    colors = brewer.pal(n_colors, palette),
                    random.color = FALSE,
                    scale=c(5.5,.5), rot.per=0.35) %>% recordPlot
    return(wc)
}


######################################################################################
#
# plot_tweets_by_keyword()
# tweet_object generated from rtweet::search_tweets() %>%
#                             rtweet::ts_plot()
# Plots twitter activities
#
######################################################################################
plot_tweets_by_keyword <- function(search_term, no_tweets, time_interval)
{
    require(rtweet)
    require(ggplot2)

    tweets <- search_tweets(search_term, n=no_tweets, include_rts = FALSE)

    ts_plot(tweets, time_interval) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold")) +
        labs(
            x = NULL, y = NULL,
            title = paste0("Frequency of Twitter Tweets from past days for ", search_term),
            subtitle = paste0("Twitter status (tweet) counts aggregated using a time interval of ", time_interval),
            caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
}

######################################################################################
#
# plot_top_hashtags()
# tweet_object generated from rtweet::search_tweets()
# Plots twitter hashtags
#
######################################################################################
plot_top_hashtags <- function(rtweet_object, no_hashtags, bar_color, chart_title)
{
    require(rtweet)
    require(dplyr)
    require(ggplot2)

    rtweet_object$hashtags %>%
        as.vector %>% unlist() %>%
        table() %>%
        as.data.frame() %>%
        arrange(desc(Freq)) %>%
        top_n(no_hashtags) %>%
        ggplot(aes(x=reorder(., Freq), y=Freq)) +
        geom_bar(stat = "identity", fill=bar_color) +
        coord_flip() +
        ggtitle(chart_title)
}


######################################################################################
#
# plot_twitter_timelines()
# tweet_object generated from rtweet::search_tweets() %>%
#                             rtweet::ts_plot()
# Plots twitter activities
#
# display color palettes > RColorBrewer::display.brewer.all()
#
# The following palettes are recommended:
# Accent, Dark2, Paired, Set1, Set2, Set3
#
#
######################################################################################
plot_twitter_timelines <- function(timeline_names, from_date, time_interval,
                                   chart_title)
{
    require(rtweet)
    require(dplyr)
    require(ggplot2)
    require(ggthemes)

    print(paste("Searching twitter activity for", paste(timeline_names, collapse = ", ")))
    print("This will take a while... Go get a coffee.")

    timelines <- get_timelines(timeline_names, n = 3200)

    timelines %>%
        filter(created_at > from_date) %>%
        group_by(screen_name) %>%
        ts_plot(time_interval, trim = 1L) +
        scale_color_brewer(palette = "Set1") +
        geom_point() +
        ggthemes::theme_economist() +
        theme(
            legend.title = ggplot2::element_blank(),
            legend.position = "bottom",
            plot.title = ggplot2::element_text(face = "bold")) +
        labs(
            x = NULL, y = NULL,
            title = chart_title,
            subtitle = paste0("Twitter status (tweet) counts aggregated by day from ", from_date),
            caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )

}

######################################################################################
#
# Show FB activities : likes, comments, shares
#
######################################################################################
plot_fb_activities <- function(page, title, theme, color_scheme = NULL, lineplot = FALSE)
{
    require(dplyr)
    require(ggplot2)
    require(ggthemes)

    # calculate counts of FB likes, comments, shares
    counts <- page %>% tbl_df %>%
        select(created_time, type, message,
               likes_count, comments_count, shares_count) %>%
        mutate(created_time =
                   as.POSIXct(created_time,
                              format = "%Y-%m-%dT%H:%M:%S+0000",
                              tz="UTC"))

    # time of earliest post as x-scale starting point (scale_x_datetime)
    earliest_time <-
        counts %>% select(created_time) %>% na.omit %>% tail(1) %>% .$created_time

    # default coloring by scale_color_manual
    if (is.null(color_scheme)) {
        color_scheme <- scale_color_manual(values = c("deepskyblue", "darkblue", "white")) # adds legend
    }

    # create base plot
    tt.base <- counts %>% ggplot(aes(x=created_time)) +
        theme +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.position = "top",
              legend.title = element_blank()) +
        ggtitle(title) +
        theme(axis.text.x = element_blank()) +
        theme(axis.ticks.x = element_blank()) +
        theme(axis.line.x = element_blank()) +
        scale_x_datetime(minor_breaks = earliest_time + (0:9)*31536000 ) +
        # scale_y_continuous(breaks=c(200000, 400000, 800000)) +
        # scale_y_sqrt(breaks=c(250000, 500000, 1000000)) +
        # scale_y_log10() +
        scale_y_continuous(expand = c(0,0)) +
        ylab("Counts of likes/shares/comments") +
        color_scheme

    if(!lineplot)
    {
        # scatterplots for FB likes, comments, shares
        tt <- tt.base +
            geom_point(aes(y=likes_count, color="likes")) + # color string = name in legendå
            geom_point(aes(y=shares_count, color="shares"), alpha = 0.9) +
            geom_point(aes(y=comments_count, color="comments"), alpha = 0.2)

        tt
    } else if (lineplot) {
        tt.base +
            geom_line(aes(y=likes_count, color="likes")) + # color string = name in legend
            geom_line(aes(y=shares_count, color="shares"), alpha = 0.9) +
            geom_line(aes(y=comments_count, color="comments"), alpha = 0.2) +
            # scale_y_sqrt(breaks=c(250000, 500000, 1000000)) +
            scale_y_log10() +
            ylab("Counts of likes/shares/comments") +
            theme_economist()
    }

}

