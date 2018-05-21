rm(list=ls())

## install Rfacebook from github
# devtools::install_github("pablobarbera/Rfacebook/Rfacebook", force=TRUE)
library(Rfacebook)

library(dplyr)
library(ggplot2)
library(magrittr)
library(comfort)

## re-install ForesightsDesign from github
# devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesign)

######################################################################################
#
# Setup FB oauth token
#
# Source: http://thinktostart.com/analyzing-facebook-with-r/
#
######################################################################################

# ## FB App Display Name: design-thinking-research
app.id <- "<your app id>"
app.secret <- "<your app secret>"
fb_oauth <- Rfacebook::fbOAuth(app_id = app.id, app_secret = app.secret, extended_permissions = TRUE)

## save the token for reuse later
save(fb_oauth, file="fb_oauth")
# reuse token
load("fb_oauth")

# test token with your own facebook account
me <- getUsers("me", fb_oauth, private_info=TRUE)
# this should give your FB name
me$name

######################################################################################
#
# Load FB Page
#
######################################################################################

# define FB page to analyze
# this.page.name <- "design.thinking.research.lab"
this.page.name <- "donaldtrump"

# getPage() limit: max.~3-10.000 posts
this.page <- getPage(this.page.name, fb_oauth, n = 10000, since='2010/01/01', until='2018/12/31')

## save and load data for reuse
save(this.page, file="trumpAll")
load("trumpAll")

# latest 10 messages
this.page %>% select(message) %>% .[1:10,]

this.title <- "User Activities on Trump's Facebook Page 2010-2018"

## different plot themes
plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_minimal())

plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_minimal(),
                   color_scheme = scale_color_manual(values = c("brown", "lightgreen", "orange")),
                   lineplot = T)

plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_fivethirtyeight())

plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_hc())

plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_economist())

plot.fb <- plot_fb_activities(this.page,
                              title=this.title,
                              theme = theme_economist()) %T>% plot

plot.fb + scale_y_log10() # looks cool

######################################################################################
##
# line plot

plot_fb_activities(this.page,
                   title=this.title,
                   theme = theme_minimal(),
                   lineplot = TRUE,
                   # scaling_factor = scale_y_sqrt()
                   scaling_factor = scale_y_log10()
                   # scaling_factor = scale_y_reverse()
)


##
######################################################################################


######################################################################################
#
# Show the most recent FB post : Rfacebook::getPost()
#
######################################################################################

# IDs of all posts of this page
this.page %>% .$id %>% print
## ID of most recent post
most_recent <- this.page$id[1]
## ID of latest post
earliest <- this.page$id[length(this.page)]

# get most recent post + comments
getPost(most_recent, fb_oauth)

# get most recent post without comments
getPost(most_recent, fb_oauth, n = 20, likes = TRUE, comments = FALSE) %>%
    .$post %>%
    select(from_name, message, likes_count, shares_count, comments_count)


######################################################################################
#
# SCRIBBLE
#
######################################################################################



# convert created_time from character into datetime
this.page %>% tbl_df %>% select(created_time)
this.page$created_time %>%
    as.POSIXct(format="%Y-%m-%dT%H:%M:%S+0000", tz="GMT") # 2017-11-25T21:54:44+0000

# apply conversion
counts <- this.page %>% tbl_df %>%
    select(created_time, type, message,
           likes_count, comments_count, shares_count) %>%
    mutate(created_time =
               as.POSIXct(created_time,
                          format = "%Y-%m-%dT%H:%M:%S+0000",
                          tz="UTC"))

# time of earliest post as x-scale starting point (scale_x_datetime)
earliest_time <-
    counts %>% select(created_time) %>% na.omit %>% tail(1) %>% .$created_time

# basetime <- as.POSIXct(earliest_time, origin="1582-10-14", tz="GMT")
# basetime

### Nice: Trump FB Posts likes/shares/comments
par(mar=c(0,0,0,0))

tt.base <- counts %>% ggplot(aes(x=created_time)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    ggtitle("User Activities on Trump's Facebook Page 2009-2017") +
    theme(axis.text.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.line.x = element_blank()) +
    scale_x_datetime(minor_breaks = earliest_time + (0:9)*31536000 ) +
    # scale_y_continuous(breaks=c(200000, 400000, 800000)) +
    # scale_y_sqrt(breaks=c(250000, 500000, 1000000)) +
    # scale_y_log10() +
    scale_y_continuous(expand = c(0,0)) +
    ylab("Counts of likes/shares/comments")

tt.base

library(ggthemes)

# coloring automatically by theme_ + scale_color_
tt.base +
    theme_economist() + theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "top",
          legend.title = element_blank()) +
    scale_color_economist() +
    geom_point(aes(y=likes_count, color="likes")) +
    geom_point(aes(y=shares_count, color="shares"), alpha = 0.5) +
    geom_point(aes(y=comments_count, color="comments"), alpha = 0.5)

# # coloring hard-coded - not good (not changeable, no legend)
# tt.base +
#   theme_economist() + theme(plot.title = element_text(hjust = 0.5)) +
#   geom_point(aes(y=likes_count), color="darkblue") +
#   geom_point(aes(y=shares_count), color="lightblue") +
#   geom_point(aes(y=comments_count), color="lightgrey", alpha = 0.5)

# coloring automatically by scale_color_manual
tt <- tt.base +
    theme_economist() + theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.title = element_blank()) +
    scale_color_manual(values = c("deepskyblue", "darkblue", "white")) + # adds legend
    geom_point(aes(y=likes_count, color="likes")) + # color string = name in legendå
    geom_point(aes(y=shares_count, color="shares"), alpha = 0.9) +
    geom_point(aes(y=comments_count, color="comments"), alpha = 0.2)
tt


library(plotly)
# devtools::install_github('hadley/ggplot2')
ggplotly(tt)

page <- this.page
# show post with most likes
page[which.max(page$likes_count), ]
page %>% .$likes_count %>%
    which.max %>% page[.,] %>%
    select(created_time, from_name, likes_count, message)

# show post with most shares
page %>% .$shares_count %>%
    which.max %>% page[.,] %>%
    select(created_time, from_name, comments_count, message)

# show post with most comments
page %>% .$comments_count %>%
    which.max %>% page[.,] %>%
    select(created_time, from_name, comments_count, message)

######################################################################################
#
# Show likes/comments/shares over time
#
######################################################################################

## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
    date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
    m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month),
                   mean)
    m$month <- as.Date(paste0(m$month, "-15"))
    m$metric <- metric
    return(m)
}
# create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# visualize evolution in metric
library(ggplot2)
library(scales) # date_format()
ggplot(df, aes(x = month, y = x, group = metric)) +
    theme_bw() +
    ggtitle("Facebook Activities on Trump 2009-2017") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_color_brewer(palette = "Paired") +
    # scale_color_brewer(palette = "Set1") +
    geom_line(aes(color = metric)) +
    scale_x_date(date_breaks = "years", labels = date_format("%Y")) +
    # better for trend analysis: compare the gradients with log
    # scale_y_log10("Average count per post", breaks = c(10, 100, 1000, 10000, 50000)) # +
    ylab("count")


