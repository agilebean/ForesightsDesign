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

######################################################################################
plot.fb <- plot_fb_activities(this.page, 
                              title=this.title,
                              theme = theme_economist()) %T>% plot

# log10 allows better comparison
plot.fb + scale_y_log10() 

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
