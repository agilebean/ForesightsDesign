#####################################################################
#
# Trend Analysis with Google Trends - Long Version
# Class: Foresights & Design
# Prof:  Dr. Chaehan So
#
#####################################################################

# try to compare:
# https://trends.google.com/trends/explore?date=today%205-y&q="ggplot","Qlikview","FusionCharts","Highcharts"
# devtools::install_github("PMassicotte/gtrendsR")
# .rs.restartR()

library(gtrendsR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(magrittr)
# install library from Github, then comment out this line (#)
devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesignTools)

#####################################################################
#  Example for Trend Analysis
#####################################################################


#####################################################################
# 1. Get data from Google Trends API
#####################################################################

# retrieve data from google trends (requires internet connection)
gtrends.object <- gtrends(search.terms)

## trend in design terminology
search.terms <- c("service design",
                  "design thinking",
                  "design research")

design.terms <-  gtrends(search.terms)

plot(design.terms)

# convert into numeric if plot doesn't show line chart
# long version
design.terms$interest_over_time$hits <- design.terms %>%
    .[[1]] %>% .$hits %>%
    lapply(function(x) if (x=="<1") x <- 0 else x) %>%
    as.numeric()
# short version
design.terms$interest_over_time$hits %<>% as.numeric

# plot (in default mode) as time series
plot(design.terms)
plot(design.terms) + theme_economist()

# plot (alternative with dplyr > more structured syntax)
design.terms %>% plot
design.terms %>% plot + theme_economist()
design.terms %>% plot + theme_economist() + geom_smooth()

design.terms %>% str
design.terms$interest_by_country %>%
    select(hits, location, keyword) %>%
    mutate(hits=as.numeric(hits)) %>%
    arrange(desc(hits)) %>%
    head(20)

design.terms$interest_by_dma  %>%
    select(hits, location, keyword) %>%
    mutate(hits=as.numeric(hits)) %>%
    arrange(desc(hits)) %>%
    head(20)

design.terms$interest_by_city %>%
    select(hits, location, keyword) %>%
    mutate(hits=as.numeric(hits)) %>%
    arrange(desc(hits)) %>%
    head(20)

design.terms$related_queries %>%
    select(subject, value, keyword) %>%
    mutate(subject=as.numeric(subject)) %>%
    arrange(desc(subject)) %>%
    head(20)


## Save as image file


#####################################################################
#  Customize plot with
#  - color palette
#  - search terms
#  - generate google trends object
#####################################################################

search.terms <- c("service design","design thinking","design research")
search.terms <- c("design research", "service design")

plot_gtrends(search.terms,
             design.terms,
             "Set1") + theme_economist()

plot_gtrends(search.terms,
             design.terms,
             "Dark2") + theme_economist()

# short version: create google trends object from search terms
# ugly (, ,)
search.terms <- c("beef", "pork", "beans","tofu")
plot_gtrends(search.terms, , "Dark2", "Design Terms") + theme_economist()

# short version - explicit parameter (palette =)
plot_gtrends(search.terms, palette = "Dark2", title = "Design Terms") +
    theme_economist()

# simplest version: all-in-one
plot_gtrends(search.terms)

#####################################################################
#
#  Example for Trend Analysis in Design Research
#  Key: Compare the most popular tools, brands or activities
#
#####################################################################

## Trend in data visualization tools
search.terms <- c("ggplot2", "Qlikview",
                  "FusionCharts", "Highcharts"
                  # "tableau"
                  # "tableau software"
                  )
datavis <- gtrends(search.terms) # max. 5 search terms

# conversion to numeric results
datavis$interest_over_time$hits %<>% as.numeric

# plot (alternative with dplyr)
datavis %>% plot
datavis %>% plot + theme_economist()

# plot (with trend line)
datavis %>% plot +
    theme_economist() +
    # scale_y_log10() +
    geom_smooth()

#
# Task: alternative to above code with plot_gtrends()?
#








datavis <- plot_gtrends(search.terms)

# Note: geom_smooth() doesn't work
datavis +
    theme_economist() +
    geom_smooth()

# Note: theme_economist() doesn't work
datavis %>% plot +
    theme_economist() +
    geom_smooth()

# works because plot_gtrends() returns a ggplot object
datavis
datavis + theme_economist()
datavis + theme_economist() + scale_y_log10()

# works because plot_gtrends() returns a gtrends object in .$gtrends
datavis$gtrends %>% plot
datavis$gtrends %>% plot + geom_smooth()
datavis$gtrends %>% plot + theme_economist() + geom_smooth()


#####################################################################
#
#  New Example for Trend Analysis:
#  Social Communication Decline due to Smartphone Overuse
#
#####################################################################

search.terms.social <- c("talk",
                         "conversation",
                         "smartphone",
                         "truthful", # "truthfulness",
                         # "smartphone overuse"
                         "authentic"
)

social.life <-  plot_gtrends(search.terms.social)

social.life +  theme_economist()

social.life$gtrends %>% plot + theme_economist()

# see trend clearer by trend lines
social.life$gtrends %>% plot + theme_economist() + geom_smooth()

# remove "truthful"
plot_gtrends(c("talk","conversation","smartphone","authentic"),
             social.life$gtrends, palette = "Spectral") + theme_economist()

# remove "talk"
plot_gtrends(c("conversation","smartphone","authentic"),
             social.life$gtrends) + theme_economist()

# remove "smartphone"
plot_gtrends(c("conversation","authentic"),
             social.life$gtrends) + theme_economist()

#####################################################################
#
#  Examples for Market Analysis
#  Key: Formulate a *concrete* question > get a clear answer
#
#####################################################################

# which is the most popular league?
basketball <- gtrends(c("nhl", "nba", "nfl"))
basketball %>% plot
plot_gtrends(c("nhl", "nba", "nfl"), basketball , "Set1", "Basketball Leagues")

# which is the most popular sports?
search.terms <- c("soccer", "football", "rugby")
sports <- gtrends(search.terms)
sports %>% plot
sports.title <- paste(search.terms, collapse = ", ")
plot_gtrends(search.terms, sports , "Set1", sports.title)

#####################################################################
#  Doesn't work
#####################################################################

## highlighting regions (probably countries) and cities
plot(sports, type = "region")
plot(sports, type = "cities")




