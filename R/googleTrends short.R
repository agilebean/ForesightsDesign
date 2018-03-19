#####################################################################
#
# Trend Analysis with Google Trends - Short Version
# Class: Foresights & Design
# Prof:  Dr. Chaehan So
#
#####################################################################

library(gtrendsR)
library(ggplot2)

# install library from Github, then comment out this line (#)
devtools::install_github("agilebean/ForesightsDesign")
library(ForesightsDesignTools)

#####################################################################
# 1. Get data from Google Trends API
#####################################################################

# define search terms
search.terms <- c("beef", "pork","tofu", "beans")

# retrieve data from google trends (requires internet connection)
gtrends.object <- gtrends(search.terms)

#####################################################################
# 2. run simplest version: all-in-one
#####################################################################

plot_gtrends(search.terms, gtrends.object)

#####################################################################
# 3. Customize plot with
#
# @palette:  color palette from RColorBrewer library
# View available color palettes > RColorBrewer::display.brewer.all()
# @title:   chart title
#
#####################################################################

# define chart title
my.title <- "Interest in meat & meat substitutes"

# use color palette "Set1", "Set2", "Set3"
plot_gtrends(search.terms,
             gtrends.object,
             palette = "Set2",
             title = my.title)

# use color palette "Dark2", "Paired", "Accent"
plot_gtrends(search.terms,
             gtrends.object,
             palette = "Dark2",
             title = my.title)

#####################################################################
# 3. Optional: Use a different theme
# Available themes: For a complete list, type theme_ (a popup shows)
# theme_bw(), theme_grey(), theme_light(),
# theme_excel(), theme_gdocs(), theme_fivethirtyeight()
# theme_pander(), theme_few(), theme_classic(),
# theme_hc(), theme_tufte(), theme_wsj(), theme_stata()
#####################################################################

library(ggthemes)
plot_gtrends(search.terms,
             gtrends.object,
             palette = "Dark2",
             title = my.title) +
    theme_economist()









