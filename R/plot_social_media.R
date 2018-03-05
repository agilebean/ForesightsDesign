######################################################################################
#
# Show FB activities : likes, comments, shares
#
######################################################################################
plot_fb_activities <- function(page, title, theme, color_scheme = NULL, lineplot = FALSE)
{
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

    # scatterplots for FB likes, comments, shares
    tt <- tt.base +
        geom_point(aes(y=likes_count, color="likes")) + # color string = name in legendå
        geom_point(aes(y=shares_count, color="shares"), alpha = 0.9) +
        geom_point(aes(y=comments_count, color="comments"), alpha = 0.2)

    tt
}
