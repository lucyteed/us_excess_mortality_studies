## CREATE OBSERVED PLOT
library(tidyverse)
library(lubridate)
library(png)

# read observed dataset
obs <- read.csv("~/cdc-weekly_counts.csv", header = TRUE, sep = ',')

# format date correctly to work with ggplot
obs$year <- format(as.Date(obs$week_end, format="%m/%d/%Y"),"%Y")
obs$month <- format(as.Date(obs$week_end, format="%m/%d/%Y"),"%m")
obs$month <- as.numeric(obs$month)
obs$date <- make_date(year = obs$year, month = obs$month)

# aggregate to monthly
obs <- aggregate(cbind(total_observed)~date,
                 data=obs,FUN=sum)

# plot observed counts
observed <- ggplot(d, aes(x = date)) +
  geom_point(aes(y = observed), alpha = 0.65, color = "black") +
  geom_smooth (aes(y = observed), alpha=0.2, linewidth=0, fill = "#fc5d28") +
  stat_smooth (geom="line", aes(y = observed), span=0.1, color = "#fc5d28", linewidth = 1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype = "twodash", 
             color = "#0282cc", size=0.8) +
  scale_x_date(breaks = scales::pretty_breaks(n = 11), date_labels = "%b %Y") +
  labs(title = "Monthly Observed Deaths, January 2015-June 2023") +
  theme(plot.title=element_text(size=10, family="Arial",color="black"), 
        axis.text.x=element_text(size=10, family="Arial", color="black"),
        axis.text.y=element_text(size=10, family="Arial",color="black"),
        axis.title=element_text(size=10, family="Arial",color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey"),
        panel.grid.major.y = element_line(color = "lightgrey")) +
  labs(x = "",
       y = "Observed Deaths")

# save plot as png
ggsave(observed, file="~/observed_plot.png", width=16.9, height=7.58)
