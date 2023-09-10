## CREATE EXCESS PLOT
library(lubridate)
library(tidyverse)
library(common)
library(svglite)
library(magick)
library(png)

# read excess estimate dataset
excess <- read.csv("~/excess_estimates.csv", header = TRUE, sep = ',')

# format date correctly to work with ggplot
excess$month <- format(as.Date(excess$date, format="%Y-%m-%d"),"%m")
excess$year <- format(as.Date(excess$date, format="%Y-%m-%d"),"%Y")
excess$month <- as.numeric(excess$month)
excess$date <- make_date(year = excess$year, month = excess$month)

# create colors for sources
colors <- c("The Economist and Solstad      " = "#42e6ff",
            "Acosta and Irizarry" = "#d566fa",
            "Msemburi et al" = "#fc6868",
            "Institute for Health Metrics and Evaluation   " = "#b4d642")

# create plot for central estimates
g_exc <- ggplot(excess, aes(x = date)) +
  geom_smooth(aes(y = who_excess, color = "Msemburi et al"),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = economist_excess, color="The Economist and Solstad      "),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = harvard_excess, 
                  color="Acosta and Irizarry"),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = ihme_excess, 
                  color="Institute for Health Metrics and Evaluation   "),
              span=0.25,fill="white",linewidth=1.5) +
  geom_point(aes(y=who_excess), color="#fc6868", alpha=0.6) +
  geom_point(aes(y=economist_excess), color="#42e6ff", alpha=0.6) +
  geom_point(aes(y=harvard_excess), color="#d566fa", alpha=0.6) +
  geom_point(aes(y=ihme_excess), color="#b4d642", alpha=0.6) +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-31")), 
             linetype = "twodash", color = "#292929", size=1)+
  scale_x_date(breaks = scales::pretty_breaks(n = 24), date_labels = "%b %Y") +
  scale_y_continuous(breaks=scales::pretty_breaks(n = 5), limits=c(-80000, 200000)) +
  labs(title = "Monthly Excess Deaths, January 2022-June 2023") +
  theme(plot.title=element_text(size=10, family="Arial", color="black"), 
        axis.title=element_text(size=10, family="Arial",color="black"),
        axis.text.x=element_text(size=10, family="Arial",color="black"),
        axis.text.y=element_text(size=10, family="Arial",color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.major.x = element_line(color = "lightgrey"),
        legend.position = c(0.955, 0.98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(color="lightgrey",linewidth=1),
        legend.title=element_blank()
  ) +
  labs(x = "",
       y = "Excess Deaths",
       color="") +
  scale_color_manual(values=colors) +
  geom_line(aes(y=0))
ggsave(g_exc, file="~/exc_lines.tiff", width=16.9, height=7.58)

# create ribbons for WHO 95% confidence interval
ggp_who_lower <- ggplot(excess, aes(x=date,y=who_95_CI_lower)) +
  geom_point() +
  stat_smooth(span=0.25)
df_who_lower <- ggplot_build(ggp_who_lower)$data[[2]]
g_who_data <- data.frame(c(1:80), df_who_lower$y)
colnames(g_who_data)[1] <- "x"
colnames(g_who_data)[2] <- "lower"
ggp_who_upper <- ggplot(excess, aes(x=date,y=who_95_CI_upper)) +
  geom_point() +
  stat_smooth(span=0.25)
df_who_upper <- ggplot_build(ggp_who_upper)$data[[2]]
g_who_data$upper <- df_who_upper$y

# create ribbons for Acosta & Irizarry 95% confidence interval
ggp_harv_lower <- ggplot(excess, aes(x=date,y=harvard_95_CI_lower)) +
  geom_point() +
  stat_smooth(span=0.25)
df_harv_lower <- ggplot_build(ggp_harv_lower)$data[[2]]
g_harv_data <- data.frame(c(1:80), df_harv_lower$y)
colnames(g_harv_data)[1] <- "x"
colnames(g_harv_data)[2] <- "lower"
ggp_harv_upper <- ggplot(excess, aes(x=date,y=harvard_95_CI_upper)) +
  geom_point() +
  stat_smooth(span=0.25)
df_harv_upper <- ggplot_build(ggp_harv_upper)$data[[2]]
g_harv_data$upper <- df_harv_upper$y

# create ribbons for The Economist 95% confidence interval
ggp_econ_lower <- ggplot(excess, aes(x=date,y=economist_95_CI_lower)) +
  geom_point() +
  stat_smooth(span=0.25)
df_econ_lower <- ggplot_build(ggp_econ_lower)$data[[2]]
g_econ_data <- data.frame(c(1:80), df_econ_lower$y)
colnames(g_econ_data)[1] <- "x"
colnames(g_econ_data)[2] <- "lower"
ggp_econ_upper <- ggplot(excess, aes(x=date,y=economist_95_CI_upper)) +
  geom_point() +
  stat_smooth(span=0.25)
df_econ_upper <- ggplot_build(ggp_econ_upper)$data[[2]]
g_econ_data$upper <- df_econ_upper$y

# create ribbons for the IHME 95% confidence interval
ggp_ihme_lower <- ggplot(excess, aes(x=date,y=ihme_95_CI_lower)) +
  geom_point() +
  stat_smooth(span=0.25)
df_ihme_lower <- ggplot_build(ggp_ihme_lower)$data[[2]]
g_ihme_data <- data.frame(c(1:80), df_ihme_lower$y)
colnames(g_ihme_data)[1] <- "x"
colnames(g_ihme_data)[2] <- "lower"
ggp_ihme_upper <- ggplot(excess, aes(x=date,y=ihme_95_CI_upper)) +
  geom_point() +
  stat_smooth(span=0.25)
df_ihme_upper <- ggplot_build(ggp_ihme_upper)$data[[2]]
g_ihme_data$upper <- df_ihme_upper$y

# create dataframe for the smoothed confidence intervals
g_exc_dat <- data.frame(g_who_data$x,
                        g_who_data$lower, g_who_data$upper,
                        g_harv_data$lower, g_harv_data$upper,
                        g_econ_data$lower, g_econ_data$upper,
                        g_ihme_data$lower, g_ihme_data$upper)
colnames(g_exc_dat)[1] <- "x"
colnames(g_exc_dat)[2] <- "who_exc_lower"
colnames(g_exc_dat)[3] <- "who_exc_upper"
colnames(g_exc_dat)[4] <- "harv_exc_lower"
colnames(g_exc_dat)[5] <- "harv_exc_upper"
colnames(g_exc_dat)[6] <- "econ_exc_lower"
colnames(g_exc_dat)[7] <- "econ_exc_upper"
colnames(g_exc_dat)[8] <- "ihme_exc_lower"
colnames(g_exc_dat)[9] <- "ihme_exc_upper"

# create plot for the smoothed confidence intervals
g_exc_rib <- ggplot(g_exc_dat, aes(x = x)) +
  geom_ribbon(aes(ymin = who_exc_lower, 
                  ymax = who_exc_upper), fill = "#fc6868", alpha = 0.2) +
  geom_ribbon(aes(ymin = harv_exc_lower, 
                  ymax = harv_exc_upper), fill = "#d566fa", alpha = 0.2) +
  geom_ribbon(aes(ymin = econ_exc_lower, 
                  ymax = econ_exc_upper), fill = "#42e6ff", alpha = 0.2) +
  geom_ribbon(aes(ymin = ihme_exc_lower,
                  ymax = ihme_exc_upper), fill = "#8ac404", alpha = 0.2) +
  theme( 
    axis.text.x=element_text(size=10, family="Arial",color="transparent"),
    axis.text.y=element_text(size=14, family="Arial",color="transparent"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_line(color = "transparent"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  ylim(-80000, 200000) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 35))

# include references
g_exc_rib <- g_exc_rib + annotate("text", x=73,y=191560,label="2",size=2) +
  annotate("text", x=79.7,y=182060,label="4",size=2) +
  annotate("text", x=71.8,y=172700,label="3",size=2) +
  annotate("text", x=75.55,y=162800,label="1",size=2) 
ggsave(g_exc_rib, file="~/exc-ribbons.tiff", width=16.9, height=7.58)

# combine lines and smoothed confidence intervals
combined <- c(image_read("~/exc_lines.tiff"), image_read("~/exc-ribbons.tiff"))
final_plot <- image_mosaic(combined)

# save plot as png
image_write(final_plot, path = "~/excess_plot.png", format = "png")
