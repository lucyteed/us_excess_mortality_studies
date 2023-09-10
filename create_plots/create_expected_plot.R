## CREATE EXPECTED PLOT
library(lubridate)
library(tidyverse)
library(common)
library(svglite)
library(magick)
library(png)

# read expected estimate dataset
expected <- read.csv("~/expected_estimates.csv", header = TRUE, sep = ',')

# format date correctly to work with ggplot
expected$month <- format(as.Date(expected$date, format="%Y-%m-%d"),"%m")
expected$year <- format(as.Date(expected$date, format="%Y-%m-%d"),"%Y")
expected$month <- as.numeric(expected$month)
expected$date <- make_date(year = expected$year, month = expected$month)

# create colors for sources
colors <- c("The Economist and Solstad      " = "#42e6ff",
            "Acosta and Irizarry" = "#d566fa",
            "Msemburi et al" = "#fc6868",
            "Institute for Health Metrics and Evaluation   " = "#b4d642")

# create plot for central estimates
g_exp <- ggplot(expected, aes(x = date)) +
  geom_smooth(aes(y = who_expected, color = "Msemburi et al"),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = economist_expected, color="The Economist and Solstad      "),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = harvard_expected, 
                  color="Acosta and Irizarry"),
              span=0.25,fill="white",linewidth=1.5) +
  geom_smooth(aes(y = ihme_expected, 
                  color="Institute for Health Metrics and Evaluation   "),
              span=0.25,fill="white",linewidth=1.5) +
  geom_point(aes(y=who_expected), color="#fc6868", alpha=0.6) +
  geom_point(aes(y=economist_expected), color="#42e6ff", alpha=0.6) +
  geom_point(aes(y=harvard_expected), color="#d566fa", alpha=0.6) +
  geom_point(aes(y=ihme_expected), color="#b4d642", alpha=0.6) +
  geom_vline(xintercept = as.numeric(as.Date("2022-12-31")), 
             linetype = "twodash", color = "#292929", size=1)+
  scale_x_date(breaks = scales::pretty_breaks(n = 24), date_labels = "%b %Y") +
  labs(title = "Monthly Expected Deaths, January 2022-June 2023") +
  theme(plot.title=element_text(size=10, family="Arial", color="black"), 
        axis.title=element_text(size=10, family="Arial",color="black"),
        axis.text.x=element_text(size=10, family="Arial",color="black"),
        axis.text.y=element_text(size=10, family="Arial",color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"),
        panel.grid.major.x = element_line(color = "lightgrey"),
        legend.position = c(0.95, 0.92),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(0.5,0.5,0.5,0.5),
        legend.box.background = element_rect(color="lightgrey",linewidth=1),
        legend.title=element_blank()
  ) +
  labs(x = "",
       y = "Expected Deaths",
       color="") +
  scale_color_manual(values=colors) +
  ylim(150000, 425000)
ggsave(g_exp, file="~/exp_lines.tiff", width=16.9, height=7.58)

# create ribbons for WHO 95% confidence interval
ggp_who_lower <- ggplot(expected, aes(x=date,y=who_expected_lower95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_who_lower <- ggplot_build(ggp_who_lower)$data[[2]]
g_who_data <- data.frame(c(1:80), df_who_lower$y)
colnames(g_who_data)[1] <- "x"
colnames(g_who_data)[2] <- "lower"
ggp_who_upper <- ggplot(expected, aes(x=date,y=who_expected_upper95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_who_upper <- ggplot_build(ggp_who_upper)$data[[2]]
g_who_data$upper <- df_who_upper$y

# create ribbons for Acosta & Irizarry 95% confidence interval
ggp_harv_lower <- ggplot(expected, aes(x=date,y=harvard_expected_lower95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_harv_lower <- ggplot_build(ggp_harv_lower)$data[[2]]
g_harv_data <- data.frame(c(1:80), df_harv_lower$y)
colnames(g_harv_data)[1] <- "x"
colnames(g_harv_data)[2] <- "lower"
ggp_harv_upper <- ggplot(expected, aes(x=date,y=harvard_expected_upper95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_harv_upper <- ggplot_build(ggp_harv_upper)$data[[2]]
g_harv_data$upper <- df_harv_upper$y

# create ribbons for The Economist 95% confidence interval
ggp_econ_lower <- ggplot(expected, aes(x=date,y=economist_expected_lower95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_econ_lower <- ggplot_build(ggp_econ_lower)$data[[2]]
g_econ_data <- data.frame(c(1:80), df_econ_lower$y)
colnames(g_econ_data)[1] <- "x"
colnames(g_econ_data)[2] <- "lower"
ggp_econ_upper <- ggplot(expected, aes(x=date,y=economist_expected_upper95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_econ_upper <- ggplot_build(ggp_econ_upper)$data[[2]]
g_econ_data$upper <- df_econ_upper$y

# create ribbons for the IHME 95% confidence interval
ggp_ihme_lower <- ggplot(expected, aes(x=date,y=ihme_expected_lower95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_ihme_lower <- ggplot_build(ggp_ihme_lower)$data[[2]]
g_ihme_data <- data.frame(c(1:80), df_ihme_lower$y)
colnames(g_ihme_data)[1] <- "x"
colnames(g_ihme_data)[2] <- "lower"
ggp_ihme_upper <- ggplot(expected, aes(x=date,y=ihme_expected_upper95)) +
  geom_point() +
  stat_smooth(span=0.25)
df_ihme_upper <- ggplot_build(ggp_ihme_upper)$data[[2]]
g_ihme_data$upper <- df_ihme_upper$y

# create dataframe for the smoothed confidence intervals
g_exp_dat <- data.frame(g_who_data$x,
                        g_who_data$lower, g_who_data$upper,
                        g_harv_data$lower, g_harv_data$upper,
                        g_economist_data$lower, g_economist_data$upper,
                        g_ihme_data$lower, g_ihme_data$upper)
colnames(g_exp_dat)[1] <- "x"
colnames(g_exp_dat)[2] <- "who_exp_lower"
colnames(g_exp_dat)[3] <- "who_exp_upper"
colnames(g_exp_dat)[4] <- "harv_exp_lower"
colnames(g_exp_dat)[5] <- "harv_exp_upper"
colnames(g_exp_dat)[6] <- "econ_exp_lower"
colnames(g_exp_dat)[7] <- "econ_exp_upper"
colnames(g_exp_dat)[8] <- "ihme_exp_lower"
colnames(g_exp_dat)[9] <- "ihme_exp_upper"

# create plot for the smoothed confidence intervals
g_exp_rib <- ggplot(g_exp_dat, aes(x = x)) +
  geom_ribbon(aes(ymin = who_exp_lower, 
                  ymax = who_exp_upper), fill = "#fc6868", alpha = 0.2) +
  geom_ribbon(aes(ymin = harv_exp_lower, 
                  ymax = harv_exp_upper), fill = "#d566fa", alpha = 0.2) +
  geom_ribbon(aes(ymin = econ_exp_lower, 
                  ymax = econ_exp_upper), fill = "#42e6ff", alpha = 0.2) +
  geom_ribbon(aes(ymin = ihme_exp_lower,
                  ymax = ihme_exp_upper), fill = "#8ac404", alpha = 0.2) +
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
  ylim(150000, 425000) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 35))

# include references
g_exp_rib <- g_exp_rib + annotate("text", x=72.5,y=400700,label="2",size=2) +
  annotate("text", x=79.2, y=390500,label="4",size=2) +
  annotate("text", x=71.3,y=379800,label="3",size=2) +
  annotate("text", x=75,y=371000,label="1",size=2)
ggsave(g_exp_rib, file="~/exp-ribbons.tiff", width=16.9, height=7.58)

# combine lines and smoothed confidence intervals
combined <- c(image_read("~/exp_lines.tiff"), image_read("~/exp-ribbons.tiff"))
final_plot <- image_mosaic(combined)

# save plot as png
image_write(final_plot, path = "~/expected_plot.png", format = "png")
