library(ggplot2)
library(gganimate)
library(gifski)
library(plotly)
source("./make_forecasts.R")

set.seed(9284)
y <- stats::rpois(n = 55, lambda = pmax(0.1, 1 + 10 * sinpi((5 + 1:55 )/ 6)))

df <- data.frame(
  date = seq(as.Date("2024-03-01"), by = "month", length.out = length(y)),
  value = y
)

df_forecasts <- make_rolling_forecasts(df = df)

ggp_gif <- ggplot(df_forecasts, aes(x = date_fixed)) +
  coord_cartesian(ylim = c(0, 3 * max(df$value))) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_label(x = unique(df_forecasts$split_date) - months(4),
             y = 2.8 * max(df$value), label = "NOW") +
  geom_vline(aes(xintercept = split_date), linetype = 3, color = "grey") +
  geom_line(aes(y = value), color = "grey", na.rm = TRUE) +
  geom_point(aes(y = value), na.rm = TRUE) +
  geom_ribbon(aes(ymin = lb_11_12, ymax = ub_11_12), alpha = 0.25, fill = "darkorange") +
  geom_ribbon(aes(ymin = lb_09_12, ymax = ub_09_12), alpha = 0.25, fill = "darkorange") +
  geom_ribbon(aes(ymin = lb_06_12, ymax = ub_06_12), alpha = 0.25, fill = "darkorange") +
  geom_point(aes(y = value), na.rm = TRUE) +
  geom_line(aes(y = point), color = "#EC5C39", na.rm = TRUE) +
  transition_states(
    index,
    transition_length = 1,
    state_length = 3
  ) +
  enter_fade() +
  exit_fade() +
  ease_aes('sine-in-out')

animate(ggp_gif, nframes = 110, fps = 10, height = 200, width = 500, units = "px")
anim_save("arima_animation.gif", animation = last_animation(), path = ".")

# Alternatively, use `plotly` for a non-GIF interactive animated visualization,
# making it much easier to spot the extreme trend prediction at index 9!

ggp_for_plotly <- ggplot(df_forecasts, aes(x = date_fixed)) +
  coord_cartesian(ylim = c(0, 3 * max(df$value))) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  geom_line(aes(y = value, frame = index), color = "grey", na.rm = TRUE) +
  geom_point(aes(y = value, frame = index), na.rm = TRUE) +
  geom_ribbon(aes(ymin = lb_11_12, ymax = ub_11_12, frame = index), alpha = 0.25, fill = "darkorange") +
  geom_ribbon(aes(ymin = lb_09_12, ymax = ub_09_12, frame = index), alpha = 0.25, fill = "darkorange") +
  geom_ribbon(aes(ymin = lb_06_12, ymax = ub_06_12, frame = index), alpha = 0.25, fill = "darkorange") +
  geom_point(aes(y = value, frame = index), na.rm = TRUE) +
  geom_line(aes(y = point, frame = index), color = "#EC5C39", na.rm = TRUE)

ggplotly(ggp_for_plotly) |>
  config(displayModeBar = FALSE) |>
  style(hoverinfo = "none") |>
  animation_opts()
