alpha <- 0.05
negative_t <- NULL
min_scale = -4
max_scale = 4


y <- rnorm(20, 5, 1)
x <- rnorm(20, 5, 1)

df <- tibble(x, y)
df <- df %>% gather(key = "grp", value = "num")
t_result <- t.test(num ~ grp, data = df, var.equal = TRUE)

t_test <- broom::tidy(t_result)

#t_test <- t_test %>% 
#  gather(key = "conf_limit", value = "confidence", starts_with("conf"))

crit_t <- round(qt(1-alpha/2, t_test$parameter), 2)


if(t_test$statistic < 0) {
  shaded_area <- c(min_scale, -crit_t)
} else {
  shaded_area <- c(crit_t, max_scale)
}

t_range <- tibble(t = c(min_scale, max_scale))
y_point <- 0.45

crit_y <- round(dt(crit_t, t_test$parameter), 2)
calc_y <- round(dt(t_test$statistic, t_test$parameter), 2)

arrows_df <- tibble(crit_xstart = crit_t, 
                    crit_xend = crit_t,
                    crit_ystart = crit_y,
                    crit_yend = crit_y + 0.05,
                    calc_xstart = t_test$statistic, 
                    calc_xend = t_test$statistic,
                    calc_ystart = calc_y,
                    calc_yend = calc_y + 0.05)

ggplot(t_test, aes(y = y_point)) +
  geom_vline(aes(xintercept = 0), color = "gray75") +
  stat_function(data = t_range,
                aes(x = t),
                fun = dt, args = list(df = t_test$parameter),
                xlim = shaded_area,
                geom = "area",
                fill = "gray") +
  stat_function(data = t_range, 
                aes(x = t),
                fun = dt, args = list(df = t_test$parameter)) +
  geom_segment(aes(x = conf.low, 
                   xend = conf.high, 
                   yend = y_point),
               color = "maroon",
               size = 1) + 
  geom_point(aes(x = conf.low), size = 1, color = "maroon") +
  geom_point(aes(x = conf.high), size = 1, color = "maroon") +
#  geom_point(aes(x = statistic, y = 0.005),
#             shape = 17,
#             color = "maroon",
#             size = 3) +
  geom_segment(data = arrows_df,
               aes(x = calc_xstart, 
                   xend = calc_xend,
                   y = 0, 
                   yend = 0.05),
               #position = position_nudge(y = 0.01),
               arrow = arrow(ends = "first",
                             type = "closed",
                             length = unit(2, "mm"))) +
  geom_text(data = arrows_df,
            aes(x = calc_xstart,
                y = 0.05),
            label = paste("t =", round(t_test$statistic, 2)),
            vjust = 0,
            position = position_nudge(y = 0.01))
               

#   
#   
#   geom_text(data = arrows_df, aes(x = crit_xstart, y = crit_ystart,
#             label = paste("crit.t =", crit_xstart)),
#             vjust = 1,
#             hjust = 1)
# 
# 
# 
# 
#   geom_segment(data = arrows_df,
#                aes(x = crit_xstart, 
#                    xend = crit_xend,
#                    y = crit_ystart, 
#                    yend = crit_yend),
#                color = "goldenrod") +
#   geom_segment(data = arrows_df,
#                aes(x = calc_xstart, 
#                    xend = calc_xend,
#                    y = calc_ystart, 
#                    yend = calc_yend))
# 
#   
#   
# 
# 
# ggplot(t_test, aes(y = 1)) + 
#   geom_point(aes(x = conf.low), color = "maroon") +
#   geom_point(aes(x = conf.high), color = "maroon") +
#   geom_segment(aes(x = conf.low, xend = conf.high, yend = 1)) +
#   geom_point(aes(x = estimate2 - estimate1),
#              shape = 17,
#              color = "maroon",
#              size = 3) +
#   labs(x = "Confidence Interval",
#        y = NULL) +
#   scale_x_continuous(breaks = seq(floor(t_test$conf.low), ceiling(t_test$conf.high), by = 0.2)) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# 
