# env
library(ggmice)
library(ggplot2)
# data
dat <- data.frame(
  Y = c(1, 1, NA, 1, NA),
  X = rep(1, 5)
)
R <- data.frame(apply(!is.na(dat), 2, as.numeric))
long_R <- data.frame(row = 1:5, vrb = c(rep("Y", 5), rep("X", 5)), ind = as.numeric(!is.na(dat)))
# plot
gg <- ggplot(long_R, aes(x = vrb, y = row, fill = as.factor(ind))) +
  geom_tile(color = "black") +
  scale_y_reverse() +
  scale_fill_manual(values = c(
    "1" = "#006CC2B3",
    "0" = "#B61A51B3"
  )) +
  scale_x_discrete(position = "top") +
  coord_fixed(expand = FALSE) +
  ggmice:::theme_minimice() +
  labs(x = NULL, y = NULL, fill = "R")
