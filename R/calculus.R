library(ggplot2)
library(dplyr)
library(cowplot)

ggplot2::theme_set(ggplot2::theme_minimal())

a <- 2 # Constant acceleration

df <- data.frame(time = 0:5)

# Acceleration
df <- df |> dplyr::mutate(acceleration = a)

a_plot <- ggplot(df, aes(x = time, y = acceleration)) + 
	geom_point() +
	geom_abline(intercept = a, slope = 0)
a_plot

# Velocity
df <- df |> dplyr::mutate(velocity = time * acceleration)

df

v_plot <- ggplot(df, aes(x = time, y = velocity)) + 
	geom_point() +
	geom_abline(intercept = 0, slope = a)
v_plot

# Distance

df <- df |> dplyr::mutate(distance = 0.5 * velocity * time)
df

d_plot <- ggplot(df, aes(x = time, y = distance)) + 
	geom_point() +
	stat_function(fun = function(t) { 0.5 * a * t^2 })
d_plot

cowplot::plot_grid(d_plot, v_plot, a_plot, ncol = 1, align = "v")
