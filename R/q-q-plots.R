library(ggplot2)

set.seed(2112)

pop_size <- 100000
samp_size <- 50

distributions <- data.frame(
	unif_pop = runif(pop_size),
	skew_pop = rchisq(pop_size, df = 5),
	norm_pop = rnorm(pop_size, mean = 2, sd = 1)
)

# Let's consider a small random sample.
samp1 <- sample(distributions$skew_pop, size = 6)
samp1	
# Now we will create a random sample from the normal distribution of the same size.
samp_norm <- rnorm(length(samp1))
samp_norm

# We are then going to pair these to samples but need to first sort them.
samp1 <- samp1 |> sort()
samp_norm <- samp_norm |> sort()

# Now we can combine them.
cbind(samp1, samp_norm)

# Now can plot them. We expect the points to fall on a line.
ggplot(cbind(samp1, samp_norm), aes(x = samp1, y = samp_norm)) + 
	geom_point() + 
	theme_minimal()

# You may notice the scales on the two axes are different. That is because we
# did not specify a mean and standard deviation to the rnorm function. We can 
# get the scales to match by doing so.
samp_norm <- rnorm(length(samp1), mean = mean(samp1), sd = sd(samp1))

# The advantage here is we can add the unit line (i.e. y = x) since the metrics
# on the x and y axes are the same.
ggplot(cbind(samp1, samp_norm), aes(x = samp1, y = samp_norm)) + 
	geom_point() + 
	geom_abline() + 
	theme_minimal()

# Let's consider bigger samples and multiple population distributions.
# Plot the population distributions.
distributions |>
	reshape2::melt(variable.name = 'distribution') |> 
	ggplot(aes(x = value, color = distribution)) + 
	geom_density() + 
	facet_wrap(~ distribution, scales = 'free', ncol = 1) +
	theme_minimal()

# Draw random samples from each of our populations.
unif_samp <- sample(distributions$unif_pop, size = samp_size)
skew_pop <- sample(distributions$skew_pop, size = samp_size)
norm_pop <- sample(distributions$norm_pop, size = samp_size)

#' Q-Q Plot using ggplot2
#'
#' This is a version of the quantile-quantile (Q-Q) plot using `ggplot2`. In
#' addition to the Q-Q plot, marginal distributions are also provided.
#'
#' @param samp a numeric vector of the sample.
#' @param theoretical_dist the distribution function to use to draw a theoretical
#'        distribution. This can be any function that follows R's conventions for
#'        distributions. That is, there needs to be a `rXXX` and `qXXX` version
#'        where `XXX` is the name of the distribution function
#'        (e.g. `norm`, `t`, `chisq`).
#' @param bins the number of bins to use for the histograms.
#' @param probs numeric vector of length two, representing probabilities.
#'        Corresponding quantile pairs that define the line drawn.
#' @param title plot title.
#' @param xlab label for the x-axis.
#' @param ylab label for the y-axis.
#' @param theme a ggplot2 theme (optional).
#' @param ... other parameters passed to the `theoretical_dist` function.
#' @return a Q-Q plot.
#' @import ggplot2
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @export
#' @examples
#' pop_size <- 100000
#' samp_size <- 50
#'
#' distributions <- data.frame(
#' 	unif_pop = runif(pop_size),
#' 	skew_pop = rchisq(pop_size, df = 5),
#' 	norm_pop = rnorm(pop_size, mean = 2, sd = 1)
#' )
#'
#' # Draw random samples from each of our populations.
#' unif_samp <- sample(distributions$unif_pop, size = samp_size)
#' skew_pop <- sample(distributions$skew_pop, size = samp_size)
#' norm_pop <- sample(distributions$norm_pop, size = samp_size)
#'
#' # Q-Q plots for our three samples
#' gg_qq_plot(unif_samp, title = 'Normal Quantile-Quantile Plot (Uniform Population)')
#' gg_qq_plot(skew_pop, title = 'Normal Quantile-Quantile Plot (Skewed Population)')
#' gg_qq_plot(norm_pop, title = 'Normal Quantile-Quantile Plot (Normal Population)')
gg_qq_plot <- function(samp,
					   theoretical_dist = 'norm',
					   bins = 20,
					   probs = c(0.25, 0.75),
					   title,
					   xlab = 'Theoretical Quantiles',
					   ylab = 'Sample Quantiles',
					   theme,
					   ...) {
	df <- data.frame(
		Sample = sort(samp),
		Theoretical = sort(do.call(paste0('r', theoretical_dist), list(length(samp), ...)))
	)
	
	x_vals <- do.call(paste0('q', theoretical_dist), list(probs, ...))
	y_vals <- quantile(samp, probs, names = FALSE, type = 7, na.rm = TRUE)
	slope <- diff(y_vals) / diff(x_vals)
	intercept <- y_vals[1] - slope * x_vals[1]
	
	p_theoretical <- ggplot2::ggplot(df, aes(x = Theoretical)) +
		ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)),
								bins = bins, fill = 'grey50') +
		ggplot2::geom_density(color = 'blue') +
		ggplot2::theme(axis.text = ggplot2::element_blank(),
					   axis.ticks = ggplot2::element_blank()) +
		ggplot2::xlab('') + ggplot2::ylab('')
	p_sample <- ggplot2::ggplot(df, aes(x = Sample)) +
		ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density)),
								bins = bins, fill = 'grey50') +
		ggplot2::geom_density(color = 'blue') +
		ggplot2::coord_flip() +
		ggplot2::theme(axis.text = ggplot2::element_blank(),
					   axis.ticks = ggplot2::element_blank()) +
		ggplot2::xlab('') + ggplot2::ylab('')
	p_main <- ggplot2::ggplot(df, aes(x = Theoretical, y = Sample)) +
		ggplot2::geom_abline(slope = slope, intercept = intercept, color = 'blue') +
		ggplot2::geom_point() +
		ggplot2::xlab(xlab) + ggplot2::ylab(ylab)
	if(!missing(theme)) {
		p_theoretical <- p_theoretical + theme
		p_sample <- p_sample + theme
		p_main <- p_main + theme
	}
	p <- cowplot::plot_grid(p_theoretical, NULL, p_main, p_sample,
							nrow = 2, ncol = 2, align = 'hv',
							rel_widths = c(3, 1), rel_heights = c(1, 3))
	if(!missing(title)) {
		title <- cowplot::ggdraw() + cowplot::draw_label(title, fontface = 'bold')
		p <- cowplot::plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))
	}
	return(p)
}

# Q-Q plots for our three samples
gg_qq_plot(unif_samp, title = 'Normal Quantile-Quantile Plot (Uniform Population)')
gg_qq_plot(skew_pop, title = 'Normal Quantile-Quantile Plot (Skewed Population)')
gg_qq_plot(norm_pop, title = 'Normal Quantile-Quantile Plot (Normal Population)')

qqnorm(unif_samp); qqline(unif_samp)


gg_qq_plot(unif_samp, theoretical_dist = 'unif')
gg_qq_plot(skew_pop, theoretical_dist = 'chisq', df = 5)
gg_qq_plot(norm_pop, theoretical_dist = 'norm')
