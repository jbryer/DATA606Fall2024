library(ggplot2)

pop_prop <- 0.6
N <- 1000000
pop <- c(rep(0, N * (1 - pop_prop)), rep(1, N * pop_prop))
length(pop)
mean(pop)

samp_dist <- numeric(1000)
n <- 30
for(i in 1:length(samp_dist)) {
	samp <- sample(pop, size = n)
	samp_dist[i] <- mean(samp)
}

ggplot(data.frame(x = samp_dist), aes(x = x)) + geom_density()


# Let's consider multiple sample sizes (i.e. n) along with multiple bootstrap sizes
pop <- runif(1e6)

n <- seq(30, 500, by = 15)
n_boots <- seq(50, 1000, by = 50)

results <- expand.grid(n, n_boots)
attributes(results) <- NULL
results <- as.data.frame(results)
names(results) <- c('n', 'n_boots')
results$samp_mean <- NA
results$samp_se <- NA
results$boot_mean <- NA
results$boot_se <- NA

pb <- txtProgressBar(min = 0, max = nrow(results), initial = 0, style = 3)
for(i in seq_len(nrow(results))) {
	setTxtProgressBar(pb, i)
	samp <- sample(pop, size = results[i,]$n)
	results[i,]$samp_mean <- mean(samp)
	results[i,]$samp_se <- sd(samp) / sqrt(length(samp))
	boot_samp_dist <- numeric(results[i,]$n_boots)
	for(j in seq_len(results[i,]$n_boots)) {
		boot_samp_dist[j] <- sample(samp, size = length(samp), replace = TRUE) |> mean()
	}
	results[i,]$boot_mean <- mean(boot_samp_dist)
	results[i,]$boot_se <- sd(boot_samp_dist)
}
close(pb)

y_limits <- c(0, 0.075)
p_samp_size_se <- ggplot(results, aes(x = n, y = samp_se)) + 
	geom_point(fill = '#9ecae1', color = 'grey50', shape = 21) + 
	geom_smooth(color = 'darkgreen', se = FALSE, method = 'loess', formula = y ~ x) +
	ylim(y_limits) +
	ylab('Standard Error') +
	xlab('Sample size (n)') +
	ggtitle(latex2exp::TeX("Standard Error (SE = \\frac{\\sigma}{\\sqrt{n}})")) +
	scale_fill_gradient(low = '#deebf7', high = '#3182bd') +
	theme(legend.position = 'bottom')

p_boot_size_se <- 
	ggplot(results, aes(x = n_boots, y = boot_se)) + 
	geom_point(aes(fill = n), color = 'grey50', shape = 21) +
	geom_smooth(color = 'darkgreen', se = FALSE, method = 'loess', formula = y ~ x) +
	ylim(y_limits) +
	ylab('Standard Error') +
	xlab('Number of Bootstrap Samples') +
	ggtitle('Bootstrap Standard Error',
			subtitle = '(i.e. standard deviation of the bootstrap sample)') +
	scale_fill_gradient(low = '#deebf7', high = '#3182bd') #+ theme(legend.position = 'none')

cowplot::plot_grid(p_samp_size_se, p_boot_size_se)
