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
