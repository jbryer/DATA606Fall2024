library(dplyr)
library(ggplot2)
library(brickset)
library(yardstick)

data(legosets, package = 'brickset')

table(legosets$year)

legosets <- legosets |>
	dplyr::mutate(volume = height * width * depth,
				  minifigs = ifelse(is.na(minifigs), 0, minifigs)) |>
	dplyr::mutate(US_retailPÇrice = log(US_retailPrice)) |>
	dplyr::select(setID, name, year, US_retailPrice, pieces, minifigs, volume) |>
	tidyr::drop_na()

legosets_train <- legosets |> dplyr::filter(year == 2022)
legosets_valid <- legosets |> dplyr::filter(year == 2023)

# legosets_train |> select(US_retailPrice, pieces, minifu)

lm(US_retailPrice ~ pieces, data = legosets_train) |> summary()
lm(US_retailPrice ~ pieces, data = legosets_train) |> anova()

lm_train <- lm(US_retailPrice ~ pieces + minifigs + volume, data = legosets_train)
summary(lm_train)
aov_out <- anova(lm_train)
aov_outm

legosets_train$predicted_price <- predict(lm_train)
legosets_train$residual <- resid(lm_train)

# total_var <- var(legosets_train$US_retailPrice)
total_var <- sum((legosets_train$US_retailPrice - mean(legosets_train$US_retailPrice))^2)
error_var <- sum(legosets_train$residual^2)

ggplot(data = legosets_train, aes(x = US_retailPrice, y = predicted_price)) +
	geom_rect(xmin = 0, ymin = 0, xmax = sqrt(total_var), ymax = sqrt(total_var), fill = 'lightblue') +
	geom_rect(xmin = 0, ymin = 0, xmax = sqrt(error_var), ymax = sqrt(error_var), fill = 'maroon') +
	xlim(0, sqrt(total_var)) + ylim(0, sqrt(total_var))
	# geom_rect(xmin = 0, ymin = 0, xmax = sqrt(total_var - error_var), ymax = sqrt(total_var - error_var), fill = 'lightgreen')

1 - error_var / total_var

sum(legosets_train$residual^2)

# R-Squared
1 - sum(legosets_train$residual^2) / sum((legosets_train$US_retailPrice - mean(legosets_train$US_retailPrice))^2)


legosets_valid$predicted_price <- predict(lm_train, newdata = legosets_valid)

legosets_valid$residual <- legosets_valid$predicted_price - legosets_valid$US_retailPrice

legosets_train$residual_squared <- legosets_train$residual ^ 2
legosets_valid$residual_squared <- legosets_valid$residual ^ 2

# R-Squared
(var(legosets_train$US_retailPrice) - mean(legosets_train$residual_squared)) / var(legosets_train$US_retailPrice)
yardstick::rsq_vec(legosets_train$US_retailPrice, legosets_train$predicted_price)


var(legosets_train$US_retailPrice)

var(legosets_valid$US_retailPrice)

ggplot(legosets_train, aes(x = US_retailPrice, y = predicted_price)) +
	ggtitle('Actual Retail Price vs Predicted Retail Price', subtitle = 'Training data: 2022') +
	geom_abline(slope = 1, intercept = 0) +
	geom_point() +
	coord_equal() +
	geom_hline(yintercept = mean(legosets_train$predicted_price), linetype = 2) +
	geom_vline(xintercept = mean(legosets_train$US_retailPrice), linetype = 2) +
	# geom_rect(xmin = mean(legosets_train$US_retailPrice) - mean(legosets_train$residual^2),
	# 		  xmax = mean(legosets_train$US_retailPrice) + mean(legosets_train$residual^2),
	# 		  ymin = mean(legosets_train$predicted_price) - mean(legosets_train$residual^2),
	# 		  ymax = mean(legosets_train$predicted_price) + mean(legosets_train$residual^2),
	# 		  fill = 'maroon',
	# 		  alpha = 0.1) +
	geom_segment(aes(x = US_retailPrice, y = predicted_price, xend = US_retailPrice, yend = US_retailPrice)) +
	geom_rect(data = legosets_train[legosets_train$residual > 0,],
		aes(xmin = US_retailPrice, xmax = US_retailPrice - residual,
				  ymin = US_retailPrice, ymax = predicted_price),
			  alpha = 0.2) +
	geom_rect(data = legosets_train[legosets_train$residual < 0,],
			  aes(xmin = US_retailPrice, xmax = US_retailPrice + abs(residual),
			  	ymin = US_retailPrice, ymax = predicted_price),
			  alpha = 0.2)

nrow(legosets_train)
sqrt(total_var)

yardstick::rsq_vec(legosets_train$US_retailPrice, legosets_train$predicted_price)

ggplot(legosets_valid, aes(x = US_retailPrice, y = predicted_price)) +
	ggtitle('Actual Retail Price vs Predicted Retail Price', subtitle = 'Validation data: 2023') +
	geom_abline(slope = 1, intercept = 0) +
	geom_point() +
	coord_equal() +
	geom_segment(aes(x = US_retailPrice, y = predicted_price, xend = US_retailPrice, yend = US_retailPrice))

(var(legosets_valid$US_retailPrice) - mean(legosets_valid$residual_squared)) / var(legosets_valid$US_retailPrice)
yardstick::rsq_trad_vec(legosets_valid$US_retailPrice, legosets_valid$predicted_price)
cor.test(legosets_valid$US_retailPrice, legosets_valid$predicted_price)
cor(legosets_valid$US_retailPrice, legosets_valid$predicted_price)^2
stats::cov.wt(cbind(legosets_valid$US_retailPrice, legosets_valid$predicted_price), cor = TRUE, method = 'unbiased')$cor[[1,2]]^2

error_var <- var(legosets_valid$residual)
total_var <- var(legosets_valid$US_retailPrice)

1 - (error_var / total_var)

# R-Squared
(var(legosets_valid$US_retailPrice) - mean(legosets_valid$residual_squared)) / var(legosets_valid$US_retailPrice)


######
# Adapted from:
# https://stackoverflow.com/questions/19096983/when-simulating-multivariate-data-for-regression-how-can-i-set-the-r-squared-e
simulate <- function(n.obs = 500,
					 beta = c(5, 3, -2),
					 R.sq = 0.8) {
	stopifnot(length(beta) == 3)
	df <- data.frame(x1 = rnorm(n.obs), x2 = rnorm(n.obs))  # x1 and x2 are independent
	var.epsilon <- (beta[2]^2 + beta[3]^2) * (1 - R.sq) / R.sq
	stopifnot(var.epsilon > 0)
	df$epsilon <- rnorm(n.obs, sd=sqrt(var.epsilon))
	df$y <- with(df, beta[1] + beta[2]*x1 + beta[3]*x2 + epsilon)
	return(df)
}
