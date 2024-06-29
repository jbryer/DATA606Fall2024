# From https://www.kaggle.com/datasets/imnikhilanand/heart-attack-prediction?select=data.csv
# Outcome: num: diagnosis of heart disease (angiographic disease status)
library(tidyverse)
library(mice)

heart <- read.csv('course_data/heart_attack_predictions.csv')

heart <- heart |>
	mutate_if(is.character, as.numeric) |>
	select(!c(slope, ca, thal))

str(heart)

# Impute missing values
mice_out <- mice(heart, m = 1)
heart_complete <- complete(mice_out)

set.seed(2112)
train_rows <- sample(nrow(heart_complete), size = .7 * nrow(heart_complete))
heart_train <- heart_complete[train_rows,]
heart_valid <- heart_complete[-train_rows,]

train_out <- glm(num ~ ., data = heart_train, family = binomial(link = 'logit'))

heart_valid$predictions <- predict(train_out, newdata = heart_valid, type = 'response')
heart_valid$predicted_num <- heart_valid$predictions > 0.5

table(heart_valid$num, useNA = 'ifany') |>
	print() |>
	prop.table()

table(heart_valid$num, heart_valid$predicted_num, useNA = 'ifany') |>
	print() |>
	prop.table()



# lr_out <- glm(num ~ ., data = heart_complete, family = binomial(link = 'logit'))
# summary(lr_out)
