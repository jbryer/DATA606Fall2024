library(ggplot2)
library(TriMatch)

data(tutoring)

tutoring$treat2 <- tutoring$treat != 'Control'

lr_out <- glm(treat2 ~ Gender + Ethnicity + Military + ESL + EdMother + EdFather +
			  Age + Employment + Income + Transfer + GPA,
			  data = tutoring,
			  family = binomial(link = 'logit'))


tutoring$ps <- fitted(lr_out)

ggplot(tutoring, aes(x = ps, y = Grade, color = treat2)) +
	geom_smooth(method = 'loess', formula = y ~ x) +
	geom_point()
