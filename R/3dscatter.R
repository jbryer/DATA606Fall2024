library(scatterplot3d)

poverty <- read.table("course_data/poverty.txt", h = T, sep = "\t")
names(poverty) <- c("state", "metro_res", "white", "hs_grad", "poverty", "female_house")
poverty <- poverty[,c(1,5,2,3,4,6)]

s3d <- scatterplot3d(poverty[,c('female_house', 'white', 'poverty')], 
					 type = "p", color = "blue", angle = 55, pch = 16)
lm_out <- lm(poverty ~ female_house + white, data = poverty)
s3d$plane3d(lm_out)
