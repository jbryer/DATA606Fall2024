data(mtcars)

model_formula <- mpg ~ cyl + disp + hp + drat + qsec
lm(model_formula, data = mtcars) |> lm.beta::lm.beta()

mycars_standardized <- lapply(mtcars[, all.vars(model_formula)], scale) 
lm(model_formula, data = mycars_standardized)

