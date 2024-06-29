
formals(mean)

myvar <- c(rnorm(10), NA)
myvar

mean(x = myvar, na.rm = TRUE)

args <- list()
args[['x']] <- myvar
args[['na.rm']] <- TRUE
args

do.call(mean, args)

f1 <- function(a = 'World') {
	return(a)
}
f1()

f2 <- function(salutation = 'Hello', ...) {
	return(paste0(salutation, ' ', f1(...)))
}

f2(a = 'Bikram', salutation = 'Goodbye')

f3 <- function(...) {
	params <- list(...)
	df <- data.frame(
		name = names(params),
		value = sapply(params, paste)
	)
	return(df)
}

f3()
f3(a = 'Hello',
   b = 'Bikram',
   c = 'Angela')

?sum

