library(tibble)
data(mtcars)
mtcars_tbl <- as_tibble(mtcars)

class(mtcars)
class(mtcars_tbl)

print(mtcars)
class(mtcars)
print.data.frame(mtcars)

print(mtcars_tbl)
class(mtcars_tbl)
print.tbl_df(mtcars_tbl)

`[`(mtcars, 1)
mtcars[,1]

is.data.frame(mtcars)
is.data.frame(mtcars_tbl)

mtcars[,1]
mtcars[,1] |> class()
mtcars_tbl[,1]
mtcars_tbl[,1] |> class()

mtcars[,1,drop=FALSE] |> class()
mtcars[,1,drop=TRUE] |> class()
mtcars_tbl[,1,drop=TRUE] |> class()
