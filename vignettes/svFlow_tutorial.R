## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dplyr)
library(svFlow)
threshold <- 1.5
iris %>.%
  filter(., Petal.Length > threshold) %>.%
  mutate(., log_var = log(Petal.Length)) %>.%
  head(.) %>.% .

## -----------------------------------------------------------------------------
flow(iris, var_ = Petal.Length, threshold = 1.5) %>_%
  filter(., var_ > threshold_) %>_%
  {..$tab <- mutate(., log_var = log(var_))} %>_%
  head(.) %>_% .

## -----------------------------------------------------------------------------
my_process <- function(data, var_ = Petal.Length, threshold = 1.5)
  enflow(data) %>_%
  filter(., var_ > threshold_) %>_%
  {..$tab <- mutate(., log_var = log(var_))} %>_%
  head(.) %>_% .


## -----------------------------------------------------------------------------
my_process(iris)

## -----------------------------------------------------------------------------
my_process(iris, var_ = Sepal.Width, threshold = 0.5)

## -----------------------------------------------------------------------------
# Create a new branch called lda
#..$branch("lda", model = mlLda(Species ~ ., data = .))
# Switching to a branch or referring to a branch: use one of those two syntaxes
#..@lda(model = mlLda(Species ~ ., data = .))
#..(lda)(model = mlLda(Species ~ ., data = .))

