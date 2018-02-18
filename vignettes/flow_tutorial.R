## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# TODO: to be reworked!
library(dplyr)
library(rlang)
library(flow)
y <- 1.5
iris %>.%
  filter(., Sepal.Length < 5) %>.%
  filter(., Petal.Length < y) %>.%
  mutate(., logSL = log(Sepal.Length)) %>.%
  head(.) %>.% .

#flow(iris, var = ~Petal.Length, threshold = 1.5) %>.%
#  filter(., Sepal.Length < 5) %>.%
#  filter(., !!var < ..$threshold) %>.%
#  {..$tab <- mutate(., logSL = log(Sepal.Length))} %>.%
#  head(.) %>.% .

## ------------------------------------------------------------------------
# Create a flow from flow code (transform %>.% into ->.; in function body!)
#transform1 <-
#  flow_function(x, var = Petal.Length, y = 1.5) %.>%
#  filter(., Sepal.Length < 5) %.>%
#  filter(., !! var < y) %.>%
#  mutate(., logSL = log(Sepal.Length)) %.>%
#  head(.) %.>% .

## ------------------------------------------------------------------------
# Create a new branch called lda
#..$branch("lda", model = mlLda(Species ~ ., data = .))
# Switching to a branch or referring to a branch: use one of those two syntaxes
#..@lda(model = mlLda(Species ~ ., data = .))
#..(lda)(model = mlLda(Species ~ ., data = .))

## ------------------------------------------------------------------------
#R_@stats$median
#R(stats)$median
#stats_ <- R_@stats
#stats_$median

