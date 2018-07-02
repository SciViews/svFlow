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

## ------------------------------------------------------------------------
flow(iris, var_ = Petal.Length, threshold = 1.5) %>_%
  filter(., Sepal.Length < 5) %>_%
  filter(., var_ < threshold_) %>_%
  {..$tab <- mutate(., logSL = log(Sepal.Length))} %>_%
  head(.) %>_% .

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

## ------------------------------------------------------------------------
#urchin %>%
#  mutate(ratio = skeleton / weight) %>%
#  group_by(origin) %>%
#  summarise(mean = mean(ratio, na.rm = TRUE))

## ------------------------------------------------------------------------
#na_rm <- TRUE
#urchin %>%
#  mutate(ratio = skeleton / weight) %>%
#  group_by(origin) %>%
#  summarise(mean = mean(ratio, na.rm = !!na_rm))

## ------------------------------------------------------------------------
#x <- quo(skeleton)
#y <- quo(weight)
#group <- quo(origin)
#na_rm <- TRUE
#urchin %>%
#  mutate(ratio = !!x / !!y) %>%
#  group_by(!!group) %>%
#  summarise(mean = mean(ratio, na.rm = !!na_rm))

## ------------------------------------------------------------------------
#x <- quo(skeleton)
#y <- quo(weight)
#xy_name <- "ratio"
#group <- quo(origin)
#na_rm <- TRUE
#urchin %>%
#  mutate(!!xy_name := !!x / !!y) %>%
#  group_by(!!group) %>%
#  summarise(mean = mean(ratio, na.rm = !!na_rm))

## ------------------------------------------------------------------------
#x <- quo(skeleton)
#y <- quo(weight)
#xy_name <- "ratio"
#xy <- quo(ratio)
#group <- quo(origin)
#na_rm <- TRUE
#urchin %>%
#  mutate(!!xy_name := !!x / !!y) %>%
#  group_by(!!group) %>%
#  summarise(mean = mean(!!xy, na.rm = !!na_rm))

## ------------------------------------------------------------------------
#flow(urchin) %>+%
#  mutate(ratio = skeleton / weight) %>+%
#  group_by(origin) %>+%
#  summarise(mean = mean(ratio, na.rm = TRUE))

## ------------------------------------------------------------------------
#flow(urchin) %>+%
#  mutate(ratio = skeleton / weight) %>+%
#  group_by(origin) %>+%
#  summarise(mean = mean(ratio, na.rm = TRUE)) %>+% .

## ------------------------------------------------------------------------
#flow(urchin, x_ = skeleton, y_ = weight, group_ = origin, xy_ = ratio, na_rm = TRUE) %>_%
#  mutate(xy_ = x_ / y_) %>_%
#  group_by(group_) %>_%
#  summarise(mean = mean(xy_, na.rm = na_rm_)) %>_% .

