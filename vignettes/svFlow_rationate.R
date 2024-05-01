## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(svFlow)

## ----error=TRUE---------------------------------------------------------------
library(svFlow)
# An example pipeline with an error in the middle:
library(dplyr)
iris %>.%
  filter(., Sepal.Length < 5.1, Sepal.Width < 3.1) %>.%
  mutate(., logS = log(Species)) %>.%
  group_by(., Species) %>.%
  summarise(., mean_logS = mean(logS))

## ----error=TRUE---------------------------------------------------------------
# Both . and .call are available and can be explored
head(.)
.call
eval(.call)

## ----error=TRUE---------------------------------------------------------------
debug_flow()

## -----------------------------------------------------------------------------
data(iris)
fl <- iris %>_% # Create a Flow object
  filter(., Sepal.Length < 5.1, Sepal.Width < 3.1) %>_%
  mutate(., logSL = log(Sepal.Length))
# Interrupt the pipeline, and inspect or modify the flow object:
fl

## -----------------------------------------------------------------------------
fl %>_%
  group_by(., Species) %>_%
  summarise(., mean_logSL = mean(logSL)) %>_% . # Get final result

## -----------------------------------------------------------------------------
fl <- flow(iris, var1_ = Sepal.Length, thresh1 = 5.1)
str(fl)

## -----------------------------------------------------------------------------
fl <- flow(iris,
    var1_      = Sepal.Length,
    var2_      = Sepal.Width,
    var_group_ = Species,
    var1_log_  = logSL,
    var1_mean_ = mean_logSL,
    thresh1    = 5.1,
    thresh2    = 3.1) %>_%
  filter(., var1_ < thresh1_, var2_ < thresh2_) %>_%
  {..$temp_data <- mutate(., var1_log_ = log(var1_))} %>_%
  group_by(., var_group_) %>_%
  summarise(., var1_mean_ = mean(var1_log_))
str(fl)
fl$temp_data # The temporary variable
fl %>_% . # The final results

