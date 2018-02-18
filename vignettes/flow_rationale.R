## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----error=TRUE----------------------------------------------------------
library(flow)
# An example pipeline with an error in the middle:
library(dplyr)
iris %>.%
  filter(., Sepal.Length < 5.1, Sepal.Width < 3.1) %>.%
  mutate(., logS = log(Species)) %>.%
  group_by(., Species) %>.%
  summarise(., mean_logS = mean(logS))

## ----error=TRUE----------------------------------------------------------
# Both . and .call are available and can be explored
head(.)
.call
eval(.call)

## ----error=TRUE----------------------------------------------------------
debug_flow()

