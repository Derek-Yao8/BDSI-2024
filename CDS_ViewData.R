library(dplyr)
library(ggplot2)
library(forcats)
# library(DIMPLE)
# line for installing MItools; only need to run once
# devtools::install_github("junsoukchoi/MItools", force = TRUE)
library(spatstat.explore)
library(spatstat.geom)
library(MItools)

ovarianData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/ovarianData.RDS"
)
lungData <- readRDS(
  "/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/lungData.RDS"
)

lungKdata <- read.csv("/Users/derekyao/Documents/BDSI/Cancer Data Science/Data/lungKdata.csv")

# Creating a recurrence status variable in the lung metadata
lungMeta_recurrence_count <- lungMeta %>%
  mutate(recurrence_status = case_when(is.na(time_to_recurrence_days) ~ 0,
                   !is.na(time_to_recurrence_days) ~ 1))

lungMeta_recurrence_count %>%
  count(recurrence_status)

# Proportion of those in lung data that have recurrence is around 62.2%

lungMeta %>%
  filter(is.na(cause_of_death), recurrence_or_lung_ca_death == 1) %>%
  count()
