rm(list = ls())
pacman::p_load(ggplot2, dplyr, tidyr, googlesheets4, kableExtra, shiny)

last_month <- seq(Sys.Date(), length = 2, by = "-1 months") %>% tail(1)

metric_labels <- c("Estimated 1RM", "Volume", "Intensity", "Tonnage", "Training Load")
metrics <- c("e1rm", "volume", "intensity", "tonnage", "load")
names(metrics) <- metric_labels

captions <- c(
  "Estimated 1 rep max over time.",
  "Volume over time. Volume is the total number of repetitions (i.e., sets times reps) performed on a given day",
  "Average intensity over time. Averge intensity is the average load, as measured by percent of estimated 1RM, across all working sets of a given lift on a given day.",
  "Tonnage over time. Tonnage is the total weight lifted (i.e., weight times sets times reps).",
  "Training load in aribitrary units (A.U.) over time. Arbitrary units are defined as the session RPE times the duration of the session in minutes."
)
names(captions) <- metrics

ylabs <- c(
  'Estimated 1RM',
  'Volume (repetitions)',
  'Average intensity (% of estimated 1RM)',
  "Tonnage (lbs)",
  "Training load (A.U.)"
)
names(ylabs) <- metrics