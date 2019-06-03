#' ---
#' title: "Training log"
#' author: Kyle Humphrey
#' date: "Last updated `r gsub(' 0', ' ', format(Sys.Date(), '%B %d %Y'), fixed = T)`"
#' ---

pacman::p_load(ggplot2, dplyr, googledrive, googlesheets4, kableExtra)

sheets_auth(email = 'khumph2@gmail.com')
files <- drive_find(q = "name = 'training-log'")
dat <- read_sheet(files$id[1], sheet = 1)

pct_1rm <- function(reps, rpe) {
  x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  x <- replace(x, rpe == 10 & reps == 1, 1)
  x <- replace(x, rpe == 10 & reps == 2, 0.98)
  return(x)
}

plt <- function(dat, y, smooth = F) {
  out <- ggplot(dat, aes(
    x = date,
    y = !!enquo(y),
    color = lift,
    linetype = lift,
    group = lift,
  )) +
    geom_point() +
    labs(x = 'Date',
         color = 'Lift',
         linetype = 'Lift') +
    geom_line() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  if (smooth == T) {
    out <- out + geom_smooth(method = "lm")
  } else {
    out <- out + geom_line()
  }
  return(out)
}

filter_date <- seq(Sys.Date(), length = 2, by = '-1 months')[2]

dat <- dat %>%
  mutate(date = as.Date(date),
         pct1rm = round(pct_1rm(reps, rpe), 2),
         e1rm = round(weight / pct1rm)) %>% 
  filter(date >= filter_date)

dat_lifts <- dat %>%
  filter(lift != 'Session') %>% 
  group_by(date, lift)


# 1RM ---------------------------------------------------------------------

dat_lifts %>%
  summarise(e1rm = mean(e1rm)) %>%
  plt(e1rm) +
  labs(y = 'Estimated 1RM')
  
#+ results='asis'
dat_lifts %>%
  group_by(lift) %>%
  filter(e1rm == max(e1rm, na.rm = T)) %>%
  slice(1) %>% 
  select(Date = date, Lift = lift, Weight = weight, Reps = reps,
                RPE = rpe, e1RM = e1rm, `% of 1RM` = pct1rm) %>% 
  knitr::kable(caption = 'Estimated One Rep Maxes') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# Tonnage -----------------------------------------------------------------

tonnage <- dat_lifts %>%
  mutate(tonnage = weight * reps) %>%
  summarise(tonnage = sum(tonnage)) 

plt(tonnage, tonnage) +
  labs(y = "Tonnage")

tonnage %>%
  group_by(date) %>%
  summarise(tonnage = sum(tonnage)) %>%
  ggplot(aes(y = tonnage, x = date)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(x = 'Date', y = 'Total tonnage (lbs)')


# Volume ------------------------------------------------------------------

dat_lifts %>%
  summarise(Volume = sum(reps)) %>% 
  plt(Volume)


# Average intensity -------------------------------------------------------

dat_lifts %>%
  summarise(intensity = mean(pct1rm)) %>%
  plt(intensity) +
  labs(y = 'Average intensity (% of estimated 1RM)')


# Session RPE -------------------------------------------------------------

dat %>%
  filter(lift == 'Session') %>%
  group_by(date) %>% 
  plt(rpe) +
  labs(y = 'Session RPE') +
  theme(legend.position = 'none')
