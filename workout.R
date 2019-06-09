#' ---
#' title: "Training log"
#' author: Kyle Humphrey
#' date: "Last updated `r gsub(' 0', ' ', format(Sys.Date(), '%B %d %Y'), fixed = T)`"
#' ---

pacman::p_load(ggplot2, dplyr, tidyr, googledrive, googlesheets4, kableExtra)

sheets_auth(email = 'khumph2@gmail.com')
files <- drive_find(q = "name = 'training-log'")
dat <- read_sheet(files$id[1], sheet = 1)

pct_1rm <- function(reps, rpe) {
  x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  if (any(rpe == 10)) {
    x <- replace(x, rpe == 10 & reps == 1, 1)
    x <- replace(x, rpe == 10 & reps == 2, 0.98)
  }
  return(x)
}

plt <- function(data, y, smooth = F) {
  out <- filter(data, param == y) %>% 
    ggplot(aes(
    x = date,
    y = value,
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

# filter_date <- seq(Sys.Date(), length = 2, by = '-1 months')[2]
filter_date <- as.Date('2019-05-28')

dat <- dat %>%
  mutate(date = as.Date(date),
         pct1rm = round(pct_1rm(reps, rpe), 2),
         e1rm = round(weight / pct1rm)) %>% 
  filter(date >= filter_date)

dat_lifts <- dat %>%
  filter(lift != 'Session') %>% 
  group_by(date, lift) %>%
  mutate(tonnage = weight * reps) %>% 
  summarise(
    e1rm = max(e1rm),
    tonnage = sum(tonnage),
    volume = sum(reps),
    intensity = mean(pct1rm)
  ) %>% 
  gather(param, value, -date, -lift)

#' # Estimated one rep max

dat_lifts %>%
  plt('e1rm') +
  labs(y = 'Estimated 1RM')
  
#+ results='asis'
dat %>%
  group_by(lift) %>%
  filter(e1rm == max(e1rm)) %>%
  slice(1) %>%
  select(Date = date, Lift = lift, Weight = weight, Reps = reps,
                RPE = rpe, e1RM = e1rm, `% of 1RM` = pct1rm) %>% 
  knitr::kable(caption = 'Estimated One Rep Maxes') %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#+ results='asis'
last_weeks <- seq(Sys.Date(), length = 3, by = '-1 weeks')

chng <- dat_lifts %>%
  group_by(lift) %>%
  filter(date > tail(last_weeks, 1), param == 'e1rm') %>%
  mutate(week = if_else(date <= last_weeks[2], 'last_wk', 'this_wk')) %>%
  group_by(lift, week) %>%
  summarise(avg_e1rm = mean(value, na.rm = T)) %>%
  ungroup() %>%
  spread(week, avg_e1rm)

if (!is.null(chng[['last_wk']])) {
  chng <- chng %>%
    mutate(chng = this_wk - last_wk) %>% 
    mutate_if(is.numeric, round) %>%
    mutate(chng = cell_spec(chng, "html", color = case_when(is.na(chng) ~ "black",
                                                            chng <= 0 ~ "red",
                                                            chng > 0 ~ "blue"))) %>% 
    rename(`Mean e1RM last week` = last_wk,
           `Change` = chng)
}

chng %>%
  rename(Lift = lift, `Mean e1RM this week` = this_wk) %>%
  knitr::kable(
    escape = F,
    align = 'lccc',
    caption = paste(
      'Change in estimated one rep maxes, average of last seven days compared',
      'to average of the seven days before that.'
    )
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


#' # Training variables

# Average Intensity ---------------------------------------------------------

#+ fig.cap=cap
cap <- paste('Volume over time. Volume is the total number of repetitions of',
             'each lift (i.e., sets times reps) performed on a given day')
dat_lifts %>% 
  plt('volume') +
  labs(y = 'Volume (total repetitions)')


# Volume ------------------------------------------------------------------

#+ fig.cap=cap
cap <- paste('Average intensity over time. Averge intensity is the average',
             'load, as measured by percent of estimated 1RM, across all',
             'working sets of a given lift on a given day.')
dat_lifts %>% 
  plt('intensity') +
  labs(y = 'Average intensity (% of estimated 1RM)')


# Tonnage -----------------------------------------------------------------

#+ fig.cap=cap
cap <- paste('Tonnage over time. Tonnage is the total weight lifted',
             '(i.e., weight times sets times reps) for a given lift.')
dat_lifts %>% 
  plt('tonnage') +
  labs(y = "Tonnage")

#+ fig.cap=cap
cap <- paste('Total tonnage over time. Total tonnage is the sum of the tonnage',
             'of all lifts performed on a given day.')
dat_lifts %>%
  group_by(date) %>%
  filter(param == 'tonnage') %>% 
  summarise(value = sum(value),
            param = unique(param),
            lift = '') %>%
  plt('tonnage') +
  theme(legend.position = 'none') +
  labs(x = 'Date', y = 'Total tonnage (lbs)')


# Training load -------------------------------------------------------------

#+ fig.cap=cap
cap <- paste('Training load in aribitrary units (A.U.) over time. Arbitrary,
             units are defined as the session RPE times the duration of the',
             'session in minutes.')
dat %>%
  filter(lift == 'Session') %>%
  mutate(value = rpe * time,
         param = 'tl') %>% 
  plt('tl') +
  labs(y = 'Training load (A.U.)') +
  theme(legend.position = 'none')
