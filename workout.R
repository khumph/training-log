pacman::p_load(ggplot2, dplyr, googledrive, googlesheets4)

sheets_auth(email = 'khumph2@gmail.com')
files <- drive_find(q = "name = 'training-log'")
dat <- read_sheet(files$id[1], sheet = 1)

pct_1rm <- function(reps, rpe) {
  x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  x <- replace(x, rpe == 10 & reps == 1, 1)
  x <- replace(x, rpe == 10 & reps == 2, 0.98)
  x
}

date_3_months_ago <- seq(Sys.Date(), length = 2, by = '-3 months')[2]


# 1RM ---------------------------------------------------------------------

dat <- dat %>%
  mutate(date = as.Date(date),
         pct1rm = pct_1rm(reps, rpe),
         e1rm = weight / pct1rm) %>% 
  filter(date > date_3_months_ago) 

dat %>% 
  # filter(lift %in% c("Deadlift", "Press", "Squat", "Bench")) %>% 
  filter(lift != 'Session') %>% 
  group_by(date, lift) %>%
  summarise(e1rm = max(e1rm)) %>%
  ggplot(aes(x = date, y = e1rm,
             color = lift, linetype = lift, group = lift)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  labs(
    x = 'Date',
    y = 'Estimated 1 Rep Max',
    color = 'Lift',
    linetype = 'Lift'
  ) + theme(legend.position = "bottom")

dat %>%
  filter(lift %in% c("Deadlift", "Press", "Squat", "Bench")) %>%
  group_by(lift) %>%
  filter(e1rm == max(e1rm, na.rm = T)) %>%
  arrange(desc(e1rm)) %>%
  select(date, lift, weight, reps, rpe, e1rm, pct1rm) %>% 
  knitr::kable()


# Tonnage -----------------------------------------------------------------

tonnage <- dat %>%
  filter(lift != 'Session') %>%
  mutate(tonnage = weight * reps) %>%
  group_by(date, lift) %>% 
  summarise(tonnage = sum(tonnage)) 

ggplot(tonnage, aes(y = tonnage, x = date, color = lift)) +
  geom_point() + 
  geom_line() + 
  theme(legend.position = "bottom")

tonnage %>%
  group_by(date) %>%
  summarise(tonnage = sum(tonnage)) %>% 
  ggplot(aes(y = tonnage, x = date)) +
  geom_point() + 
  geom_line() + 
  theme(legend.position = "bottom") + 
  labs(x = 'Date', y = 'Total tonnage (lbs)')


# Volume ------------------------------------------------------------------

dat %>%
  group_by(date, lift) %>% 
  summarise(Volume = sum(reps)) %>% 
  ggplot(aes(y = Volume, x = date, color = lift)) +
  geom_point() + 
  geom_line() +
  labs(x = 'Date', y = 'Volume', color = "Lift") +
  theme(legend.position = "bottom")


# Average intensity -------------------------------------------------------

dat %>%
  filter(lift != 'Session') %>%
  group_by(date, lift) %>%
  summarise(intensity = mean(pct1rm)) %>%
  ggplot(aes(y = intensity, x = date, color = lift)) +
  geom_point() +
  geom_line() +
  labs(x = 'Date',
       y = 'Average intensity (% of estimated 1RM)',
       color = "Lift") +
  theme(legend.position = "bottom")


# Session RPE -------------------------------------------------------------

dat %>%
  filter(lift == 'Session') %>% 
  group_by(date) %>% 
  ggplot(aes(y = rpe, x = date, color = lift)) +
  geom_point() + 
  geom_line() +
  labs(x = 'Date', y = 'Session RPE', color = "Lift") +
  theme(legend.position = "bottom")
