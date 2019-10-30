# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "lubridate",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue",
                     "ggbeeswarm",
                     "ggrepel", 
                     "wesanderson",
                     "gganimate",
                     "gifski")

# Uncomment to install packages
# for (package in package_list) {
#  install.packages(package)
#}

# Call libraries
for (package in package_list) {
  library(package, character.only = T)
}

# Clean up
rm(package_list, package)

### PLOT THEME ----------------------------------------------------------------
# Create theme for plots
squirrel_plot_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", family = "mono"),
        plot.subtitle = element_text(size = 10, face = "bold", family = "mono"),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0))

# Create caption to correctly source all plots
squirrel_caption <- "Data from Squirrel Census 2019"

# Create colour palette
squirrel_pal <- wes_palette("IsleofDogs1", 6, type = "continuous")

### IMPORT DATA ---------------------------------------------------------------
squirrel_org <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

squirrel <- squirrel_org

### SET DATA TYPES ------------------------------------------------------------
# Factorise and change date to correct format
squirrel_tidy <- squirrel %>% 
  mutate(shift = fct_explicit_na(shift, "Unknown"),
         age = fct_explicit_na(age, "Unknown"),
         primary_fur_color = fct_explicit_na(primary_fur_color, "Unknown"),
         highlight_fur_color = fct_explicit_na(highlight_fur_color, "Unknown"),
         location = fct_explicit_na(location, "Unknown"),
         date = mdy(date))

# Change logical variables from T/F to 1/0
squirrel_tidy <- squirrel_tidy %>% 
  mutate_if(is_logical, ~as.numeric(.))

# Add day variable, showing day of week squirrel was sighted
squirrel_tidy <- squirrel_tidy %>% 
  mutate(day = as_factor(wday(date, label = T, abbr = T, week_start = 1)))

### FRIENDLIEST SQUIRRELS -----------------------------------------------------
# Group by color and calculate means for 0/1 variables to get pct of squirrels
# doing that activity
friend <- squirrel_tidy %>% 
  group_by(primary_fur_color, date) %>% 
  summarise_if(is.numeric, ~mean(.)) %>% 
  select(primary_fur_color, date, approaches)

# Create plot of pct of squirrels apporaching spotter each day, grouped by color
friend_plot <- friend %>% 
  filter(primary_fur_color != "Unknown") %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = approaches, color = primary_fur_color), size = 3) +
  geom_line(aes(y = approaches, color = primary_fur_color)) +
  squirrel_plot_theme +
  scale_color_manual(values = c(squirrel_pal[4], squirrel_pal[2], 
                                squirrel_pal[6], squirrel_pal[3]), name = "") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.05, 0.4)) +
  scale_x_date(breaks = pretty_breaks(n = 16), labels = date_format("%a-%b-%d")) +
  theme(axis.text.x = element_text(angle = 90), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right") +
  labs(title = "Cinnamon squirrels are the friendliest",
       subtitle = "Percentage of squirrels reported as approaching the squirrel spotter\n",
       x = "",
       y = "",
       caption = squirrel_caption) +
  transition_reveal(date)

# Animate the plot with a pause at the end, save as gif
friend_animate <- animate(friend_plot, end_pause = 25) 
anim_save("friendly_squirrels.gif", friend_animate)


### OTHER PLOTS ---------------------------------------------------------------------
# Group by primary fur color, summarise T/F variables using mean to get % 
# of squirrels doing that activity
squirrel_sum <- squirrel_tidy %>%
  group_by(primary_fur_color) %>% 
  summarise_if(is.numeric, ~mean(.))

color <- squirrel_sum %>% 
  select(primary_fur_color, running, chasing, climbing, eating, foraging) %>% 
  pivot_longer(cols = 2:6, names_to = "activity", values_to = "activity_pct") %>% 
  mutate(activity = as_factor(activity))

color %>%
  ggplot(aes(x = primary_fur_color)) +
  geom_col(aes(y = activity_pct, fill = primary_fur_color)) +
  facet_wrap(~activity) +
  coord_flip()

noise <- squirrel_sum %>% 
  select(primary_fur_color, kuks, quaas, moans) %>% 
  pivot_longer(cols = 2:4, names_to = "noise", values_to = "noise_pct") %>% 
  mutate(noise = as_factor(noise))
  
noise %>%
  ggplot(aes(x = primary_fur_color)) +
  geom_col(aes(y = noise_pct, fill = noise)) +
  facet_wrap(~noise) +
  coord_flip()

### DAILY SQUIRRELS -----------------------------------------------------------
daily <- squirrel_tidy %>% 
  group_by(day, primary_fur_color) %>% 
  summarise(count = n())

mean_daily <- daily %>%
  group_by(primary_fur_color) %>% 
  summarise(mean_per_day = mean(count))

daily_joined <- daily %>%
  left_join(mean_daily, by = "primary_fur_color") %>% 
  mutate(daily_vs_avg = count/mean_per_day - 1)

daily_joined %>% 
  ggplot(aes(x = reorder(day, desc(day)))) +
  geom_point(aes(y = daily_vs_avg, color = primary_fur_color)) +
  geom_segment(aes(y = daily_vs_avg,
                   xend = day, yend = 0, color = primary_fur_color)) +
  coord_flip() +
  facet_wrap(~primary_fur_color) +
  squirrel_plot_theme +
  labs(title = "You will have more luck spotting squirrels on weekends",
       subtitle = "Squirrel sighting per day versus weekly average",
       y = "",
       x = "") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none", panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_color_manual(values = c(squirrel_pal[4], squirrel_pal[3], 
                                squirrel_pal[6], squirrel_pal[1])) +
  ggsave("squirrel_per_day.png", width = 8, height = 8*9/16)
  

