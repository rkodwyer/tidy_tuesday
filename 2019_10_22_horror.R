# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "lubridate",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue")

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

### IMPORT DATA ---------------------------------------------------------------
horror_movies_org <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

horror_movies <- horror_movies_org

glimpse(horror_movies)

### PLOT THEME ----------------------------------------------------------------
# Create theme for plots
horror_plot_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", family = "mono"),
        plot.subtitle = element_text(size = 10, face = "bold", family = "mono"),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0))

# Create caption to correctly source all plots
horror_caption <- "Data from Georgios Karamanis"

### TIDY DATA -----------------------------------------------------------------
# Convert variables to right data type
horror_movies <- horror_movies %>% 
  mutate(release_date = dmy(release_date),
         release_country = as_factor(release_country),
         budget = as.numeric(str_remove_all(budget, "[$,]")),
         movie_run_time = as.numeric(str_remove_all(movie_run_time, " min")),
         movie_rating = as_factor(movie_rating),
         language = as_factor(language))

# Add release year
horror_movies <- horror_movies %>% 
  mutate(release_year = year(release_date))

horror_movies %>% 
  ggplot() +
  geom_point(aes(x = review_rating, y = movie_run_time))

# Summarise by year
horror_movies_summary <- horror_movies %>% 
  group_by(release_year) %>% 
  summarise(avg_rating = mean(review_rating, na.rm = T),
            min_rating = min(review_rating, na.rm = T),
            max_rating = max(review_rating, na.rm = T),
            avg_runtime = mean(movie_run_time, na.rm = T),
            min_runtime = min(movie_run_time, na.rm = T),
            max_runtime = max(movie_run_time, na.rm = T),
            avg_budget = mean(budget, na.rm = T),
            min_budget = min(budget, na.rm = T),
            max_budget = max(budget, na.rm = T)) %>% 
  pivot_longer(cols = 2:10, names_to = "statistic", values_to = "value")

### PLOT RATINGS ----------------------------------------------------------
ratings <- horror_movies_summary %>% 
  filter(statistic == "avg_rating" | 
           statistic == "min_rating" | 
           statistic == "max_rating") %>%
  drop_na()
  
ratings_labels <- ratings %>% 
  filter(statistic == "max_rating"| statistic == "min_rating") %>% 
  pivot_wider(names_from = statistic, values_from = value)
  
ggplot(data = ratings) +
  geom_point(aes(x = release_year, y = value, colour = statistic), size = 3) +
  geom_segment(data = ratings_labels,
               aes(x = release_year, xend = release_year, 
                   y = min_rating, yend = max_rating),
               colour = "darkgrey") +
  geom_text(aes(label = "min", x = 2017, y = 1), nudge_x = 0.6) +
  geom_text(aes(label = "mean", x = 2017, y = 5.5), nudge_x = 0.6) +
  geom_text(aes(label = "max", x = 2017, y = 9.8), nudge_x = 0.6) +
  horror_plot_theme +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  labs(title = "Average horror movie rating is rising",
       subtitle = "but range between lowest and highest rating is widening\n",
       y = "Rating",
       x = "",
       caption = horror_caption) +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = pretty_breaks(n = 9)) +
  ggsave("ratings.png", width = 8, height = 8*9/16)

  
