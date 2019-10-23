# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "lubridate",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue",
                     "ggbeeswarm",
                     "ggrepel")

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
         budget = as.numeric(str_remove_all(budget, "[$,]"))/10^6,
         movie_run_time = as.numeric(str_remove_all(movie_run_time, " min")),
         movie_rating = as_factor(movie_rating),
         language = as_factor(language))

# Add release year and budget_band
horror_movies <- horror_movies %>% 
  mutate(release_year = year(release_date),
         budget_band = case_when(budget <=0.01 ~ "$0-10k",
                                 budget <=0.05 ~"$10-50k",
                                 budget <= 0.1 ~ "$50-100k",
                                 budget <= 0.5 ~ "$100-500k",
                                 budget <= 2 ~ "$500k-2mil",
                                 budget <= 5 ~ "$2-5mil",
                                 budget <= 50 ~ "$5-50mil",
                                 budget > 50 ~ "+$50mil"),
         budget_band = as_factor(budget_band))

# Reorder factor levels for plotting
horror_movies$budget_band =  fct_relevel(horror_movies$budget_band, 
                                         c("$0-10k",
                                           "$10-50k",
                                           "$50-100k",
                                           "$100-500k",
                                           "$500k-2mil",
                                           "$2-5mil",
                                           "$5-50mil",
                                           "+$50mil"))

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


### PLOT RATING VS BUDGET -----------------------------------------------------
# Calculate mean rating for each budget_band for plot
mean_rating_df <- horror_movies %>%
  filter(!is.na(budget_band)) %>% 
  group_by(budget_band) %>% 
  summarise(mean_rating = mean(review_rating, na.rm = T))

# Plot rating versus budget_band with means shown and labelled
horror_movies %>%
  filter(!is.na(review_rating) & !is.na(budget_band) & !is.na(release_year)) %>% 
  ggplot(aes(x = budget_band, y = review_rating)) +
  geom_quasirandom(aes(colour = as_factor(release_year)), alpha = 0.8) +
  geom_point(data = mean_rating_df, aes(x = budget_band, y = mean_rating), 
             colour = "black", size = 3, shape = 15) +
  geom_text_repel(data = mean_rating_df, aes(x = budget_band, y = mean_rating, 
                                              label = format(signif(mean_rating, 2), 1)),
                  nudge_y = 1 + mean_rating_df$mean_rating,
                  nudge_x = 0.4,
                  segment.size  = 0.3,
                  segment.color = "grey50",
                  size = 3) +
  horror_plot_theme +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = "Spectral") +
  labs(title = "For horror movies, bigger budget equals better rating",
       subtitle = "Rating versus budget (with mean labelled)\n",
       x = "",
       y = "",
       caption = horror_caption) +
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = pretty_breaks(5),
                     expand = c(0.2, 0.2)) +
  ggsave("budget_vs_rating.png", width = 8, height = 8*9/16)
  



