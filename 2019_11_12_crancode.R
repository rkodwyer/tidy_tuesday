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

### PLOT THEME ----------------------------------------------------------------
# Create theme for plots
cran_plot_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold", family = "mono"),
        plot.subtitle = element_text(size = 10, face = "bold", family = "mono"),
        plot.caption = element_text(size = 10, face = "italic", hjust = 0))

# Create caption to correctly source all plots
cran_caption <- "Data from Phillip Massicotte"

cran_pal <- brewer.pal(8, "Dark2")

### IMPORT DATA ---------------------------------------------------------------
cran_code_org <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

cran_code <- cran_code_org
glimpse(cran_code)

### TIDY DATA -----------------------------------------------------------------
cran_tidy <- cran_code %>% 
  mutate(language = fct_explicit_na(language,"Missing")) %>% 
  mutate(version = fct_explicit_na(version,"Missing")) %>% 
  mutate(total_lines = code + comment + blank) %>% 
  mutate(code_blank_ratio = code/blank) %>% 
  mutate(code_per_file = code/file) %>% 
  mutate(blank_per_file = blank/file)

### GROUP BY LANGUAGE ---------------------------------------------------------
# Filter for languages with more than 40 entries
cran_lang <- cran_tidy %>% 
  group_by(language) %>% 
  filter(n() > 40) %>% 
  summarise(lang_tot = n(),
            file_tot = sum(file), code_tot = sum(code), blank_tot = sum(blank),
            lines_tot = sum(total_lines),
            blank_mean = mean(blank), code_mean = mean(code), file_mean = mean(file),
            lines_mean = mean(total_lines),
            code_blank_ratio = code_tot/blank_tot,
            code_per_file = code_tot /file_tot,
            blank_per_file = blank_tot/file_tot,
            lines_per_file = lines_tot/file_tot) %>% 
  arrange(desc(lang_tot))

# Calculate % code and lines per file vs mean across top languages
cran_lang <-cran_lang %>% 
  mutate(code_vs_mean = code_per_file/mean(code_per_file) - 1,
         lines_vs_mean = lines_per_file/mean(lines_per_file) - 1) %>% 
  arrange(desc(code_vs_mean))

# Add flag for colouring plots
cran_lang_plot <- cran_lang %>% 
  mutate(flag_code = if_else(code_vs_mean < 0 , "neg", "pos"),
         flag_lines = if_else(lines_vs_mean < 0, "neg","pos"))

### PLOT - TOTAL LINES VS AVERAGE ---------------------------------------------
cran_lang_plot %>% 
  ggplot(aes(x = reorder(language, desc(lines_vs_mean)), y = lines_vs_mean)) +
  geom_point(aes(colour = flag_lines)) +
  geom_segment(aes(xend = language, yend = 0, colour = flag_lines)) +
  geom_text(aes(label = if_else(cran_lang_plot$language == "JavaScript", 
                                paste0(cran_lang_plot$lang_tot, " packages"), 
                                as.character(cran_lang_plot$lang_tot))),
            hjust = if_else(cran_lang_plot$flag_lines == "neg", 1.5, -0.3),
            #nudge_y = , 
  size = 3) +
  coord_flip() +
  labs(title = "Did BASH fix a SH verbosity problem?",
       subtitle = "Total lines per file versus mean of all languages\n",
       y = "% difference of total lines per file versus mean across all languages",
       x = "",
       caption = glue::glue("For languages with more than 40 packages; ", cran_caption)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  cran_plot_theme +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = c(cran_pal[4], cran_pal[5])) +
  ggsave("total_lines_vs_mean.png", width = 8, height = 8*9/16)

### PLOT - LINES OF CODE VS AVERAGE -------------------------------------------
cran_lang_plot %>% 
  ggplot(aes(x = reorder(language, desc(code_vs_mean)), y = code_vs_mean)) +
  geom_point(aes(colour = flag_code)) +
  geom_segment(aes(xend = language, yend = 0, colour = flag_code)) +
  coord_flip() +
  labs(title = "Bourne Again Shell really fixed the Bourne Shell verbosity problem",
       subtitle = "Lines of code per file versus mean of all languages\n",
       y = "% difference of lines of code per file versus mean across all languages",
       x = "",
       caption = glue::glue("For languages with more than 40 packages; ", cran_caption)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  cran_plot_theme +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = c(cran_pal[4], cran_pal[5])) +
  ggsave("code_lines_vs_mean.png", width = 8, height = 8*9/16)

  
