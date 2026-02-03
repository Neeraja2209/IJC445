#Installing necessary libraries
install.packages("tidyverse") 
install.packages("car")
install.packages("corrplot")
install.packages(("RColorBrewer"))
install.packages("treemapify")
install.packages("plotly")

#Loading packages
library(tidyverse)
library(dplyr)
library(car)
library(corrplot)
library(broom)
library(RColorBrewer)
library(treemapify)
library(plotly)

#Reading dataset 
music_data<-read.csv("data_viz_dataset.csv", header = TRUE)



#Rearranging data in a more favourable format
music_data$duration_sec <- music_data$duration_ms/1000 #removing ms time measure and replacing it with second ~ gets me lower numbers thus easier to interpret
music_data <- music_data %>% select(artist_id, album_id, song_id, release_date, release_date_precision,
                                    artist_name, artist_popularity, song_name, 
                                    song_popularity, genre, explicit, duration_ms, key, mode,
                                    time_signature, acousticness, danceability, energy, 
                                    instrumentalness, liveness, loudness, speechiness, valence,
                                    tempo, song_rank_score, song_peak_position, song_weeks_on_chart)

#Standardising and scaling the data for comparing audio features
audio_features <- c("danceability", "energy", "loudness", "speechiness",
                    "acousticness", "instrumentalness", "liveness",
                    "valence", "tempo")

music_data_stds <- music_data %>%
  filter(!is.na(song_popularity), !is.na(genre)) %>%
  mutate(across(all_of(audio_features), scale))

#############################################################################################
#comparing genre popularity
# filtering data for 10 genres(to avoid cluttering the visual with too many genres) 
genre_filtered_data <- music_data_stds %>% 
  filter(genre %in% c("pop", "hip hop","dance pop","disco", "contemporary country", "classic soul",
                            "rock-and-roll", "canadian pop", "boy band", "classic uk pop"))

#Creating boxplot
ggplot(genre_filtered_data, aes(x = genre, y = song_popularity)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Popularity Distribution by Genre",
    x = "Genre",
    y = "Popularity"
  )

#############################################################################################
#treemap for genre wise average song popularity

genre_summary <- genre_filtered_data %>%
  group_by(genre) %>%
  summarise(avg_popularity = mean(song_popularity, na.rm = TRUE))


ggplot(genre_summary, 
       aes(area = avg_popularity, 
           fill = genre,
           label = paste(genre, "\n", round(avg_popularity, 1)))) +
  
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    reflow = TRUE,
                    min.size = 5) +
  
  labs(
    title = "Average Song Popularity Score by Genre",
    subtitle = "Popularity scale from 0–100",
    caption = "MusicOSet Data (1964-2020)"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

#############################################################################################
#linear regression model for temporal effect of year on popularity by genre

#Extracting year from  release date for temporal analysis
genre_filtered_data <- genre_filtered_data %>%
  mutate(
    year = as.numeric(str_extract(release_date, "^\\d{4}"))
  )
# Fitting separate linear regression for each genre to estimate how song popularity changes over time
genre_model <- genre_filtered_data %>%
  group_by(genre) %>%
  do(tidy(lm(song_popularity ~ year, data = .)))

#creating faceted regression scatter plot
ggplot(genre_filtered_data, aes(x = year, y = song_popularity, colour = genre)) +
  
  geom_point(alpha = 0.3) +
  
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  
  facet_wrap(~ genre, scales = "free_y") +
  
  theme_minimal() +
  
  labs(
    title = "Effect of Year on Popularity by Genre",
    subtitle = "Linear regression lines showing temporal trends",
    x = "Year",
    y = "Popularity"
  ) +
  
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )
 
#############################################################################################
#Interactive line plot showing trends based on weeks on chart by genre over the years
# Extracting year from the release date column
music_data <- music_data %>%
  mutate(
    year = as.numeric(str_extract(release_date, "^\\d{4}"))
  )
# Calculating average weeks on chart for each year per genre
weeks_summary <- music_data %>%
  group_by(year, genre) %>%
  summarise(avg_weeks = mean(song_weeks_on_chart, na.rm = TRUE)) %>%
  ungroup()
#Extracting unique values of genres and sorting them to use as the legend for the line plot

genres <- sort(unique(weeks_summary$genre))

#Selecting default genres to show on the plot in the beginning
default_genres <-c("boy band","canadian pop","dance pop","hip hop", "pop" )

#Creating an empty plot
line_plot <- plot_ly()

#Adding time series line by genre to the plot and setting default legend values
for (g in genres) {
  genre_data <- weeks_summary %>% filter(genre == g)
  line_plot <- line_plot %>%
    add_trace(
      x = genre_data$year,
      y = genre_data$avg_weeks,
      type = 'scatter',
      mode = 'lines+markers',
      name = g,
      visible = ifelse(g %in% default_genres, TRUE, "legendonly")
    )
}
#Creating layout with the legend and slider to interactively choose the genres and time frame
line_plot <- line_plot %>%
  layout(
    title = "Average Weeks on Chart by Genre Over Years",
    xaxis = list(title = "Year", rangeslider = list(visible = TRUE)),
    yaxis = list(title = "Average Weeks on Chart"),
    legend = list(title = list(text = "Click a genre to show/hide"))
  )

line_plot

#############################################################################################
#Aggregating by genre
genre_feature_summary <- genre_filtered_data %>%
  select(genre, all_of(audio_features)) %>%
  group_by(genre) %>%
  summarise(across(all_of(audio_features), mean))

#Converting to long format for heatmap plotting
genre_feature_long <- genre_feature_summary %>%
  pivot_longer(
    cols = -genre,
    names_to = "Audio_Feature",
    values_to = "Mean_Scaled_Value"
  )

#Plotting heatmap
ggplot(genre_feature_long,
       aes(x = Audio_Feature, y = genre, fill = Mean_Scaled_Value)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#4575B4", "#FFFFBF", "#D73027"),
    name = "Relative Association"
  ) +
  theme_minimal() +
  labs(
    title = "Correlation Between Genres and Audio Features",
    x = "Audio Features",
    y = "Genre"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

#############################################################################################
#boxplot for artist popularity by genre
# Plot boxplot
ggplot(genre_filtered_data, aes(x = genre, y = artist_popularity, fill = genre)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Distribution of Artist Popularity by Genre",
       x = "Genre",
       y = "Artist Popularity") +
  theme_minimal() +
  theme(legend.position = "none")

#############################################################################################
#End of code