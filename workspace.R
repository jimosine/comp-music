library(spotifyr)
library(tidyverse)
library(knitr)
library(ggthemes)
#library(ggjoy)

kanye_albums <- get_playlist_audio_features("", "4bQNKK5gntETwqlI1RW9w1")
summary_all <- kanye_albums %>%
  summarise(
    mean_tempo = mean(tempo),
    mean_popularity = mean(track.popularity),
    mean_danceability = mean(danceability),
    mean_valence = mean(valence),
    mean_speechiness = mean(speechiness),
    mean_acousticness = mean(acousticness),
    mean_liveness = mean(liveness),
    sd_speechiness = sd(speechiness),
    sd_acousticness = sd(acousticness),
    sd_liveness = sd(liveness),
    median_speechiness = median(speechiness),
    median_acousticness = median(acousticness),
    median_liveness = median(liveness),
    mad_speechiness = mad(speechiness),
    mad_acousticness = mad(acousticness),
    mad_liveness = mad(liveness)
  )
summary_all

to_plot <- kanye_albums %>%
  valence = mean(valence)
  album = track.album.name

test <- kanye_albums %>% 
  #arrange(kanye, track.album.release_date) %>% 
  group_by(track.album.name, track.album.release_date) %>% 
  summarize(mean_popularity = mean(track.popularity),
            mean_dance = mean(danceability), std_dance = sd(danceability),
            mean_energy = mean(energy), std_energy = sd(energy),
            mean_key = mean(key), std_key = sd(key),
            mean_loudness = mean(loudness), std_loudness = sd(loudness),
            mean_mode = mean(mode), std_mode = sd(mode),
            mean_speechiness = mean(speechiness), std_speechiness = sd(speechiness),
            mean_acousticness = mean(acousticness), std_acousticness = sd(acousticness),
            mean_intrumentalness = mean(instrumentalness), std_instrumentalness = sd(instrumentalness),
            mean_liveness = mean(liveness), std_liveness = sd(liveness),
            mean_valence = mean(valence), std_valence = sd(valence),
            mean_tempo = mean(tempo), std_temp = sd(tempo)
  )

kanye_albums <- arrange(kanye_albums, desc(track.album.release_date))

#ADD MEAN FEATURES
kanye_albums  <- transform(kanye_albums,
    mean_tempo = mean(tempo),
    mean_valence = mean(valence),
    mean_speechiness = mean(speechiness),
    mean_energy = mean(energy)
)

#ADD POSITIVE COLUMN
kanye_albums <- kanye_albums %>% add_column(mood = 'yes')
kanye_albums <- kanye_albums %>% 
  mutate(mood= ifelse(track.album.name == 'The College Dropout' | track.album.name == 'Graduation' | track.album.name == 'Late Registration', "positive", "negative"))


#BOXPLOTS 1
p <- ggplot(kanye_albums, aes(x = reorder(track.album.release_date, desc(track.album.release_date)), y = valence, fill=mood)) + 
  geom_boxplot() 
p <- p + scale_x_discrete(labels=c("JESUS IS KING","KIDS SEE GHOSTS","ye","The Life Of Pablo", "Yeezus", "Watch The Throne", "My Beautiful Dark Twisted Fantasy", "808s & Heartbreak", "Graduation", "Late Registration", "The College Dropout"))
p <- p +  geom_hline(yintercept = 0.40, linetype = 3)
p <- p + coord_flip() + labs(title="Distribution of the valence feature",y="", x = "")

p <- p + annotate(
  "text",
  x = 2.5, y = 0.58,
  label = "Average\nvalence",
  vjust = 1, size = 2, color = "grey40"
)

p <- p + annotate(
  "curve",
  x = 2.5, y = 0.58,
  xend = 2.5, yend = 0.40,
  arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  color = "grey40")

p #+ theme_economist_white()

#BOXPLOTS 2
p2 <- ggplot(kanye_albums, aes(x = track.album.release_date, y = tempo)) + 
  geom_boxplot() 
p2 +  geom_hline(aes(yintercept = mean_tempo))

#BOXPLOTS 3
p3 <- ggplot(kanye_albums, aes(x = track.album.release_date, y = speechiness)) + 
  geom_boxplot() 
p3 +  geom_hline(aes(yintercept = mean_speechiness))

#BOXPLOTS 4
p4 <- ggplot(kanye_albums, aes(x = track.album.release_date, y = energy)) + 
  geom_boxplot() 
p4 +  geom_hline(aes(yintercept = mean_energy)) + geom_hline(aes(yintercept = 0.5, color = "red"))

# TEST

seperated_kanye <- kanye_albums %>% add_column(style = 'yes')
seperated_kanye <- seperated_kanye %>% 
  mutate(style = ifelse(track.album.name == 'The College Dropout' | track.album.name == 'Graduation' | track.album.name == 'Late Registration', "old", "new"))

mood1 <- kanye_albums %>% ggplot(aes(x = valence, y = energy, color = track.album.release_date)) + 
  geom_jitter(alpha = 0.75) + geom_hline(yintercept = 0.5) + geom_vline(xintercept = 0.5)
mood1

mood2 <- kanye_albums %>% ggplot(aes(x = valence, y = energy, color = style)) + 
  geom_jitter(alpha = 0.75) + geom_hline(yintercept = 0.5) + geom_vline(xintercept = 0.5)
mood2

