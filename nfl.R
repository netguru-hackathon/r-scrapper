library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(ggrepel)

# Data from https://github.com/ryurko/nflscrapR-data/blob/master/play_by_play_data/regular_season/reg_pbp_2019.csv
pbp <- read_csv("data/reg_pbp_2019.csv")
# Data from https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv
nfl_logos <- read_csv("data/teamlogos.csv")

# Examples based on tutorial https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701
pbp <- 
  pbp %>%
  filter(!is_na(epa), play_type == "no_play" | play_type == "pass" | play_type == "run") %>%
  mutate(
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)(right end)"), 1, 0),
    success = if_else(epa > 0, 1, 0),
    passer_player_name =
      ifelse(
        play_type == "no_play" & pass == 1, 
        str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
        passer_player_name
      ),
    receiver_player_name =
      ifelse(
        play_type == "no_play" & str_detect(desc, "pass"), 
        str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
        receiver_player_name
      ),
    rusher_player_name =
      ifelse(
        play_type == "no_play" & rush == 1, 
        str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
        rusher_player_name
      ),
    passer_player_name =
      ifelse(passer_player_name == "G.Minshew II", "G.Minshew", passer_player_name)
    )

cle_rushers_epa_and_success_rate <-
  pbp %>%
  filter(posteam == "CLE", rush == 1, down <= 4) %>%
  group_by(rusher_player_name) %>%
  summarize(
    mean_epa = mean(epa),
    success_rate = mean(success),
    ypc = mean(yards_gained),
    plays = n()
  ) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays > 1)

neutral_situations_run_game <-
  pbp %>%
  filter(wp > 0.2 & wp < 0.8 & down <= 2 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(mean_pass = mean(pass), plays = n()) %>%
  arrange(mean_pass)

ggplot(neutral_situations_run_game, aes(x = reorder(posteam, -mean_pass), y = mean_pass)) +
  geom_text(aes(label = posteam))

ggsave("graphs/neutral_situations_run_game.png", dpi = 1000)

epa_and_success_rate_per_pass <-
  pbp %>%
  filter(pass == 1) %>%
  group_by(posteam) %>%
  summarize(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  ) %>%
  left_join(nfl_logos, by = c("posteam" = "team_code"))

display_logo <- geom_image(aes(image = url), size = 0.05)
default_theme <-
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12)
  )

ggplot(epa_and_success_rate_per_pass, aes(x = success_rate, y = epa_per_db)) +
  labs(
    x = "Success rate",
    y = "EPA per play",
    caption = "Data from nflscrapR",
    title = "Dropback success rate & EPA/play",
    subtitle = "Through week 12"
  ) +
  display_logo +
  default_theme

ggsave("graphs/epa_and_success_rate_per_pass.png", dpi = 1000)

early_down_epa_and_success_rate <- 
  pbp %>%
  group_by(posteam) %>%
  filter(down <= 2) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_dropback = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_dropback = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  ) %>%
  left_join(nfl_logos, by = c("posteam" = "team_code"))

ggplot(early_down_epa_and_success_rate, aes(x = epa_per_rush, y = epa_per_dropback)) +
  labs(
    x = "Rush EPA/play",
    y = "Pass EPA/play",
    caption = "Data from nflscrapR",
    title = "Early-down rush and pass EPA/play",
    subtitle = "Through week 12"
  ) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  display_logo +
  default_theme

ggsave("graphs/early_down_epa_per_play.png", dpi = 1000)

ggplot(early_down_epa_and_success_rate, aes(x = success_per_rush, y = success_per_dropback)) +
  labs(
    x = "Rush success rate",
    y = "Pass success rate",
    caption = "Data from nflscrapR",
    title = "Early-down rush and pass success rate",
    subtitle = "Through week 12"
  ) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  display_logo +
  default_theme

ggsave("graphs/early_down_success_rate.png", dpi = 1000)

qb_epa_and_success_rate <-
  pbp %>%
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa) / n_plays,
    success_per_play = sum(epa) / n_plays
  ) %>%
  filter(n_dropbacks >= 100)

ggplot(qb_epa_and_success_rate, aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(
    yintercept = mean(qb_epa_and_success_rate$epa_per_play),
    color = "red",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mean(qb_epa_and_success_rate$success_per_play),
    color = "red",
    linetype = "dashed"
  ) +
  geom_point(
    color = ifelse(qb_epa_and_success_rate$posteam == "CLE", "red", "black"),
    cex = qb_epa_and_success_rate$n_plays / 60, alpha = 1/4
  ) +
  geom_text_repel(
    aes(label = name),
    force = 1,
    point.padding = 0,
    segment.size = 0.1
  ) +
  labs(
    x = "Success rate",
    y = "EPA per play",
    caption = "Data from nflscrapR",
    title = "QB success rate and EPA/play",
    subtitle = "Through week 12, min 100 pass attempts, includes all QB's pass and rush play"
  ) +
  default_theme

ggsave("graphs/qb_epa_and_success_rate.png", dpi = 1000)

# Own examples
goal_line_epa_and_success_rate <-
  pbp %>%
  filter(yardline_100 <= 2) %>%
  group_by(posteam) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_pass = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_pass = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  ) %>%
  left_join(nfl_logos, by = c("posteam" = "team_code"))

ggplot(goal_line_epa_and_success_rate, aes(x = epa_per_rush, y = epa_per_pass)) +
  labs(
    x = "Rush EPA/play",
    y = "Pass EPA/play",
    caption = "Data from nflscrapR",
    title = "Pass and rush EPA in goal line situations (2 and less to go)",
    subtitle = "Through week 12"
  ) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  display_logo +
  default_theme
  
ggsave("graphs/goal_line_epa_per_play.png", dpi = 1000)

ggplot(goal_line_epa_and_success_rate, aes(x = success_per_rush, y = success_per_pass)) +
  labs(
    x = "Rush success rate",
    y = "Pass success_rate",
    caption = "Data from nflscrapR",
    title = "Pass and rush success rate in goal line situations (2 and less to go)",
    subtitle = "Through week 12"
  ) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  display_logo +
  default_theme

ggsave("graphs/goal_line_success_rate.png", dpi = 1000)

qb_pass_location_epa <-
  pbp %>%
  filter(!is_na(pass_location)) %>%
  mutate(
    pass_left = ifelse(pass_location == "left", 1, 0),
    pass_other = ifelse(pass_location == "left", 0, 1),
    play = 1
  ) %>%
  group_by(passer_player_name, posteam) %>%
  summarize(
    n_plays = sum(play),
    epa_pass_left = sum(epa * pass_left) / sum(pass_left),
    epa_pass_other = sum(epa * pass_other) / sum(pass_other)
  ) %>%
  filter(n_plays >= 100)

ggplot(qb_pass_location_epa, aes(x = epa_pass_other, y = epa_pass_left)) +
  geom_hline(yintercept = mean(qb_pass_locations$epa_pass_left), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(qb_pass_locations$epa_pass_other), color = "red", linetype = "dashed") +
  geom_point(
    color = ifelse(qb_pass_locations$passer_player_name == "M.Trubisky", "red", "black"),
    cex = 2.5, alpha = 0.5
  ) +
  geom_text_repel(
    aes(label = passer_player_name),
    force = 1,
    point.padding = 0.1,
    segment.size = 0.1
  ) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  labs(
    x = "EPA per pass middle or right",
    y = "EPA per pass left",
    caption = "Data from nflscrapR",
    title = "QB EPA per pass location",
    subtitle = "Through 12 weeks, min 100 pass attempts"
  ) +
  default_theme

ggsave("graphs/qb_pass_location_epa.png", dpi = 1000)
