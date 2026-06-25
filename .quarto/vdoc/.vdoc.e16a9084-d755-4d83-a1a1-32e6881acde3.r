#
#
#
#
#
#
#
#
#
library(tidyverse)

games <- read_csv("Games.csv", show_col_types = FALSE) %>%
  mutate(gameDateTimeEst = lubridate::ymd_hms(gameDateTimeEst),
         year = lubridate::year(gameDateTimeEst))

team_code <- function(city, team) {
  case_when(
    city == "Atlanta" & team == "Hawks" ~ "ATL",
    city == "Boston" & team == "Celtics" ~ "BOS",
    city == "Brooklyn" & team == "Nets" ~ "BKN",
    city == "Charlotte" & team %in% c("Hornets", "Bobcats") ~ "CHA",
    city == "Chicago" & team == "Bulls" ~ "CHI",
    city == "Cleveland" & team == "Cavaliers" ~ "CLE",
    city == "Dallas" & team == "Mavericks" ~ "DAL",
    city == "Denver" & team == "Nuggets" ~ "DEN",
    city == "Detroit" & team == "Pistons" ~ "DET",
    city == "Golden State" & team == "Warriors" ~ "GSW",
    city == "Houston" & team == "Rockets" ~ "HOU",
    city == "Indiana" & team == "Pacers" ~ "IND",
    city == "Kansas City" & team == "Kings" ~ "KCK",
    city == "LA" & team == "Clippers" ~ "LAC",
    city == "Los Angeles" & team == "Clippers" ~ "LAC",
    city == "Los Angeles" & team == "Lakers" ~ "LAL",
    city == "Miami" & team == "Heat" ~ "MIA",
    city == "Milwaukee" & team == "Bucks" ~ "MIL",
    city == "Minnesota" & team == "Timberwolves" ~ "MIN",
    city == "New Orleans" & team == "Pelicans" ~ "NOP",
    city == "New Orleans" & team == "Hornets" ~ "NOH",
    city == "New York" & team == "Knicks" ~ "NYK",
    city == "New York" & team == "Nets" ~ "BKN",
    city == "Oklahoma City" & team == "Thunder" ~ "OKC",
    city == "Orlando" & team == "Magic" ~ "ORL",
    city == "Philadelphia" & team == "76ers" ~ "PHI",
    city == "Phoenix" & team == "Suns" ~ "PHX",
    city == "Portland" & team == "Trail Blazers" ~ "POR",
    city == "Sacramento" & team == "Kings" ~ "SAC",
    city == "San Antonio" & team == "Spurs" ~ "SAS",
    city == "San Diego" & team == "Clippers" ~ "SDC",
    city == "San Diego" & team == "Rockets" ~ "SDR",
    city == "San Francisco" & team == "Warriors" ~ "SFW",
    city == "Seattle" & team == "SuperSonics" ~ "SEA",
    city == "Toronto" & team == "Raptors" ~ "TOR",
    city == "Utah" & team == "Jazz" ~ "UTA",
    city == "Vancouver" & team == "Grizzlies" ~ "VAN",
    city == "Washington" & team %in% c("Bullets", "Wizards") ~ "WSH",
    TRUE ~ str_to_upper(str_replace_all(str_sub(coalesce(city, team), 1, 3), "[^A-Z]", ""))
  )
}

unique_scores_df <- games %>%
  mutate(
    home_code = team_code(hometeamCity, hometeamName),
    away_code = team_code(awayteamCity, awayteamName),
    label = paste0(home_code, " - ", away_code),
    total_points = homeScore + awayScore
  ) %>%
  arrange(gameDateTimeEst) %>%
  group_by(homeScore, awayScore) %>%
  slice_head(n = 1) %>%
  ungroup()

score_limits <- unique_scores_df %>%
  summarize(
    home_low = quantile(homeScore, 0.08),
    home_high = quantile(homeScore, 0.92),
    away_low = quantile(awayScore, 0.08),
    away_high = quantile(awayScore, 0.92),
    total_low = quantile(total_points, 0.08),
    total_high = quantile(total_points, 0.92)
  )

unique_scores_df <- unique_scores_df %>%
  mutate(
    label = if_else(
      homeScore <= score_limits$home_low |
      homeScore >= score_limits$home_high |
      awayScore <= score_limits$away_low |
      awayScore >= score_limits$away_high |
      total_points <= score_limits$total_low |
      total_points >= score_limits$total_high,
      label,
      NA_character_
    )
  )

unique_score_count <- nrow(unique_scores_df)

years_covered <- games %>%
  filter(!is.na(year)) %>%
  summarize(min_year = min(year), max_year = max(year)) %>%
  mutate(label = if_else(min_year == max_year, as.character(min_year), paste0(min_year, "–", max_year))) %>%
  pull(label)

years_covered <- if_else(is.na(years_covered), "unknown years", years_covered)

max_score <- max(unique_scores_df$homeScore, unique_scores_df$awayScore)

plot <- ggplot(unique_scores_df, aes(x = awayScore, y = homeScore, color = total_points)) +
  geom_point(size = 4.5, alpha = 0.85) +
  geom_text(aes(label = label), size = 2.8, color = "black", check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(0, max_score + 50, by = 50),
                     minor_breaks = seq(0, max_score + 10, by = 10),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_y_continuous(breaks = seq(0, max_score + 50, by = 50),
                     minor_breaks = seq(0, max_score + 10, by = 10),
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_color_gradientn(colors = c("#0B1738", "#3F1F78", "#7A1D9A", "#D64B70", "#F4A71F", "#FAF3D6"),
                        name = "Total Pts") +
  coord_fixed(ratio = 0.75) +
  labs(
    title = "NBA Scorigami",
    subtitle = paste0(unique_score_count, " unique final scores from Games.csv (", years_covered, ")"),
    x = "Away PTS",
    y = "Home PTS",
    caption = "Each point is a unique NBA final score observed in the dataset."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 26),
    plot.subtitle = element_text(size = 16),
    plot.caption = element_text(size = 12)
  ) +
  annotate("text",
           x = max_score * 0.6,
           y = max_score * 0.12,
           label = paste0(unique_score_count, " unique scores\n", years_covered),
           hjust = 0,
           size = 5,
           color = "grey20")

plot
#
#
#
#
#
