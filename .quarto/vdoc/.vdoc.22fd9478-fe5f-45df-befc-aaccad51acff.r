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
library(plotly)

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
    total_points = homeScore + awayScore,
    home_visitor = "Home",
    win_loss = if_else(homeScore > awayScore, "Win", "Lose")
  ) %>%
  arrange(gameDateTimeEst) %>%
  group_by(homeScore, awayScore) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(home_visitor = if_else(homeScore > awayScore, "Home", "Visitor"))

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
    text_label = if_else(
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

home_visitor_colors <- c(Home = "#0A2240", Visitor = "#CE1141")
win_loss_colors <- c(Win = "#0A2240", Lose = "#D64B70")

plot <- plot_ly() %>%
  add_trace(
    data = filter(unique_scores_df, home_visitor == "Home"),
    x = ~awayScore,
    y = ~homeScore,
    type = "scatter",
    mode = "markers+text",
    name = "Home",
    text = ~text_label,
    textposition = "top center",
    marker = list(color = home_visitor_colors["Home"], size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{text}<extra></extra>"),
    visible = TRUE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, home_visitor == "Visitor"),
    x = ~awayScore,
    y = ~homeScore,
    type = "scatter",
    mode = "markers+text",
    name = "Visitor",
    text = ~text_label,
    textposition = "top center",
    marker = list(color = home_visitor_colors["Visitor"], size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{text}<extra></extra>"),
    visible = TRUE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, win_loss == "Win"),
    x = ~awayScore,
    y = ~homeScore,
    type = "scatter",
    mode = "markers+text",
    name = "Win",
    text = ~text_label,
    textposition = "top center",
    marker = list(color = win_loss_colors["Win"], size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{text}<extra></extra>"),
    visible = FALSE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, win_loss == "Lose"),
    x = ~awayScore,
    y = ~homeScore,
    type = "scatter",
    mode = "markers+text",
    name = "Lose",
    text = ~text_label,
    textposition = "top center",
    marker = list(color = win_loss_colors["Lose"], size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{text}<extra></extra>"),
    visible = FALSE
  ) %>%
  layout(
    title = list(text = paste0("NBA Scorigami<br><sup>", unique_score_count, " unique final scores from Games.csv (", years_covered, ")</sup>"), xref = "paper"),
    xaxis = list(
      title = "Visitor PTS",
      tickmode = "linear",
      dtick = 50,
      minor = list(dtick = 2, showgrid = TRUE, ticklen = 4),
      range = c(0, max_score + 5)
    ),
    yaxis = list(
      title = "Home PTS",
      tickmode = "linear",
      dtick = 50,
      minor = list(dtick = 2, showgrid = TRUE, ticklen = 4),
      range = c(0, max_score + 5),
      scaleanchor = "x",
      scaleratio = 1
    ),
    legend = list(orientation = "h", x = 0.1, y = 1.15),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.05,
        y = 1.18,
        buttons = list(
          list(method = "update",
               args = list(list(visible = c(TRUE, TRUE, FALSE, FALSE)),
                           list(title = paste0("NBA Scorigami<br><sup>", unique_score_count, " unique final scores from Games.csv (", years_covered, ")</sup>"))),
               label = "Home/Visitor"),
          list(method = "update",
               args = list(list(visible = c(FALSE, FALSE, TRUE, TRUE)),
                           list(title = paste0("NBA Scorigami<br><sup>", unique_score_count, " unique final scores from Games.csv (", years_covered, ")</sup>"))),
               label = "Win/Lose")
        )
      )
    ),
    annotations = list(
      list(
        x = 0.02,
        y = -0.18,
        text = paste0(unique_score_count, " unique scores<br>", years_covered),
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left",
        font = list(size = 14, color = "grey20")
      )
    )
  )

plot
#
#
#
#
#
