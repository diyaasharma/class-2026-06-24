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

team_scores <- bind_rows(
  games %>% transmute(team = team_code(hometeamCity, hometeamName), points = homeScore),
  games %>% transmute(team = team_code(awayteamCity, awayteamName), points = awayScore)
)

top_teams <- team_scores %>%
  group_by(team) %>%
  summarize(
    games = n(),
    total_points = sum(points),
    avg_points = total_points / games,
    .groups = "drop"
  ) %>%
  arrange(desc(total_points), desc(avg_points)) %>%
  slice_head(n = 10)

knitr::kable(
  top_teams %>% mutate(avg_points = round(avg_points, 1)),
  caption = "Top 10 highest scoring teams by total points in Games.csv",
  align = c("l", "r", "r", "r")
)

unique_scores_df <- games %>%
  mutate(
    home_code = team_code(hometeamCity, hometeamName),
    away_code = team_code(awayteamCity, awayteamName),
    label = paste0(home_code, " - ", away_code),
    total_points = homeScore + awayScore,
    home_visitor = if_else(homeScore > awayScore, "Home", "Visitor"),
    win_loss = if_else(homeScore > awayScore, "Win", "Lose")
  ) %>%
  arrange(gameDateTimeEst) %>%
  group_by(homeScore, awayScore) %>%
  summarize(
    home_code = first(home_code),
    away_code = first(away_code),
    label = first(label),
    total_points = first(total_points),
    home_visitor = first(home_visitor),
    win_loss = first(win_loss),
    game_count = n(),
    .groups = "drop"
  )

extreme_rows <- unique_scores_df %>%
  summarize(
    min_id = which.min(total_points),
    max_id = which.max(total_points)
  )

unique_scores_df <- unique_scores_df %>%
  mutate(
    text_label = if_else(
      row_number() %in% c(extreme_rows$min_id, extreme_rows$max_id),
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

game_counts <- unique_scores_df$game_count
max_games <- max(game_counts)

plot <- plot_ly() %>%
  add_trace(
    data = filter(unique_scores_df, home_visitor == "Home"),
    x = ~awayScore,
    y = ~homeScore,
    color = ~game_count,
    colorscale = list(
      c(0, "#0B1738"),
      c(0.25, "#3F1F78"),
      c(0.5, "#7A1D9A"),
      c(0.75, "#D64B70"),
      c(1, "#F4A71F")
    ),
    colorbar = list(title = "Games"),
    type = "scatter",
    mode = "markers+text",
    name = "Home",
    text = ~text_label,
    hovertext = ~label,
    textposition = "top center",
    marker = list(size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{hovertext}<br>", "Games: %{marker.color}<extra></extra>"),
    visible = TRUE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, home_visitor == "Visitor"),
    x = ~awayScore,
    y = ~homeScore,
    color = ~game_count,
    colorscale = list(
      c(0, "#0B1738"),
      c(0.25, "#3F1F78"),
      c(0.5, "#7A1D9A"),
      c(0.75, "#D64B70"),
      c(1, "#F4A71F")
    ),
    showscale = FALSE,
    type = "scatter",
    mode = "markers+text",
    name = "Visitor",
    text = ~text_label,
    hovertext = ~label,
    textposition = "top center",
    marker = list(size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{hovertext}<br>", "Games: %{marker.color}<extra></extra>"),
    visible = TRUE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, win_loss == "Win"),
    x = ~awayScore,
    y = ~homeScore,
    color = ~game_count,
    colorscale = list(
      c(0, "#0B1738"),
      c(0.25, "#3F1F78"),
      c(0.5, "#7A1D9A"),
      c(0.75, "#D64B70"),
      c(1, "#F4A71F")
    ),
    showscale = FALSE,
    type = "scatter",
    mode = "markers+text",
    name = "Win",
    text = ~text_label,
    hovertext = ~label,
    textposition = "top center",
    marker = list(size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{hovertext}<br>", "Games: %{marker.color}<extra></extra>"),
    visible = FALSE
  ) %>%
  add_trace(
    data = filter(unique_scores_df, win_loss == "Lose"),
    x = ~awayScore,
    y = ~homeScore,
    color = ~game_count,
    colorscale = list(
      c(0, "#0B1738"),
      c(0.25, "#3F1F78"),
      c(0.5, "#7A1D9A"),
      c(0.75, "#D64B70"),
      c(1, "#F4A71F")
    ),
    showscale = FALSE,
    type = "scatter",
    mode = "markers+text",
    name = "Lose",
    text = ~text_label,
    hovertext = ~label,
    textposition = "top center",
    marker = list(size = 10, opacity = 0.85, line = list(width = 0.5, color = "#FFFFFF")),
    hovertemplate = paste("Score: %{y} - %{x}<br>", "Teams: %{hovertext}<br>", "Games: %{marker.color}<extra></extra>"),
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
    legend = list(orientation = "h", x = 0.1, y = 1.05),
    margin = list(t = 140, b = 80, l = 80, r = 40),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.05,
        y = 1.12,
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
