football_data_by_year <- football_data %>%
  mutate(date = as.Date(date, "%Y-%m-%d"),
         year = year(date),
         decade = floor(year / 10) * 10)

goal_trend <- football_data %>%
  mutate(total_goals = home_score + away_score) %>%
  group_by(year) %>%
  summarise(avg_goals = mean(total_goals, na.rm = TRUE))

ggplot(goal_trend, aes(x = year, y = avg_goals)) +
  geom_line(color = "blue") +
  labs(title = "Trend of Average Goals Scored Over Time",
       x = "Year", y = "Average Goals") +
  theme_minimal()

team_success <- football_data %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    home_score < away_score ~ away_team,
    TRUE ~ "Draw"
  )) %>%
  filter(winner != "Draw") %>%
  count(winner, name = "wins") %>%
  arrange(desc(wins))

kable(head(team_success, 10), caption = "Top 10 Most Successful Teams") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

team_by_decade <- football_data %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    home_score < away_score ~ away_team,
    TRUE ~ "Draw"
  )) %>%
  filter(winner != "Draw") %>%
  group_by(decade, winner) %>%
  summarise(wins = n(), .groups = "drop") %>%
  arrange(decade, desc(wins))

ggplot(team_by_decade, aes(x = reorder(winner, -wins), y = wins, fill = as.factor(decade))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top Teams by Decade",
       x = "Team", y = "Wins", fill = "Decade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

football_cleaneded <- football_cleaned %>%
  mutate(match_type = if_else(tolower(tournament) == "friendly", "Friendly", "Tournament"))

# Summarize the number of wins for each match type
win_insights <- football_cleaneded %>%
  filter(outcome %in% c("Home Win", "Away Win")) %>%  # Assuming there is a column `result` indicating match outcomes
  group_by(team, match_type) %>%
  summarise(total_wins = n(), .groups = "drop") %>%
  arrange(team, desc(total_wins))