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

