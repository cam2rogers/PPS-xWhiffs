# Run values of outcomes

sctot %>%
  filter(game_year == 2019) -> sc19
  
## RUN VALUES ##

sc19 <- run_expectancy_code(sc19, level="pitch")

sc19 %>%
  filter(description == "called_strike") %>%
  summarize(runValue = mean(re24)) -> rvcs

sc19 %>%
  filter(description == "swinging_strike" | description == "swinging_strike_blocked" | description == "swinging_strike_pitchout") %>%
  summarize(re = mean(re24)) -> rvwhiff
