## Descriptive power

# Grabbing pitching stats from Fangraphs

pit_stats <- fg_pitch_leaders(x=2019, y=2019, league="all", qual=1, pitcher_type = "pit", ind = 0)
colnames(pit_stats)[4] <- "pitcher_name"

# Joining data to Arsenal Score data

arsenal_scores %>%
  left_join(pit_stats, by="pitcher_name") -> arsenalplusstats

# Computing R-squared values

summary(lm(ERA ~ as, data=arsenalplusstats, na.action=na.pass))
summary(lm(FIP ~ as, data=arsenalplusstats, na.action=na.pass))
summary(lm(xFIP ~ as, data=arsenalplusstats, na.action=na.pass))
summary(lm(SIERA ~ as, data=arsenalplusstats, na.action=na.pass))
summary(lm(kwERA ~ as, data=arsenalplusstats, na.action=na.pass))
summary(lm(WAR ~ as, data=arsenalplusstats, na.action=na.pass))

# Joining data to xWhiff data

xWhiff_total %>%
  left_join(pit_stats, by="pitcher_name") -> xwplusstats

# Computing R-squared values
  
summary(lm(Whiff ~ xWhiff, data=xwplusstats, na.action=na.pass))
summary(lm(K_9 ~ xWhiff, data=xwplusstats, na.action=na.pass))
summary(lm(SIERA ~ xWhiff, data=xwplusstats, na.action=na.pass))
summary(lm(kwERA ~ xWhiff, data=xwplusstats, na.action=na.pass))

## Predictive power

# Arsenal Scores (R-squared)

arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(as ~ as18, data=.) %>%
  summary(.)

arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(ERA ~ as18, data=.) %>%
  summary(.)

arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(FIP ~ as18, data=.) %>%
  summary(.)

arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(xFIP ~ as18, data=.) %>%
  summary(.)

arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(SIERA ~ as18, data=.) %>%
  summary(.)
  
arsenal_scores_2018 %>%
  inner_join(arsenalplusstats3, by="pitcher_name") %>%
  lm(kwERA ~ as18, data=.) %>%
  summary(.)

# All pitches xWhiff rate (R-squared)

xWhiff_total_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(xWhiff ~ xWhiff18, data=.) %>%
  summary(.)
  
xWhiff_total_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(Whiff ~ xWhiff18, data=.) %>%
  summary(.)

xWhiff_total_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(K_9 ~ xWhiff18, data=.) %>%
  summary(.)

xWhiff_total_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(SIERA ~ xWhiff18, data=.) %>%
  summary(.)

xWhiff_total_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(kwERA ~ xWhiff18, data=.) %>%
  summary(.)

# Individual pitches xWhiff rate (R-squared)

xWhiffs_pitches_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(xWhiff ~ xWhiff18, data=.) %>%
  summary(.)
  
xWhiffs_pitches_2018 %>%
  inner_join(xwplusstats3, by="pitcher_name") %>%
  lm(Whiff ~ xWhiff18, data=.) %>%
  summary(.)
