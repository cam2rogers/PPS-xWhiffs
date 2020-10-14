# Function to retrieve player names from their savant IDs (adapted from baseballr package)

playername_lookup_adj <- function(id) {
  if (!exists("chadwick_player_lu_table")) {
    print("Be patient, this may take a few seconds...")
    print("Data courtesy of the Chadwick Bureau Register (https://github.com/chadwickbureau/register)")
    url <- "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv"
    chadwick_player_lu_table <- readr::read_csv(url)
    assign("chadwick_player_lu_table", chadwick_player_lu_table, envir = .GlobalEnv)
  }
  
  x <- chadwick_player_lu_table %>% 
    dplyr::filter(id == key_mlbam | id == key_retro | id == key_bbref | id == key_fangraphs) %>%
    dplyr::mutate(full_name = paste(name_first, name_last, sep=" ")) %>%
    dplyr::select(full_name) %>% dplyr::pull(full_name)
  x
}
final_pitches$pitcher_name <- sapply(final_pitches$pitcher, playername_lookup_adj)

# PPS function

pitch_score <- function(df) {
  df$pitch_score <- ifelse(df$Swing == 0, (1-df$swingprob)*df$csprob*rvcs, df$whiffprob*rvwhiff)
  return(df)
}

fp2 <- pitch_score(final_pitches)

# Computing average pitch scores for each pitch type/handedness combo

fp2 %>% group_by(p_throws, pitch_type) %>% summarize(mps = mean(pitch_score)) -> mean_scores
fp2 %>% left_join(mean_scores, by=c("p_throws", "pitch_type")) %>% mutate(adj_score = pitch_score - mps) -> fp2

# Mean PPS, Overall PPS, Arsenal Scores

fp2 %>%
  group_by(pitcher_name, pitch_type) %>%
  summarize(ps = mean(adj_score)*100,
            n = n()) %>%
  arrange(desc(ps)) -> pitch_scores_mean

fp2 %>%
  group_by(pitcher_name, pitch_type) %>%
  summarize(ps = sum(adj_score),
            n = n()) %>%
  arrange(desc(ps)) -> pitch_scores_sum

fp2 %>%
  group_by(pitcher_name) %>%
  summarize(as = sum(adj_score),
            n = n()) %>%
  filter(n >= 486) %>%
  arrange(desc(as)) -> arsenal_scores

# Filtering results for qualified pitchers

fp2 %>%
  group_by(pitcher_name) %>%
  summarize(total_pitches = n()) %>%
  filter(total_pitches >= 486) -> qualified_pitchers

pitch_scores_mean %>%
  filter(pitcher_name %in% qualified_pitchers$pitcher_name) %>%
  left_join(qualified_pitchers, by="pitcher_name") %>%
  mutate(usage_pct = n / total_pitches) %>%
  filter(usage_pct >= 0.05) -> pitch_scores_mean

pitch_scores_sum %>%
  filter(pitcher_name %in% qualified_pitchers$pitcher_name) %>%
  left_join(qualified_pitchers, by="pitcher_name") %>%
  mutate(usage_pct = n / total_pitches) %>%
  filter(usage_pct >= 0.05) -> pitch_scores_sum
  
# xWhiff Rates (by pitch and overall)

fp2 %>%
  group_by(pitcher_name, pitch_type) %>%
  filter(Swing == 1) %>%
  summarize(xWhiff = sum(whiffprob)/n(),
            Whiff = sum(Miss == 1)/n(),
            diff = Whiff - xWhiff,
            swings = n()) %>%
  left_join(pitch_scores_mean, by=c("pitcher_name", "pitch_type")) %>%
  filter(pitcher_name %in% qualified_pitchers$pitcher_name, usage_pct >= 0.05) %>%
  select(-ps) %>%
  filter(swings >= 40) %>%
  arrange(desc(xWhiff)) -> xWhiffs_pitches

fp2 %>%
  group_by(pitcher_name) %>%
  filter(Swing == 1) %>%
  summarize(xWhiff = sum(whiffprob)/n(),
            Whiff = sum(Miss == 1)/n(),
            diff = Whiff - xWhiff,
            swings = n()) %>%
  left_join(arsenal_scores, by="pitcher_name") %>%
  filter(pitcher_name %in% qualified_pitchers$pitcher_name) %>%
  select(-as) %>%
  filter(swings >= 40) %>%
  arrange(desc(xWhiff)) -> xWhiff_total
  
  
#### Repeating same process for 2018 stats
  
pitches_2018$pitcher_name <- sapply(pitches_2018$pitcher, playername_lookup_adj)

pitches_2018_2 <- pitch_score2(pitches_2018)
pitches_2018_2 %>% group_by(p_throws, pitch_type) %>% summarize(mps = mean(pitch_score)) -> mean_scores_2
pitches_2018_2 %>% left_join(mean_scores_2, by=c("p_throws", "pitch_type")) %>% mutate(adj_score = pitch_score - mps) -> pitches_2018_2

pitches_2018_2 %>%
  group_by(pitcher_name) %>%
  summarize(total_pitches = n()) %>%
  filter(total_pitches >= 486) -> qualified_pitchers_2018

pitches_2018_2 %>%
  group_by(pitcher_name, pitch_type) %>%
  summarize(ps18 = mean(adj_score)*100,
            n = n()) %>%
  arrange(desc(ps18)) -> pitch_scores_mean_2018

pitches_2018_2 %>%
  group_by(pitcher_name, pitch_type) %>%
  summarize(ps18 = sum(adj_score),
            n = n()) %>%
  arrange(desc(ps18)) -> pitch_scores_sum_2018

pitch_scores_mean_2018 %>%
  filter(pitcher_name %in% qualified_pitchers_2018$pitcher_name) %>%
  left_join(qualified_pitchers_2018, by="pitcher_name") %>%
  mutate(usage_pct = n / total_pitches) %>%
  filter(usage_pct >= 0.05) -> pitch_scores_mean_2018

pitch_scores_sum_2018 %>%
  filter(pitcher_name %in% qualified_pitchers_2018$pitcher_name) %>%
  left_join(qualified_pitchers_2018, by="pitcher_name") %>%
  mutate(usage_pct = n / total_pitches) %>%
  filter(usage_pct >= 0.05) -> pitch_scores_sum_2018

pitches_2018_2 %>%
  group_by(pitcher_name) %>%
  summarize(as18 = sum(adj_score),
            n = n()) %>%
  filter(n >= 486) %>%
  arrange(desc(as18)) -> arsenal_scores_2018

pitches_2018_2 %>%
  group_by(pitcher_name, pitch_type) %>%
  filter(Swing == 1) %>%
  summarize(xWhiff18 = sum(whiffprob)/n(),
            Whiff18 = sum(Miss == 1)/n(),
            diff18 = Whiff18 - xWhiff18,
            swings18 = n()) %>%
  left_join(pitch_scores_mean_2018, by=c("pitcher_name", "pitch_type")) %>%
  filter(pitcher_name %in% qualified_pitchers_2018$pitcher_name, usage_pct >= 0.05) %>%
  select(-ps18) %>%
  filter(swings18 >= 40) %>%
  arrange(desc(xWhiff18)) -> xWhiffs_pitches_2018

pitches_2018_2 %>%
  group_by(pitcher_name) %>%
  filter(Swing == 1) %>%
  summarize(xWhiff18 = sum(whiffprob)/n(),
            Whiff18 = sum(Miss == 1)/n(),
            diff18 = Whiff18 - xWhiff18,
            swings18 = n()) %>%
  left_join(arsenal_scores_2018, by="pitcher_name") %>%
  filter(pitcher_name %in% qualified_pitchers_2018$pitcher_name) %>%
  select(-as18) %>%
  filter(swings18 >= 40) %>%
  arrange(desc(xWhiff18)) -> xWhiff_total_2018
