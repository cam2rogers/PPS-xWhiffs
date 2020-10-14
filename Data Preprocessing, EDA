# Data

Statcast2015 <- read_csv("Downloads/Statcast2015.csv")
Statcast2016 <- read_csv("Downloads/Statcast2016.csv")
Statcast2017 <- read_csv("Downloads/Statcast2017.csv")
Statcast2018 <- read_csv("Downloads/Statcast2018.csv")
Statcast2019 <- read_csv("Downloads/Statcast2019.csv")

sctot <- rbind(Statcast2015, Statcast2016, Statcast2017, Statcast2018, Statcast2019)

## EDA - pitches

sctot <- sctot %>%
  mutate(Swing = as.factor(ifelse(description %in%
                          c("foul", "foul_bunt", "foul_pitchout",
                            "foul_top", "hit_into_play",
                            "hit_into_play_no_out",
                            "hit_into_play_score", "missed_bunt",
                            "swinging_pitchout",
                            "swinging_strike",
                            "swinging_strike_blocked", "foul_tip"), 1, 0)),
         Miss = as.factor(ifelse(description %in%
                         c("swinging_pitchout",
                           "swinging_strike",
                           "swinging_strike_blocked", "foul_tip"), 1, 0)),
         Count = as.factor(paste(balls, strikes, 
                           sep="-")),
         calledStrike = as.factor(ifelse(description == "called_strike", 1, 0)))

sctot <- sctot %>%
  mutate(goodContact = ifelse(launch_speed_angle %in% c(5, 6), 1, 0))
sctot$goodContact <- as.factor(sctot$goodContact)
sctot <- sctot %>%
  mutate(Str = ifelse(description %in% c("swinging_pitchout",
                           "swinging_strike",
                           "swinging_strike_blocked", "foul_tip", "foul", "foul_top", "foul_pitchout", "foul_bunt"), 1, 0))
sctot$Str <- as.factor(sctot$Str)

sctot$barrel <- ifelse(sctot$launch_speed_angle == 6, 1, 0)

## Creating regressed statistics

# Regressed xwOBAcon

sctot %>%
  filter(type == "X") %>%
  group_by(batter, game_year) %>%
  mutate(PA = 1) %>%
  summarize(bat_bbe = sum(PA),
            bat_xwc = mean(estimated_woba_using_speedangle, na.rm = TRUE)) -> bat_xwc
avg_xwc <- sctot %>% filter(type == "X") %>% group_by(game_year) %>% mutate(PA = 1) %>% 
  summarize(tot_bat_bbe = sum(PA),
            avg_xwc = mean(estimated_woba_using_speedangle, na.rm=T))

bat_xwc %>%
  group_by(game_year) %>%
  summarize(sd_xwc = sd(bat_xwc)) %>%
  left_join(avg_xwc, by="game_year") %>%
  mutate(o_uncertain = sqrt(sd_xwc^2 - (0.5/sqrt(tot_bat_bbe))^2)) -> reg_details
bat_xwc %>%
  left_join(reg_details, by="game_year") %>%
  mutate(reg_xwc = (avg_xwc/o_uncertain^2 + bat_xwc/(0.5/sqrt(bat_bbe))^2)/(1/o_uncertain^2 + 1/(0.5/sqrt(bat_bbe))^2)) -> reg_xwc

sctot %>%
  left_join(reg_xwc, by=c("batter", "game_year")) -> sctot

# Regressed Whiff rate

sctot %>%
  filter(Swing == 1) %>%
  group_by(batter, game_year) %>%
  mutate(pitch = 1) %>%
  summarize(Nsw = sum(pitch),
            bat_whiff = sum(Miss==1)/Nsw) -> bat_whiff
avg_whiff <- sctot %>% filter(Swing == 1) %>% group_by(game_year) %>% mutate(pitch=1) %>% summarize(totsw = sum(pitch),
                                                                                                    avg_whiff = sum(Miss==1)/totsw)
bat_whiff %>%
  group_by(game_year) %>%
  summarize(sd_whiff = sd(bat_whiff)) %>%
  left_join(avg_whiff, by="game_year") %>%
  mutate(o_uncertain = sqrt(sd_whiff^2 - (0.5/sqrt(totsw))^2)) -> whiffreg_details
bat_whiff %>%
  left_join(whiffreg_details, by="game_year") %>%
  mutate(reg_whiff = (avg_whiff/(o_uncertain^2) + bat_whiff/((0.5/sqrt(Nsw))^2))/(1/(o_uncertain^2) + 1/((0.5/sqrt(Nsw))^2))) -> reg_whiff
sctot %>%
  left_join(reg_whiff, by=c("batter", "game_year")) -> sctot

# Regressed average launch angle

sctot %>%
  filter(type == "X") %>%
  group_by(pitcher, game_year, pitch_type) %>%
  mutate(PA = 1) %>%
  summarize(pit_bbe = sum(PA, na.rm=T),
            avgLA = mean(launch_angle, na.rm = TRUE)) -> las
avg_la <- sctot %>% filter(type == "X") %>% group_by(game_year, pitch_type) %>% mutate(PA = 1) %>% summarize(tot_pit_bbe = sum(PA, na.rm=T),
                                                                                     lg_LA = mean(launch_angle, na.rm=T))

las %>%
  group_by(game_year, pitch_type) %>%
  summarize(sd_la = sd(avgLA, na.rm=T)) %>%
  left_join(avg_la, by=c("game_year", "pitch_type")) %>%
  mutate(o_uncertain = sqrt(sd_la^2 - (lg_LA/sqrt(tot_pit_bbe))^2)) -> lareg_details
las %>%
  left_join(lareg_details, by=c("game_year", "pitch_type")) %>%
  mutate(reg_la = (lg_LA/o_uncertain^2 + avgLA/((avgLA/sqrt(pit_bbe))^2))/(1/o_uncertain^2 + 1/((avgLA/sqrt(pit_bbe))^2))) -> reg_la
reg_la %>% select(-o_uncertain) -> reg_la
sctot %>%
  left_join(reg_la, by=c("pitcher", "game_year", "pitch_type")) -> sctot

# Average fastball velocities

sctot %>%
  filter(pitch_type %in% fb) %>%
  group_by(pitcher, game_year) %>%
  summarize(fb_velo = mean(release_speed, na.rm=T)) ->fb_velos
sctot %>%
  left_join(fb_velos, by=c("pitcher", "game_year")) %>%
  mutate(diff_fb = release_speed - fb_velo) -> sctot

## EDA - pitches

library(tidyverse)

fb <- c("FF", "FT", "FA", "SI")
slct <- c("FC", "SL")
cu <- c("CU", "KC", "EP")
ch <- c("CH", "FS", "SC")

sctot %>%
  select(release_speed, release_pos_x, release_pos_z, release_spin_rate, release_pos_y, effective_speed, pfx_x, pfx_z) %>%
  na.omit(.) %>%
  cor(.)
