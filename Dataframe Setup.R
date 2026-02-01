#-------------------------------------------------------------------------------
# Creating Master Umpire Dashboard Dataframe
# Neal Wood - Last Update: 1/7/2024
#-------------------------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(tidyr)

pitcher_master <- read_csv("~/Desktop/Coding Projects/Baseball R/All CSV Files/Statcast Scrapes/Unformatted/Master Pitcher Dataset")
ball_strike_master <- read_csv("~/Desktop/Coding Projects/Baseball R/All CSV Files/Statcast Scrapes/Unformatted/Ball & Strike Data Master")
unique_cols_pitcher <- setdiff(colnames(pitcher_master), colnames(ball_strike_master))

combined <- cbind(ball_strike_master, pitcher_master[, unique_cols_pitcher])
master_umpire <- combined |> filter(game_type != "S") |> add_umpires()

#-------------------------------------------------------------------------------
# Create File for Umpire Main Tab in Umpire Dashboard
#-------------------------------------------------------------------------------

umppbpall <- umppbpall %>%
  filter(balls >= 0, pitchername != "Eephus", pitchtype != "EP") %>%
  mutate(balls = ifelse(balls == 4, 3, balls)) %>%
  mutate(strikes = ifelse(strikes == 3, 2, strikes)) %>%
  mutate(pitchname = ifelse(pitchname == "Forkball", "Splitter", pitchname)) %>%
  mutate(pitchname = ifelse(pitchname %in% c("Slow Curve", "Slurve", "Knuckle Curve", "KC"), "Curveball", pitchname)) %>%
  mutate(pitchname = ifelse(pitchname == "Screwball", "Fastball", pitchname)) %>%
  mutate(pitchtype = ifelse(pitchtype == "FO", "ST", pitchtype)) %>%
  mutate(pitchtype = ifelse(pitchtype %in% c("CS", "SV", "KC"), "CU", pitchtype)) %>%
  mutate(pitchtype = ifelse(pitchtype == "SC", "FF", pitchtype))

write_csv(umppbpall, "Master Umpire 2016-2023")

umpdash <- umppbpall %>%
  filter(description %in% c("called_strike", "ball")) %>%
  select(hpumpname, balls, strikes, pitchname, bstand, battername, pitchername, pthrows, pfxx, pfxz, platex, platez, badball, badk, deltarunexp, deltahomewinexp, description)
write_csv(umpdash, "Master Umpire Dashboard")



#-------------------------------------------------------------------------------
# Create File for Umpire Evaluation Tab in Umpire Dashboard
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Zone Accuracy Rating Scores/Percentiles
#-------------------------------------------------------------------------------
umpeval <- read_csv("Master Umpire 2016-2023")

umpevalzone <- umpeval %>%
  filter(!is.na(zone), description %in% c("called_strike", "ball")) %>%
  select(bstand, pthrows, hpumpname, balls, strikes, description, 
         pitchname, pfxx, pfxz, platex, platez, deltarunexp, deltahomewinexp, badball, badk, gameToD, zone) %>%
  group_by(hpumpname, zone) %>%
  mutate(zonecall_tscb = sum(badball)) %>%
  mutate(zonecall_tbcs = sum(badk)) %>%
  mutate(totalpitchcalls = n()) %>%
  unique()

umpcallcomp <- umpevalzone %>%
  select(hpumpname, zone, zonecall_tscb, zonecall_tbcs, totalpitchcalls ) %>%
  mutate(totalmissedcall = zonecall_tbcs + zonecall_tscb) %>%
  mutate(pctmissedcall = round(100*(totalmissedcall/totalpitchcalls), digits = 3)) %>%
  select(hpumpname, zone, totalmissedcall, totalpitchcalls, pctmissedcall) %>%
  mutate(zonehl = paste0("zone", zone)) %>%
  unique()

# Use find & replace to change to different zone - manual process 
# Make sure to change the zone 
test_zone_1 <- umpcallcomp %>%
  subset(zone == 1) %>%
  mutate(pctmissedcallpctnl = pctmissedcall) %>%
  mutate(totalmissedcallpcntl = totalmissedcall) %>%
  mutate(totalpitchcallspcntl = totalpitchcalls)

test_zone_1$pctmissedcallrank[order(test_zone_1$pctmissedcallpctnl, decreasing = FALSE)] <- 1:nrow(test_zone_1) 
test_zone_1$totalmissedcallsrank[order(test_zone_1$totalmissedcallpcntl, decreasing = FALSE)] <- 1:nrow(test_zone_1) 
test_zone_1$totalpitchcallsrank[order(test_zone_1$totalpitchcallspcntl, decreasing = TRUE)] <- 1:nrow(test_zone_1) 

test_zone_1$pctmissedcallpctnl = 100*(round(1 - ((test_zone_1$pctmissedcallrank) / max(test_zone_1$pctmissedcallrank, na.rm = TRUE)), digits = 2))
test_zone_1$totalmissedcallpcntl = 100*(round(1 - ((test_zone_1$totalmissedcallsrank) / max(test_zone_1$totalmissedcallsrank, na.rm = TRUE)), digits = 2))
test_zone_1$totalpitchcallspcntl = 100*(round(1 - ((test_zone_1$totalpitchcallsrank) / max(test_zone_1$totalpitchcallsrank, na.rm = TRUE)), digits = 2))


lowall <- umpcallcomp %>% subset(select = c(hpumpname, zone)) %>% unique() %>% filter(zone == 1)

lowall$zonehl <- "Low"
lowall$pctmissedcall <- 0
lowall$pctmissedcallpctnl <- -5
lowall$pctmissedcallrank <- 20

lowall$totalmissedcall <- 0
lowall$totalmissedcallpcntl <- -5
lowall$totalmissedcallsrank <- 20

lowall$totalpitchcalls <- 0
lowall$totalpitchcallspcntl <- -5
lowall$totalpitchcallsrank <- 20



# High
highall <- umpcallcomp %>% subset(select = c(hpumpname, zone)) %>% unique() %>% filter(zone == 1)


highall$zonehl <- "High"
highall$pctmissedcall <- 100
highall$pctmissedcallpctnl <- 105
highall$pctmissedcallrank <- 1

highall$totalmissedcall <- 100
highall$totalmissedcallpcntl <- 105
highall$totalmissedcallsrank <- 1  

highall$totalpitchcalls <- 100
highall$totalpitchcallspcntl <- 105
highall$totalpitchcallsrank <- 1



# Rbind to Combine Low and High
LowHigh <- rbind(lowall, highall)

test_zone_1 <- rbind(test_zone_1, LowHigh)

# Combine all rankings and percentiles into one master df & create CSV with data for future use
masterzone <- rbind(test_zone_1, test_zone_2, test_zone_3, test_zone_4, test_zone_5, 
                    test_zone_6, test_zone_7, test_zone_8, test_zone_9, test_zone_11,
                    test_zone_12, test_zone_13, test_zone_14)
write_csv(masterzone, "Master Umpire Zone Percentiles 2016-2023")




#-------------------------------------------------------------------------------
# Zone Accuracy by Pitch Type
#-------------------------------------------------------------------------------

umpeval <- read_csv("Master Umpire 2016-2023")

umpevalzp <- umpeval %>%
  filter(!is.na(zone), description %in% c("called_strike", "ball")) %>%
  select(bstand, pthrows, hpumpname, balls, strikes, description, 
         pitchname, pfxx, pfxz, platex, platez, deltarunexp, deltahomewinexp, badball, badk, gameToD, zone) %>%
  group_by(hpumpname, zone, pitchname) %>%
  mutate(zonecall_tscb = sum(badball)) %>%
  mutate(zonecall_tbcs = sum(badk)) %>%
  mutate(totalpitchcalls = n()) %>%
  unique()




