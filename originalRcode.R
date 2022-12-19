library(haven)
library(tidyverse)
library(knitr)
library(ggplot2)
library(factoextra)
library(dplyr)

library(nflreadr)
library(nflfastR)

library(ClusterR)
library(cluster)

# getting play-by-play data from 2021 and 2022 (so far)
raw <- rbind(load_pbp(2021), load_pbp(2022))
raw$posteam_mov <- raw$posteam_score - raw$defteam_score

# selecting important variables
sel <- c('passer', 'passer_id', 'receiver', 'receiver_id', 'posteam', 'complete_pass', 'incomplete_pass', 
         'interception', 'fumble', 'sack', 'qb_scramble', 'pass_length',
         'pass_location', 'yards_gained','air_yards', 'yards_after_catch', 'shotgun', 
         'no_huddle', 'home_team','away_team','posteam_type','yardline_100',
         'posteam_score', 'defteam_score', 'posteam_mov', 'qtr', 'half_seconds_remaining', 
         'game_seconds_remaining','down','ydstogo','posteam_timeouts_remaining',
         'defteam_timeouts_remaining', 'qb_dropback')

# filtering for all QB dropbacks
dropback <- raw[,..sel] %>% filter(qb_dropback == 1)
rm(raw)
dropback$outside_deep <- ifelse(dropback$pass_length == "deep" & 
                                  (dropback$pass_location == "right" | dropback$pass_location == "left"), 1, 0)
dropback$outside_short <- ifelse(dropback$pass_length == "short" & 
                                  (dropback$pass_location == "right" | dropback$pass_location == "left"), 1, 0)
dropback$middle_deep <- ifelse(dropback$pass_length == "deep" & dropback$pass_location == "middle", 1, 0)
dropback$middle_short <- ifelse(dropback$pass_length == "short" & dropback$pass_location == "middle", 1, 0)


# checking for min pass attempts/receptions
pass_attempts <- aggregate(qb_dropback ~ passer + passer_id, data = dropback, FUN=length) %>% filter(qb_dropback >= 100)
receptions <- aggregate(qb_dropback ~ receiver + receiver_id, data = dropback, FUN=length) %>% filter(qb_dropback >= 50)

# calculate stats for players if they pass the min threshold
total_qb_stats <- merge(pass_attempts,
                      aggregate(cbind(complete_pass, incomplete_pass, shotgun, 
                                      air_yards, yards_gained) 
                                ~ passer + passer_id, data = dropback, FUN=sum, 
                                na.rm=TRUE))
total_rec_stats <- merge(receptions, aggregate(cbind(yards_after_catch, yards_gained, 
                                                     outside_deep, outside_short, 
                                                     middle_deep, middle_short) 
                                 ~ receiver + receiver_id, data = dropback, FUN=sum, 
                                 na.rm=TRUE))

avg_qb_stats <- merge(pass_attempts,
                        aggregate(cbind(complete_pass, incomplete_pass, shotgun, air_yards, yards_gained) 
                                  ~ passer + passer_id, data = dropback, FUN=mean, na.rm=TRUE))
avg_rec_stats <- merge(receptions, 
                         aggregate(cbind(yards_after_catch, yards_gained, outside_deep, 
                                         outside_short, middle_deep, middle_short) 
                                   ~ receiver + receiver_id, data = dropback, FUN=mean, na.rm=TRUE))
rm(pass_attempts); rm(receptions)

avg_rec_stats <- avg_rec_stats[order(-avg_rec_stats$qb_dropback),] %>% mutate_if(is.numeric, ~round(., 3))
total_rec_stats <- total_rec_stats[order(-total_rec_stats$qb_dropback),] %>% mutate_if(is.numeric, ~round(., 3))

# nifty trick, the only players that have average YAC > average yards gained are running backs (and Deebo)
rbs <- avg_rec_stats %>% filter(yards_after_catch > yards_gained)
wr_te <- avg_rec_stats %>% filter(avg_rec_stats$yards_after_catch <= avg_rec_stats$yards_gained)

#qb_vars
rec_vars <- c('yards_after_catch', 'yards_gained',  'outside_deep', 'outside_short', 
              'middle_deep', 'middle_short')

scaled_rec <- data.frame(scale(avg_rec_stats[,rec_vars]))
scaled_rec <- apply(scaled_rec, 2, function(x) as.numeric(x))
#scaled_rec <- round(scaled_rec, 3)

# finding optimal number of clusters
fviz_nbclust(scaled_rec, kmeans, method = "wss", k.max = 8)

#k4 <- kmeans(scaled_rec, centers = 4, nstart = 25)
#fviz_cluster(k4, data=scaled_rec)

k3 <- kmeans(scaled_rec, centers=3, nstart = 25)
fviz_cluster(k3, data=scaled_rec, main="Classification of NFL Receivers into 3 Clusters")

#scaled_rec <- cbind(total_rec_stats$receiver, scaled_rec, k4$cluster)
total_rec_stats <- cbind(total_rec_stats, k3$cluster)
avg_rec_stats <- cbind(avg_rec_stats, k3$cluster)

total_rec_stats[which(total_rec_stats$receiver == 'C.McCaffrey'),ncol(total_rec_stats)]
total_rec_stats[which(total_rec_stats$receiver == 'C.Kupp'),ncol(total_rec_stats)]
total_rec_stats[which(total_rec_stats$receiver == 'T.Kelce'),ncol(total_rec_stats)]

cat1 <- aggregate(cbind(yards_after_catch, yards_gained) ~ k3$cluster, 
                              data = avg_rec_stats, FUN=mean)
categories <- round(merge(cat1, aggregate(cbind(outside_deep, outside_short, 
                                                middle_deep, middle_short) 
                                          ~ k3$cluster, data = total_rec_stats, FUN=sum)),2)

categories[nrow(categories)+1,] <- NA
categories$total <- NA
for (j in 2:ncol(categories)) {
  categories[nrow(categories),j] <- sum(categories[1:nrow(categories)-1,j])
}
for (i in 1:nrow(categories)) {
  categories$total[i] <- sum(categories[i,4:7]) # shouldn't really be hardcoded
}

categories[,4:7] <- round((categories[,4:7] / categories$total),3)

write.csv(categories, 'C:/Users/drewm/Desktop/wr_categories.csv', row.names = FALSE)


dropback <- inner_join(dropback, avg_rec_stats[,c('receiver_id', 'k3$cluster')], by='receiver_id')

# just dropbacks during two minute drills in one-score games
two_min <- dropback %>% filter((half_seconds_remaining <= 120 & qtr == 2) |
                               (half_seconds_remaining <= 120 & qtr == 4 & 
                                  posteam_mov <= 0 & posteam_mov >= -8))

twomin_tends <- summarize(two_min, 
                          yards_after_catch = mean(yards_after_catch, na.rm=TRUE),
                          yards_gained = mean(yards_gained, na.rm=TRUE),
                          outside_deep = mean(outside_deep, na.rm=TRUE),
                          outside_short = mean(outside_short, na.rm=TRUE),
                          middle_deep = mean(middle_deep, na.rm=TRUE),
                          middle_short = mean(middle_short, na.rm=TRUE),
                          cluster1 = (length(which(`k3$cluster` == 1))) / length(`k3$cluster`),
                          cluster2 = (length(which(`k3$cluster` == 2))) / length(`k3$cluster`),
                          cluster3 = (length(which(`k3$cluster` == 3))) / length(`k3$cluster`))

all_tends <- round(rbind(twomin_tends, 
                         summarize(dropback, 
                                   yards_after_catch = mean(yards_after_catch, na.rm=TRUE),
                                   yards_gained = mean(yards_gained, na.rm=TRUE),
                                   outside_deep = mean(outside_deep, na.rm=TRUE),
                                   outside_short = mean(outside_short, na.rm=TRUE),
                                   middle_deep = mean(middle_deep, na.rm=TRUE),
                                   middle_short = mean(middle_short, na.rm=TRUE),
                                   cluster1 = (length(which(`k3$cluster` == 1))) / length(`k3$cluster`),
                                   cluster2 = (length(which(`k3$cluster` == 2))) / length(`k3$cluster`),
                                   cluster3 = (length(which(`k3$cluster` == 3))) / length(`k3$cluster`))), 3)

rownames(all_tends) <- c('Two Minute Drills', 'All Plays')
