library(tidyverse)

#2019-20 Season 
szn19<- readxl::read_xlsx("/Users/calburke/Downloads/szn19.xlsx")

#Correlation between a team's shot per game and points earned 
cor(szn19$P, szn19$`Shots/GP`)
# .44 ... in a sport with so many factors contributing to winning that is high
# Shot against now
cor(szn19$P, szn19$`SA/GP`)
# -.43 
# Let's visualize it 
plot(szn19$`Shots/GP`, szn19$P, xlab = 'Shots per Game', ylab = 'Points', 
     abline(lm(szn19$P~szn19$`Shots/GP`), col="red") #regression line
    )
# regression line 
lm(szn19$P~ szn19$`Shots/GP`)
# Slope = 3.425 ... for every 3.425 extra shots per game you can expect 1 extra point

# if a team aims for 40 shots per game 
3.425*40-29.7
# Theroetically if you average 40 shots per game you could expect 107 points 
# (Obvisous cosniderations to take in to account such as shot quality, shooter scoring percentage etc
# but the underlying point is there.)
# 
# Lets take a look at players now
# top 100 nhl goal scorers for the 2019-20 season 
# t(top)
# c(100)
# g(goalscorers)
# 19(2019 season)
tcg19<- readxl::read_xlsx("/Users/calburke/Downloads/tcg19.xlsx")

# correlation between shots and goals 
cor(tcg19$G, tcg19$S)
#.67 even higher than team success hmmmm

# visualize
plot(tcg19$S, tcg19$G, xlab = 'Shots', ylab = 'Goals',
     abline(lm(tcg19$G~tcg19$S), col="red") # regression line
     )
# regression 
lm(tcg19$G ~ tcg19$S)
# Slope = .1 (excuse my rounding) ... for every 10 shots you can expect 1 goal (same considerations as before)
# If a player aimed for high end of 300 shots in this season 
.1*300+7.75
# if you get 300 shots on goal or
300/70 #rough amount of games played during this season (covid shortage)
# you get a seemingly reasonable goal of 4.3 shots per game (as a player I very much know this is much 
# easier said than done however, it certainly is achievable) you can expect roughly 
# 38 goals a year!!!! 
# How many players had 38 goals this year? 

tcg19 %>% 
  filter(G>37) %>% 
  count()
# 7 that puts you in the top 7 scorers in the NHL....
# 
# Ok fine maybe these guys are elite scorers lets correct for their above average shooting percentage...
# if it is above average 

mean(tcg19$`S%`)
# 14.5 average shooting percentage for top 100 goal scorers
# adjust goal totals to what they'd be if each player had an average shooting percentage
adjusted_tcg19<- tcg19 %>% 
  mutate(AG= (G/(`S%`/14.5)))

# now let's redo the above analysis

lm(adjusted_tcg19$AG ~ adjusted_tcg19$S)

300*.15-.044
# oops now I just scored 45 goals... seems we're dragging more scorers up than bringing the best down
# 
# Fine let's incorporate those outside the top 100 goal scorers
# league-wide save percentage is .910 which means scoring percentage is .09 
300*.09 
# 27 goals! not bad 
# how many people scored 27 goals last season 
tcg19 %>% 
  filter(G>27) %>% 
  count()
# 23... that puts you in the top 23 people in the league 
# 
# Ok let's take in to account some players may not be on the ice as much 
# if you can just get 1 more shot on goal per game at league wide shooting percentage of .09 
# thats 82 extra shots in theory for a total of 
82*.09
# 7 extra goals... that's a lot in the scheme of a player's season
# 
# In conclusion, players need to shoot more. More shots = more goals. No matter a player's playing style 
# they should aim to get more shots on net. Perhaps selfish but if given the choice between pass and shot # a player should choose shot. Goal scoring is often clouded by advanced analysis and complicated playing
# styles. In it's most basic form more shots= more goals and fixing a strategy around this simple fact
# may be a more effective and simpler way to accomplish it. 
# 
# Further analysis is needed in to the effects of shot quality, shot location, shot situation, and 
# opportunity costs of taking shots 