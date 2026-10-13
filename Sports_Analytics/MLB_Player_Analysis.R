install.packages("Lahman")
library(tidyverse)
library(Lahman)

#examine and save biographical data
view(People)
info_player <- People

#view an edit batting data
view(Batting)
bats <- Batting %>% 
  group_by(playerID) %>% #find every players total home runs, bases stolen, and hit percentage for career
  summarize(Games_Played = sum(G),
            Total_HR = sum(HR),
            Total_Stolen = sum(SB),
            total_At_Bat = sum(AB),
            total_Hits = sum(H),
            hit_percent = sum(H)/sum(AB)
            )

baseball <- bats %>% #filter data for just columns i want
  inner_join(info_player) %>% 
  filter(Games_Played>10,
         hit_percent > .1,
         total_At_Bat > 50
         ) %>% 
  select(nameGiven, nameLast, Games_Played, Total_HR, 
         Total_Stolen, total_At_Bat, Total_HR, hit_percent, height, weight)


#find relationship between height/weight and career stats

#specifically does height/weight affect batting average, bases stolen, total hr

baseball %>%
  filter(Games_Played>150) %>% #do taller people hit better? No, height is negatively correlated with batting average
  ggplot(aes(x=height, y=hit_percent)) +
  geom_smooth()+ 
  geom_point()
  
baseball %>% 
  filter(Games_Played > 150) %>% #no relationship
ggplot(aes(x=weight, y=hit_percent)) +
  geom_smooth()+ 
  geom_point()

baseball %>% 
  filter(Total_Stolen > 10, Games_Played>150) %>% #are taller people better at stealing bases?
ggplot(aes(x=height, y=Total_Stolen)) + #also negatively correlated
  geom_smooth()+ 
  geom_point()


baseball %>% 
  filter(
    Total_HR > 30, Games_Played > 150 #positively correlated
  ) %>% 
ggplot(aes(x=height, y=Total_HR)) +
  geom_smooth()+ 
  geom_point()

baseball %>%
  filter(Total_HR > 30, Games_Played > 150) %>% 
  ggplot(aes(x=hit_percent, y=Total_HR))+
  geom_smooth()+
  geom_point()

#make nicer plots for batting avg, hr, and height

baseball %>%
  filter(Games_Played>150, Total_HR > 30) %>%
  ggplot(aes(x=height, y=hit_percent))+
  geom_smooth(col="red")+ 
  geom_point()+
  labs(x= "Height (inches)",
       y="Career Batting Average", 
       title = "Does Height Affect Batting Average?",
       subtitle = "Games Played > 150 & 30+ Career HR")+
  theme_classic()

baseball %>% 
  filter(Total_HR > 30, Games_Played > 150) %>% 
  ggplot(aes(x=height, y=Total_HR)) +
  geom_smooth(col="limegreen")+ 
  geom_point()+
  labs(
    x= "Height (inches)",
    y= "Career Home Runs",
    title = "Do Tall People Hit More Home Runs?",
    subtitle = "Games Played > 150 & 30+ Career HR"
  )+
  theme_classic()
