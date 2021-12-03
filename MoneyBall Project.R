library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)

# Load data

batting <- read.csv('Batting.csv')
head(batting)

# Check structure and summary of data

str(batting)
summary(batting)

# Feature Engineering

# Batting Average (BA)

batting$BA <- batting$H / batting$AB

# On Base Percentage (OBP)

batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

# Slugging Percentage (SLG)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

batting$SLG <- (batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) / batting$AB

str(batting)

# Merging Salary Data with Batting Data

salary <- read.csv("Salaries.csv")
summary(salary)

batting.after1985 <- batting[batting$yearID >= 1985, ]
summary(batting.after1985)

combo <- merge(batting.after1985, salary, c('playerID','yearID'))

summary(combo)

# Analyzing the Lost Players

lost_players <- subset(combo, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))

lost_players_after_2001 <- subset(lost_players, yearID == 2001)
lost_players_after_2001

lost_players_after_2001 <- lost_players_after_2001[c("playerID", "H", "X2B", "X3B", "HR", "OBP", "SLG", "BA", "AB", "salary")]
lost_players_after_2001

# Replacement Players

theme_set(theme_dark())

combo_2021 <- subset(combo, yearID == 2001)
combo_2021 <- combo_2021[c("playerID", "H", "X2B", "X3B", "HR", "OBP", "SLG", "BA", "AB", "salary")]
head(combo_2021)

pl <- ggplot(combo_2021, aes(x = OBP,y = salary, color = AB)) + 
                          geom_point(size = 2, alpha = 0.5) + 
                          scale_color_gradient(high = "green", low = "red")

pointsToLabel <- c('giambja01', 'damonjo01', 'saenzol01')

pl <- pl + geom_text(aes(label = playerID), color = "black",
                       data = subset(combo_2021, playerID %in% pointsToLabel), check_overlap = TRUE)


pl <- pl + xlab("On Base Percentage (OBP)") + 
  ylab("Salary") + 
  ggtitle("Dependence of Salary and OBP") +
  scale_x_continuous(limits = c(0, max(combo_2021$OBP + 0.1), breaks = seq(0, 1, 0.1)) + 
  scale_y_continuous(limits = c(0, max(combo_2021$salary) + 250000), breaks = seq(0, 22500000, 1000000))) 
  
pl

# gpl <- ggplotly(pl)
  
# gpl

# Let's limit players by asked criteria

limited_combo_2021 <- combo_2021 %>% 
                      filter(AB >= sum(lost_players_after_2001$AB)/length(lost_players_after_2001$AB), 
                      OBP >= mean(lost_players_after_2001$OBP), 
                      salary <= 15000000/length(lost_players_after_2001$salary)) %>% 
                      arrange(desc(OBP))

# Remove our ex-players

limited_combo_2021 <- filter(limited_combo_2021,!(playerID %in% c('giambja01', 'damonjo01', 'saenzol01')))

# Plot filtered data


final_pl <- ggplot(limited_combo_2021, aes(x = OBP,y = salary, color = AB)) + 
  geom_point(size = 5, alpha = 0.75) + 
  scale_color_gradient(high = "green", low = "red")


final_pl <- final_pl + geom_text(aes(label = playerID), color = "black",
                     data = limited_combo_2021, check_overlap = TRUE)


final_pl <- final_pl + xlab("On Base Percentage (OBP)") + 
  ylab("Salary") + 
  ggtitle("Dependence of Salary and OBP (Similar to Ex-players") +
  scale_x_continuous(limits = c(0, max(limited_combo_2021$OBP + 0.1), breaks = seq(0, 1, 0.1)) + 
  scale_y_continuous(limits = c(0, max(limited_combo_2021$salary) + 500000), breaks = seq(0, max(limited_combo_2021$salary), 100000))) 

final_pl

# final_gpl <- ggplotly(final_pl)

# final_gpl

# 3 Cheapest Players

cheapest.players <- head(arrange(limited_combo_2021, salary), 3)
print(cheapest.players)

# 3 Players with Best On Base Percentage (OBP)

best.OBP.players <- head(arrange(limited_combo_2021, desc(OBP)), 3)
print(best.OBP.players)


# 3 Players with Best At Bats (AB)

best.AB.players <- head(arrange(limited_combo_2021, desc(AB)), 3)
print(best.AB.players)
