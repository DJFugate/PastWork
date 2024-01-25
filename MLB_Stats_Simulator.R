library(ggplot2)

# using 2022 stats (not including HBP and SF)
## 0 = Out
## 1 = Single
## 2 = Double
## 3 = Triple
## 4 = Home Run
## 5 = Walk

Adolis_Garcia <- c(rep(0, 454), rep(1, 85), rep(2, 34), rep(3, 5), rep(4, 27), rep(5, 40))
Freddie_Freeman <- c(rep(0, 413), rep(1, 129), rep(2, 47), rep(3, 2), rep(4, 21), rep(5, 84))
Barry_Bonds_2001 <- c(rep(0, 320), rep(1, 49), rep(2, 32), rep(3, 2), rep(4, 73), rep(5, 177))
length(Barry_Bonds_2001)

# Median average Abs per game in 2022 = 33.54, 33.87/9= 3.73 ~ 4 ABs per player per game
num_sims <- 1000
num_games <- 162
total_abs <- 0
total_bases <- 0
hits <- 0
AVG <- 0
SLUG <- 0
OBP <- 0
OPS <- 0
BBs <- 0

for (i in 1:num_sims) {
  hits <- 0
  for (j in 1:num_games) {
    outcomes <- sample(Freddie_Freeman, size = 4, replace = T)
    BBs[j] <- sum(outcomes == 5)
    total_abs[j] <- sum(outcomes != 5)
    hits[j] <- sum(outcomes != 0 & outcomes != 5)
    total_bases[j] <- sum(outcomes[which(outcomes != 5)])
  }
  SLUG[i] <- sum(total_bases) / sum(total_abs)
  AVG[i] <- sum(hits) / sum(total_abs)
  OBP[i] <- (sum(hits) + sum(BBs)) / sum(total_abs)
  OPS[i] <- OBP[i] + SLUG[i]
}

# Making vectors into data frames to plot
AVG <- data.frame(AVG)
OBP <- data.frame(OBP)
SLUG <- data.frame(SLUG)
OPS <- data.frame(OPS)

# Histograms of each statistic
ggplot(AVG, aes(x = AVG)) + geom_histogram(fill = "cyan", color = "black") +
  labs(title = "Distribution of Averages",
       y = "")

ggplot(OBP, aes(x = OBP)) + geom_histogram(fill = "cyan", color = "black") +
  labs(title = "Distribution of OBP",
       y = "")

ggplot(SLUG, aes(x = SLUG)) + geom_histogram(fill = "cyan", color = "black") +
  labs(title = "Distribution of Slugging %",
       y = "")

ggplot(OPS, aes(x = OPS)) + geom_histogram(fill = "cyan", color = "black") +
  labs(title = "Distribution of OPS (On Base Plus Slugging)",
       y = "")

# Summaries of the Statistics
summary(AVG)
summary(OBP)
summary(SLUG)
summary(OPS)

# Slash Lines
best_year <- which(OPS == max(OPS))
AVG[best_year,]
OBP[best_year,]
SLUG[best_year,]

print(paste("Based on their actual 2022 hitting statistics, the best simulated slash line for <Player Name> during 2022 is", round(AVG[best_year,], 3), "/", round(OBP[best_year,], 3), "/", round(SLUG[best_year,], 3), "."))

