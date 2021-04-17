rm(list=ls())

require(ggplot2)
require(dplyr)

source("read_brackets.R")
source("utility_functions.R")

set.seed(193849)

n_truth_brackets <- 500 # need to get up to 10,000
n_adversaries <- 100

X <- drawTruthSampleList(P, structureList, n_truth_brackets)
checkTruthSampleList(X, structureList)
  
Xmax <- drawMaxLikelihoodBracket(structureList, P)
checkBracket(structureList, Xmax)

# espn brackets
Y <- drawAdversarySampleList(Th, structureList, n_truth_brackets, n_adversaries)
checkAdversarySampleList(Y, structureList, n_truth_brackets, n_adversaries)
  
Ymax <- drawMaxLikelihoodBracket(structureList, Th)
checkBracket(structureList, Ymax)

Yscore <- scoreAdversarySampleList(Y, X, n_truth_brackets)

################################################################################

Z <- Y[[1]][[2]]
checkBracket(structureList, Z)
Zscore <- scoreTruthSampleList(Z, X)

newZ <- geneticBracketSearch(Z, 3, 50, 50, TRUE)
cat(paste("Score from", newZ$start_score, "to", newZ$score, "\n"))
cat(paste("P(win) from", newZ$start_pwin, "to", newZ$pwin, "\n"))

plot_data <- data.frame(
  group = c(rep("Original Bracket", length(Zscore)),
            rep("Adversary Brackets", length(unlist(Yscore))),
            rep("New Bracket", length(newZ$scores))),
  score = c(Zscore, unlist(Yscore), newZ$scores)
)

ggplot(plot_data, aes(x = score, group = group, col = group)) +
  geom_density() +
  labs(x = "Score", y = "Density", col = "") +
  xlim(0, 1920) +
  geom_vline(aes(xintercept = score, col = group), lty = 2, 
             data = plot_data %>% group_by(group) %>% summarise(score = mean(score)))

plot_data2 <- data.frame(
  group = factor(rep(c("Original Bracket", "New Bracket", "Mean Adversary", "Mean Truth"), 
                     each = length(trans$TEAM)),
                 levels = c("Mean Truth", "Mean Adversary", "Original Bracket", "New Bracket")),
  team = factor(rep(trans$TEAM, times = 4), levels = rev(trans$TEAM)),
  round = c(rowSums(Z), 
            rowSums(newZ$current_best), 
            apply(sapply(Y, function(z) {
                              apply(sapply(z, rowSums), 1, mean)
                            }), 1, mean),
            apply(sapply(X, function(z) rowSums(z)), 1, mean))
)

ggplot(plot_data2, aes(x = round, y = team, fill = group, group = group)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  facet_grid(. ~ group) +
  labs(y = "", x = "Round", fill = "")

plot_data2 <- data.frame(
  group = factor(rep(c("Max Likelihood Truth", "Max Likelihood Adversary", "Difference"), 
                     each = length(trans$TEAM))),
  team = factor(rep(trans$TEAM, times = 3), levels = rev(trans$TEAM)),
  round = c(rowSums(Xmax), 
            rowSums(Ymax),
            abs(rowSums(Xmax) - rowSums(Ymax)))
)

ggplot(plot_data2, aes(x = round, y = team, fill = group, group = group)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  facet_grid(. ~ group) +
  labs(y = "", x = "Round", fill = "")

################################################################################

Z <- Xmax
checkBracket(structureList, Z)
Zscore <- scoreTruthSampleList(Z, X)

set.seed(1938383)
newZ1 <- geneticBracketSearch(Z, 3, 100, 50, TRUE)
cat(paste("Score from", newZ1$start_score, "to", newZ1$score, "\n"))
cat(paste("P(win) from", newZ1$start_pwin, "to", newZ1$pwin, "\n"))

newZ1 <- geneticBracketSearch(Z, 3, 100, 50, TRUE, method = 2)
cat(paste("Score from", newZ1$start_score, "to", newZ1$score, "\n"))
cat(paste("P(win) from", newZ1$start_pwin, "to", newZ1$pwin, "\n"))

plot_data <- data.frame(
  group = c(rep("Original Bracket", length(Zscore)),
            rep("Adversary Brackets", length(unlist(Yscore))),
            rep("New Bracket", length(newZ1$scores))),
  score = c(Zscore, unlist(Yscore), newZ1$scores)
)

ggplot(plot_data, aes(x = score, group = group, col = group)) +
  geom_density() +
  labs(x = "Score", y = "Density", col = "") +
  xlim(0, 1920) +
  geom_vline(aes(xintercept = score, col = group), lty = 2, 
             data = plot_data %>% group_by(group) %>% summarise(score = mean(score)))

plot_data2 <- data.frame(
  group = factor(rep(c("Original Bracket", "New Bracket", "Mean Adversary", "Mean Truth"), 
                     each = length(trans$TEAM)),
                 levels = c("Mean Truth", "Mean Adversary", "Original Bracket", "New Bracket")),
  team = factor(rep(trans$TEAM, times = 4), levels = rev(trans$TEAM)),
  round = c(rowSums(Z), 
            rowSums(newZ1$current_best), 
            apply(sapply(Y, function(z) {
              apply(sapply(z, rowSums), 1, mean)
            }), 1, mean),
            apply(sapply(X, function(z) rowSums(z)), 1, mean))
)

windows(8, 8)
ggplot(plot_data2, aes(x = round, y = team, fill = group, group = group)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  facet_grid(. ~ group) +
  labs(y = "", x = "Round", fill = "")

plot(newZ1$scores, newZ1$cov, ylab="Covariance with truth", xlab = "score")
plot(newZ1$scores, newZ1$covA, ylab="Covariance with Adversary", xlab = "score")

################################################################################

set.seed(3148493)
Z <- Ymax
checkBracket(structureList, Z)
Zscore <- scoreTruthSampleList(Z, X)

newZ2 <- geneticBracketSearch(Z, 3, 100, 50, TRUE)
cat(paste("Score from", newZ2$start_score, "to", newZ2$score, "\n"))
cat(paste("P(win) from", newZ2$start_pwin, "to", newZ2$pwin, "\n"))

plot_data <- data.frame(
  group = c(rep("Original Bracket", length(Zscore)),
            rep("Adversary Brackets", length(unlist(Yscore))),
            rep("New Bracket", length(newZ2$scores))),
  score = c(Zscore, unlist(Yscore), newZ2$scores)
)

ggplot(plot_data, aes(x = score, group = group, col = group)) +
  geom_density() +
  labs(x = "Score", y = "Density", col = "") +
  xlim(0, 1920) +
  geom_vline(aes(xintercept = score, col = group), lty = 2, 
             data = plot_data %>% group_by(group) %>% summarise(score = mean(score)))

plot_data2 <- data.frame(
  group = factor(rep(c("Original Bracket", "New Bracket", "Mean Adversary", "Mean Truth"), 
                     each = length(trans$TEAM)),
                 levels = c("Mean Truth", "Mean Adversary", "Original Bracket", "New Bracket")),
  team = factor(rep(trans$TEAM, times = 4), levels = rev(trans$TEAM)),
  round = c(rowSums(Z), 
            rowSums(newZ2$current_best), 
            apply(sapply(Y, function(z) {
              apply(sapply(z, rowSums), 1, mean)
            }), 1, mean),
            apply(sapply(X, function(z) rowSums(z)), 1, mean))
)

ggplot(plot_data2, aes(x = round, y = team, fill = group, group = group)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  facet_grid(. ~ group) +
  labs(y = "", x = "Round", fill = "")

################################################################################

set.seed(193449)
Z <- drawEquiprobableBracket(64, 6, structureList)
checkBracket(structureList, Z)
Zscore <- scoreTruthSampleList(Z, X)

newZ3 <- geneticBracketSearch(Z, 3, 100, 50, TRUE)
cat(paste("Score from", newZ3$start_score, "to", newZ3$score, "\n"))
cat(paste("P(win) from", newZ3$start_pwin, "to", newZ3$pwin, "\n"))

plot_data <- data.frame(
  group = c(rep("Original Bracket", length(Zscore)),
            rep("Adversary Brackets", length(unlist(Yscore))),
            rep("New Bracket", length(newZ2$scores))),
  score = c(Zscore, unlist(Yscore), newZ3$scores)
)

ggplot(plot_data, aes(x = score, group = group, col = group)) +
  geom_density() +
  labs(x = "Score", y = "Density", col = "") +
  xlim(0, 1920) +
  geom_vline(aes(xintercept = score, col = group), lty = 2, 
             data = plot_data %>% group_by(group) %>% summarise(score = mean(score)))

plot_data2 <- data.frame(
  group = factor(rep(c("Original Bracket", "New Bracket", "Mean Adversary", "Mean Truth"), 
                     each = length(trans$TEAM)),
                 levels = c("Mean Truth", "Mean Adversary", "Original Bracket", "New Bracket")),
  team = factor(rep(trans$TEAM, times = 4), levels = rev(trans$TEAM)),
  round = c(rowSums(Z), 
            rowSums(newZ3$current_best), 
            apply(sapply(Y, function(z) {
              apply(sapply(z, rowSums), 1, mean)
            }), 1, mean),
            apply(sapply(X, function(z) rowSums(z)), 1, mean))
)

ggplot(plot_data2, aes(x = round, y = team, fill = group, group = group)) +
  geom_bar(stat = "identity", width = 0.5, position = position_dodge()) +
  facet_grid(. ~ group) +
  labs(y = "", x = "Round", fill = "")

################################################################################

# draw multiple brackets from the winners

trans$TEAM[order(P[,6], decreasing = TRUE)]

N_per_team <- 1000

gonz <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Gonzaga"))
gonz_pwin <- sapply(gonz, probabilityOfWin)

ill <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Illinois"))
ill_pwin <- sapply(gonz, probabilityOfWin)

bay <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Baylor"))
bay_pwin <- sapply(bay, probabilityOfWin)

iow <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Iowa"))
iow_pwin <- sapply(iow, probabilityOfWin)

hou <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Houston"))
hou_pwin <- sapply(hou, probabilityOfWin)

osu <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Ohio State"))
osu_pwin <- sapply(osu, probabilityOfWin)

mic <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Michigan"))
mic_pwin <- sapply(mic, probabilityOfWin)

ala <- drawWinnerSampleList(P, structureList, N_per_team, which(trans$TEAM == "Alabama"))
ala_pwin <- sapply(ala, probabilityOfWin)

plot_data_teams <- data.frame(
  team = rep(c("Gonzaga", "Illinois", "Baylor", "Iowa", "Houston", "Ohio State", "Michigan", "Alabama"),
             each = N_per_team),
  pwin = c(gonz_pwin, ill_pwin, bay_pwin, iow_pwin, hou_pwin, osu_pwin, mic_pwin, ala_pwin)
)

ggplot(plot_data_teams, aes(x = pwin, group = team, col = team)) +
  geom_density() +
  labs(x = "Probability of Win for Truth Brackets with the noted winning Team",
       y = "Density",
       col = "")

ggplot(plot_data_teams, aes(y = pwin, x = team, group = team, col = team)) +
  geom_boxplot() +
  labs(x = "Probability of Win for Truth Brackets with the noted winning Team",
       y = "Win Probability",
       col = "")

################################################################################

cov(rowSums(Xmax), rowSums(Ymax))

cov(rowSums(Xmax), rowSums(Ymax))
