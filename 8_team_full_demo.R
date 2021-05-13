rm(list = ls())

require(ggplot2)
require(dplyr)

source("utility_functions.R")

set.seed(193849)

n_truth_brackets <- 100
n_adversaries <- 10

Bstruct <- matrix(c(
  4,2,1,
  4,2,1,
  5,2,1,
  5,2,1,
  6,3,1,
  6,3,1,
  7,3,1,
  7,3,1
), nrow = 8, ncol = 3, byrow = TRUE)

BstuctUnique <- sort(unique(Bstruct))

structureList <- vector("list", length = length(BstuctUnique))
for (i in seq_along(BstuctUnique))
{
  structureList[[i]] <- which(Bstruct == BstuctUnique[i], arr.ind = TRUE)
}

P <- matrix(c(
  0.5,0.4,0.3,
  0.5,0.3,0.1,
  0.5,0.2,0.1,
  0.5,0.1,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1
), nrow = 8, ncol = 3, byrow = TRUE)

checkBracket <- function(structureList, M)
{
  for (i in seq_along(structureList))
  {
    ind <- structureList[[i]]
    if (abs(sum(M[ind]) - 1) > 1E-6)
    {
      print(structureList[[i]])
      print(sum(M[ind]))
      print(M)
      stop("Section does not sum to 1")
    }
  }
}

checkBracket(structureList, P)

Th <- matrix(c(
  0.51,0.3,0.2,
  0.49,0.3,0.1,
  0.3,0.3,0.1,
  0.7,0.1,0.2,
  0.6,0.2,0.15,
  0.4,0.3,0.1,
  0.5,0.4,0.1,
  0.5,0.1,0.05
), nrow = 8, ncol = 3, byrow = TRUE)

checkBracket(structureList, Th)

################################################################################

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


################################################################################

blocks <- list(
  matrix(c(1,0,0,0,0,0), nrow = 2),
  matrix(c(0,1,0,0,0,0), nrow = 2),
  matrix(c(1,0,1,0,1,0), nrow = 2),
  matrix(c(0,1,0,1,0,1), nrow = 2),
  matrix(c(1,0,0,0,0,0), nrow = 2),
  matrix(c(0,1,0,0,0,0), nrow = 2),
  matrix(c(1,0,1,0,0,0), nrow = 2),
  matrix(c(0,1,0,1,0,0), nrow = 2)
)

require(arrangements)
temp <- arrangements::permutations(8, 4)
ind <- which(apply(temp, 1, function(z) all(c(1,2) %in% z)))
temp <- temp[-ind,]
ind <- which(apply(temp, 1, function(z) all(c(3,4) %in% z)))
temp <- temp[-ind,]
ind <- which(apply(temp, 1, function(z) all(c(5,6) %in% z)))
temp <- temp[-ind,]
ind <- which(apply(temp, 1, function(z) all(c(7,8) %in% z)))
temp <- temp[-ind,]
ind <- which(apply(temp, 1, function(z) all(z[1:2] %in% c(1,2,5,6))))
temp <- temp[-ind,]
ind <- which(apply(temp, 1, function(z) all(z[1:2] %in% c(3,4,7,8))))
temp <- temp[-ind,]

stopifnot(nrow(temp) == 2^8)

temp[temp == 5] <- 1
temp[temp == 6] <- 2

temp <- temp[-which(duplicated(temp)),]

stopifnot(nrow(temp) == 2^7)

zgrid <- list(nrow(temp))
for (i in 1:nrow(temp))
{
  zgrid[[i]] <- rbind(blocks[[temp[i,1]]],
                      blocks[[temp[i,2]]],
                      blocks[[temp[i,3]]],
                      blocks[[temp[i,4]]])
}

dummy <- lapply(zgrid, function(M) checkBracket(structureList, M))

################################################################################

zgrid_data <- data.frame(
  max_score = sapply(zgrid, function(M) max(scoreTruthSampleList(M, X))),
  mean_score = sapply(zgrid, function(M) mean(scoreTruthSampleList(M, X))),
  win = sapply(zgrid, function(M) probabilityOfWin(M)),
  distX = sapply(zgrid, function(M){
    sum((c(M)-c(Xmax))^2)
  }),
  distY = sapply(zgrid, function(M){
    sum((c(M)-c(Ymax))^2)
  }),
  logitX = sapply(zgrid, function(M) {
    sum(log(ifelse(M == 0, NA, M*P/(1-M*P))), na.rm = TRUE)
  }),
  logitY = sapply(zgrid, function(M) {
    sum(log(ifelse(M == 0, NA, M*Th/(1-M*Th))), na.rm = TRUE)
  }),
  logodds = sapply(zgrid, function(M) {
    sum(log(ifelse(M == 0, NA, M*P/Th)), na.rm = TRUE)
  })
)

zgrid_lm_data <- data.frame(t(sapply(zgrid, rowSums)))
zgrid_lm_data2 <- data.frame(t(sapply(zgrid, function(u) rowSums(u*P/Th))))
zgrid_data$logit_win <- ifelse(zgrid_data$win == 0, NA, qlogis(zgrid_data$win))

g1 <- glm(zgrid_data$logit_win ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data = zgrid_lm_data)
summary(g1)

g1 <- glm(zgrid_data$logit_win ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 - 1, data = zgrid_lm_data2)
summary(g1)

require(ggplot2)
ggplot(zgrid_data, aes(x = max_score, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = mean_score, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = distX, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = distY, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = logitX, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = logitY, y = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = logodds, y = win)) + geom_jitter()

ggplot(zgrid_data, aes(x = distX, y = distY, col = win)) + geom_jitter()
ggplot(zgrid_data, aes(x = logitX, y = logitY, col = win)) + geom_jitter()

ind <- which.max(zgrid_data$win)
zgrid[[ind]]


which(sapply(zgrid, function(u) sum(u*Xmax)/(nrow(u) - 1)) > 0.7)
which(sapply(zgrid, function(u) sum(u*Xmax*Ymax)/(nrow(u) - 1)) > 0.5)

# start from Xmax
# start from Ymax



