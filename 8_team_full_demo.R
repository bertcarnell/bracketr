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
      stop("Section does not sum to 1")
    }
  }
}

checkBracket(structureList, P)

Th <- matrix(c(
  0.5,0.4,0.3,
  0.5,0.3,0.1,
  0.5,0.2,0.1,
  0.5,0.1,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1,
  0.5,0.25,0.1
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
  list(matrix(c(1,0,0,0,0,0), nrow = 2),
       matrix(c(0,1,0,0,0,0), nrow = 2)),
  list(matrix(c(1,0,1,0,1,0), nrow = 2),
       matrix(c(0,1,0,1,0,1), nrow = 2)),
  list(matrix(c(1,0,0,0,0,0), nrow = 2),
       matrix(c(0,1,0,0,0,0), nrow = 2)),
  list(matrix(c(1,0,1,0,0,0), nrow = 2),
       matrix(c(0,1,0,1,0,0), nrow = 2))
)




