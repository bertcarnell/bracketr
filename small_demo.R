
n_truth_brackets <- 100
n_adversaries <- 5

## bracket structure

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

## create a listing of the game groupings so the "which" search only needs to done once

structureList <- vector("list", length = length(BstuctUnique))
for (i in seq_along(BstuctUnique))
{
  structureList[[i]] <- which(Bstruct == BstuctUnique[i], arr.ind = TRUE)
}

# 538 probabilities
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
    stopifnot(abs(sum(M[ind]) - 1) < 1E-6)
  }
}

checkBracket(structureList, P)

# ESPN probabilities
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

drawBracket <- function(structureList, P)
{
  X <- matrix(NA, nrow = nrow(P), ncol = ncol(P))
  for (i in seq_along(structureList))
  {
    ind <- structureList[[i]]
    if (ind[1,2] == ncol(X))
    {
      X[ind] <- rmultinom(1, size = 1, prob = P[ind])
    } else
    {
      current_column <- ind[1,2]
      higher_column <- current_column + 1
      higher_ind <- ind
      higher_ind[,2] <- higher_column
      if (any(X[higher_ind] == 1))
      {
        X[ind] <- X[higher_ind]
      } else
      {
        X[ind] <- rmultinom(1, size = 1, prob = P[ind])
      }
    }
  }
  return(X)
}

# 538, truth brackets
X <- lapply(1:n_truth_brackets, function(i) drawBracket(structureList, P))
dummy <- sapply(X, function(M) checkBracket(structureList, M))

# espn brackets
Y <- vector("list", length = n_truth_brackets)
Y <- lapply(1:n_truth_brackets, function(i)
{
  lapply(1:n_adversaries, function(j)
  {
    drawBracket(structureList, Th)
  })
})

dummy <- lapply(1:n_truth_brackets, function(i)
{
  lapply(1:n_adversaries, function(j)
  {
    checkBracket(structureList, Y[[i]][[j]])
  })
})

# test bracket
Z <- drawBracket(structureList, P)

scoreBracket <- function(M, Mtruth)
{
  # ESPN tracks March Madness brackets using a 10-20-40-80-160-320 scoring system.
  sum(apply(M*Mtruth, 2, sum)*c(80, 160, 320))
}

Zscore <- sapply(X, function(Mtruth) scoreBracket(Z, Mtruth))

Yscore <- vector("list", n_truth_brackets)
for (i in seq_along(Y))
{
  Yscore[[i]] <- sapply(Y[[i]], function(M) scoreBracket(M, X[[i]]))
}

ind <- which(mapply(function(a, b)
{
  all(a >= b)
}, a = Zscore, b = Yscore))

# P(win | Z)
length(ind) / n_truth_brackets


permuteBracket <- function(M, n_games)
{
  #set.seed(193939)
  #M <- X[[1]]
  #n_games <- 7
  #game_num <- 3
  # change the outcome of one of the games
  game_num <- sample(1:n_games, size = 1)
  ind <- which(Bstruct == game_num, arr.ind = TRUE)
  if (ind[1,2] > 1)
  {
    ind_back <- ind
    ind_back[,2] <- ind[1,2] - 1
    winner <- which(M[ind] == 1)
    loser <- setdiff(which(M[ind_back] == 1), winner)
    winner <- ind[winner,1]
    loser <- ind[loser,1]
  } else
  {
    winner <- ind[1,1]
    loser <- ind[2,1]
  }
  temp <- M[winner,]
  M[winner,] <- M[loser,]
  M[loser,] <- temp
  return(M)
}

for (i in 1:100)
{
  test <- permuteBracket(X[[1]], 7)
  checkBracket(structureList, test)
}


probabilityOfWin <- function(Z)
{
  Zscore <- sapply(X, function(Mtruth) scoreBracket(Z, Mtruth))

  ind <- which(mapply(function(a, b)
  {
    all(a >= b)
  }, a = Zscore, b = Yscore))
  
  length(ind) / n_truth_brackets
}

Yscore <- vector("list", n_truth_brackets)
for (i in seq_along(Y))
{
  Yscore[[i]] <- sapply(Y[[i]], function(M) scoreBracket(M, X[[i]]))
}

# X is a global draw from the truth distribution
# Y is a global draw from the adversary distribution
# n_truth_brackets is a global variable
# Yscores are a global object 
geneticBracketSearch <- function(seedBracket, pRate, nchildren, ngenerations, n_games)
{
  current_best <- seedBracket
  children <- vector("list", length = nchildren)
  current_best_score <- mean(sapply(X, function(truth_list_element) scoreBracket(seedBracket, truth_list_element)))
  current_best_pwin <- probabilityOfWin(current_best)
  start_score <- current_best_score
  start_pwin <- current_best_pwin
  children_scores <- numeric(nchildren)
  children_pwin <- numeric(nchildren)
  for (i in 1:ngenerations)
  {
    for (j in 1:nchildren)
    {
      nmutations <- rpois(1, pRate) + 1
      children[[j]] <- current_best
      for (k in 1:nmutations)
      {
        children[[j]] <- permuteBracket(children[[j]], n_games)
      }
      children_scores[j] <- mean(sapply(X, function(truth_list_element) scoreBracket(children[[j]], truth_list_element)))
      children_pwin[j] <- probabilityOfWin(children[[j]])
    }
    ind <- which.max(children_pwin)
    if (children_pwin[ind] > current_best_pwin)
    {
      current_best_pwin <- children_pwin[ind]
      current_best_score <- children_scores[ind]
      current_best <- children[[ind]]
      print(paste("Generation", i))
      print(current_best)
      print(current_best_score)
      print(current_best_pwin)
    }
  }
  return(list(current_best = current_best,
              score = current_best_score,
              start_score = start_score,
              pwin = current_best_pwin,
              start_pwin = start_pwin))
}

geneticBracketSearch(Y[[94]][[1]], 0.5, 20, 10, 7)
