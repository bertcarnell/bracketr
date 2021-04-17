
drawBracket <- function(structureList, P, winner = NA)
{
  X <- matrix(NA, nrow = nrow(P), ncol = ncol(P))
  for (i in seq_along(structureList))
  {
    ind <- structureList[[i]]
    if (ind[1,2] == ncol(X))
    {
      if (is.na(winner))
      {
        X[ind] <- rmultinom(1, size = 1, prob = P[ind])
      } else
      {
        X[ ,ncol(P)] <- 0
        X[winner, ncol(P)] <- 1
      }
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


drawMaxLikelihoodBracket <- function(structureList, P)
{
  X <- matrix(NA, nrow = nrow(P), ncol = ncol(P))
  for (i in seq_along(structureList))
  {
    ind <- structureList[[i]]
    if (ind[1,2] == ncol(X))
    {
      ind2 <- which.max(P[ind])
      temp <- rep(0, length=nrow(ind))
      temp[ind2] <- 1
      X[ind] <- temp
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
        ind2 <- which.max(P[ind])
        temp <- rep(0, length=nrow(ind))
        temp[ind2] <- 1
        X[ind] <- temp
      }
    }
  }
  return(X)
}

drawEquiprobableBracket <- function(n_teams, n_rounds, structureList)
{
  M <- matrix(NA, nrow = n_teams, ncol = n_rounds)
  M[,n_rounds] <- rep(1/n_teams, n_teams)
  M[,5] <- rep(2/n_teams, n_teams)
  M[,4] <- rep(4/n_teams, n_teams)
  for (i in 1:n_rounds)
  {
    M[,i] <- rep(2^(n_rounds-i)/n_teams, n_teams)
  }
  return(drawBracket(structureList, M))
}

scoreBracket <- function(M, Mtruth)
{
  # ESPN tracks March Madness brackets using a 10-20-40-80-160-320 scoring system.
  sum(apply(M*Mtruth, 2, sum)*c(10, 20, 40, 80, 160, 320))
}

# method 1 swaps rows from a game
# method 2 draws after a game is swapped
permuteBracket <- function(M, method = 1)
{
  #set.seed(193939)
  #M <- X[[1]]
  #n_games <- 63
  #game_num <- 62
  # change the outcome of one of the games
  n_games <- nrow(M) - 1
  game_num <- sample(1:n_games, size = 1)
  ind <- which(Bstruct == game_num, arr.ind = TRUE)
  rnd <- ind[1,2]
  if (method == 1)
  {
    if (rnd > 1) # if not the first round
    {
      ind_back <- ind
      ind_back[,2] <- rnd - 1
      winner <- which(M[ind] == 1)
      loser <- base::setdiff(which(M[ind_back] == 1), winner)
      winner <- ind[winner,1]
      loser <- ind[loser,1]
    } else # if the first round, there are only two options
    {
      winner <- ind[1,1]
      loser <- ind[2,1]
    }
    temp <- M[winner,]
    M[winner,] <- M[loser,]
    M[loser,] <- temp
    return(M)
  } else if (method == 2)
  {
    if (rnd == 1)
    {
      #swap
      M[ind] <- rev(M[ind])
    } else if (rnd > 1)
    {
      ind_back <- ind
      ind_back[,2] <- rnd - 1
      winner <- which(M[ind] == 1)
      loser <- base::setdiff(which(M[ind_back] == 1), winner)
      winner <- ind[winner,1]
      loser <- ind[loser,1]
      #swap
      temp <- M[winner,rnd]
      M[winner,rnd] <- M[loser,rnd]
      M[loser,rnd] <- temp
    }
    if (rnd <= 5)
    {
      for (k in rnd:5)
      {
        game_num <- floor(game_num / 2)
        ind2 <- which(Bstruct == game_num, arr.ind = TRUE)
        back_ind2 <- ind2
        back_ind2[,2] <- ind2[1,2] - 1
        M[ind2] <- rmultinom(1, size = 1, prob = M[back_ind2] * P[ind2])
      }
    }
    return(M)
  } else
  {
    stop("method not implemented")
  }
}

if (FALSE) #####################################################################
{
  M1 <- X[[1]]
  for (i in 1:1000) {
    M1 <- permuteBracket(M, method = 1)
    checkBracket(structureList, M)
  }
  for (i in 1:1000){
    M1 <- permuteBracket(M, method = 2)
    checkBracket(structureList, M)
  }
} ##############################################################################

# Yscore is a global variable
# n_truth_brackers is a global variable
probabilityOfWin <- function(Z)
{
  Zscore <- sapply(X, function(Mtruth) scoreBracket(Z, Mtruth))
  
  ind <- which(mapply(function(a, b)
  {
    all(a > b)
  }, a = Zscore, b = Yscore))
  
  length(ind) / n_truth_brackets
}

# X is a global draw from the truth distribution
# Y is a global draw from the adversary distribution
# n_truth_brackets is a global variable
# Yscores are a global object 
geneticBracketSearch <- function(seedBracket, pRate, nchildren, ngenerations, debug = FALSE, method = 1)
{
  cat(paste(Sys.time(), "\n"))
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
    # want to cool the permutation rate with generations
    # pRate = pRate / (1 + log(i))
    pRate <- (0.5 - pRate) / (ngenerations - 1) * (i - 1) + pRate
    if (debug) cat(paste("\tGeneration", i, "Poisson Rate", round(pRate, 3), "P(win)", current_best_pwin, "\n"))
    for (j in 1:nchildren)
    {
      nmutations <- rpois(1, pRate) + 1
      children[[j]] <- current_best
      for (k in 1:nmutations)
      {
        children[[j]] <- permuteBracket(children[[j]], method)
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
    }
  }
  cat(paste(Sys.time(), "\n"))
  return(list(current_best = current_best,
              score = current_best_score,
              start_score = start_score,
              pwin = current_best_pwin,
              start_pwin = start_pwin,
              scores = scoreTruthSampleList(current_best, X),
              cov = correlateTruthSampleList(current_best, X),
              covA = correlateAdversarySampleList(current_best, Y, n_truth_brackets)))
}

drawTruthSampleList <- function(P, structureList, n_truth_brackets)
{
  return(lapply(1:n_truth_brackets, function(i) drawBracket(structureList, P)))
}

checkTruthSampleList <- function(X, structureList)
{
  dummy <- sapply(X, function(M) checkBracket(structureList, M))
}

drawWinnerSampleList <- function(P, structureList, n_truth_brackets, winner)
{
  return(lapply(1:n_truth_brackets, function(i) drawBracket(structureList, P, winner)))
}

drawAdversarySampleList <- function(Th, structureList, n_truth_brackets, n_adversaries)
{
  Y <- vector("list", length = n_truth_brackets)
  Y <- lapply(1:n_truth_brackets, function(i)
  {
    lapply(1:n_adversaries, function(j)
    {
      drawBracket(structureList, Th)
    })
  })
  return(Y)
}

checkAdversarySampleList <- function(Y, structureList, n_truth_brackets, n_adversaries)
{
  dummy <- lapply(1:n_truth_brackets, function(i)
  {
    lapply(1:n_adversaries, function(j)
    {
      checkBracket(structureList, Y[[i]][[j]])
    })
  })
}

scoreTruthSampleList <- function(Z, X)
{
  return(sapply(X, function(Mtruth) scoreBracket(Z, Mtruth)))
}

scoreAdversarySampleList <- function(Y, X, n_truth_brackets)
{
  Yscore <- vector("list", n_truth_brackets)
  for (i in seq_along(Y))
  {
    Yscore[[i]] <- sapply(Y[[i]], function(M) scoreBracket(M, X[[i]]))
  }
  return(Yscore)
}

correlateTruthSampleList <- function(Z, X)
{
  return(sapply(X, function(Mtruth) cov(rowSums(Mtruth), rowSums(Z))))
}

correlateAdversarySampleList <- function(Z, Y, n_truth_brackets)
{
  return(sapply(Y, function(ML)
  {
    mean(sapply(ML, function(M) cov(rowSums(M), rowSums(Z))))
  }))
}

