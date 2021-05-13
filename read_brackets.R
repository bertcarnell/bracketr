# Read Bracket

require(openxlsx)
source("utility_functions.R")

Praw <- read.xlsx("2021BracketMetadata.xlsx", sheet = "538")
Thraw <- read.xlsx("2021BracketMetadata.xlsx", sheet = "ESPN")
trans <- read.xlsx("2021BracketMetadata.xlsx", sheet = "Order")

Praw_teams_clean <- gsub("[[:space:]][0-9]+", "", Praw$TEAM)
ind <- match(tolower(trans$TEAM), table = tolower(Praw_teams_clean))

#cbind(Praw_teams_clean[ind], trans$TEAM)

P <- matrix(NA, nrow=64, ncol=6)

normalizeValues <- function(x, desiredSum)
{
  temp <- suppressWarnings(as.numeric(x))
  if (any(is.na(temp)))
  {
    if ((desiredSum - sum(temp, na.rm = TRUE)) < 0)
    {
      temp[which(is.na(temp))] <- 0.001
    } else
    {
      temp[which(is.na(temp))] <- (desiredSum - sum(temp, na.rm = TRUE)) / length(which(is.na(temp)))
    }
  } 
  return(temp)
}

P[,1] <- normalizeValues(Praw$`2ND`, 32)[ind]
P[,2] <- normalizeValues(Praw$SWEET.16, 16)[ind]
P[,3] <- normalizeValues(Praw$ELITE.EIGHT, 8)[ind]
P[,4] <- normalizeValues(Praw$FINAL.FOUR, 4)[ind]
P[,5] <- normalizeValues(Praw$CHAMP., 2)[ind]
P[,6] <- normalizeValues(Praw$WIN, 1)[ind]

apply(P, 2, sum)

extractTheta <- function(rnd)
{
  #rnd <- Thraw$R64
  name <- gsub("^[0-9]+", "", rnd)
  name <- gsub("[-][0-9.%]+$", "", name)
  ind <- match(trans$TEAM_ESPN, table = name)
  #print(trans$TEAM_ESPN[which(is.na(ind))])
  num <- gsub("^.+[-]", "", rnd)
  num <- gsub("[%]", "", num)
  num <- as.numeric(num) / 100
  return(num[ind])
}

Th <- matrix(NA, nrow = 64, ncol = 6)

Th[,1] <- extractTheta(Thraw$R64)
Th[,2] <- extractTheta(Thraw$R32)
Th[,3] <- extractTheta(Thraw$S16)
Th[,4] <- extractTheta(Thraw$E8)
Th[,5] <- extractTheta(Thraw$F4)
Th[,6] <- extractTheta(Thraw$NCG)

apply(Th, 2, sum)

Bstruct <- matrix(NA, nrow = 64, ncol = 6)
Bstruct[,6] <- 1
Bstruct[,5] <- rep(2:(4-1), each = 32)
Bstruct[,4] <- rep(4:(8-1), each = 16)
Bstruct[,3] <- rep(8:(16-1), each = 8)
Bstruct[,2] <- rep(16:(32-1), each = 4)
Bstruct[,1] <- rep(32:(64-1), each = 2)

## bracket structure

BstuctUnique <- sort(unique(Bstruct))

## create a listing of the game groupings so the "which" search only needs to done once

structureList <- vector("list", length = length(BstuctUnique))
for (i in seq_along(BstuctUnique))
{
  structureList[[i]] <- which(Bstruct == BstuctUnique[i], arr.ind = TRUE)
}

P <- normalizeBracket(structureList, P)
Th <- normalizeBracket(structureList, Th)

checkBracket(structureList, P)
checkBracket(structureList, Th)

save(P, Th, structureList, Bstruct, BstuctUnique, trans,
     file = "2021BracketMetadata.Rdata")

