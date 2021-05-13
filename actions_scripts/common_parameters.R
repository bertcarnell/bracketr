# simulation parameters
n_truth_brackets <- 10000
n_adversaries <- 20

# genetic algorithm parameters
pRate <- 3
nchildren <- 50
ngenerations <- 50
gen_method <- 2

# common simulation data
set.seed(193849)

X <- drawTruthSampleList(P, structureList, n_truth_brackets)
checkTruthSampleList(X, structureList)

Y <- drawAdversarySampleList(Th, structureList, n_truth_brackets, n_adversaries)
checkAdversarySampleList(Y, structureList, n_truth_brackets, n_adversaries)

Yscore <- scoreAdversarySampleList(Y, X, n_truth_brackets)

