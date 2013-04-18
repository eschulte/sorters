raw <- read.csv("../results/heritability/offspring.csv")

# Standardize traits
std=function(x){(x-mean(x))/sd(x)}

# Standardize results for each row
cycles<-std(raw$cycles)
instructions<-std(raw$instructions)
cache<-std(raw$cache)
pageFaults<-std(raw$pageFaults)

# new variable for the standardized results
offspring<-cbind(cycles,instructions,cache,pageFaults)

# offspring
cov(offspring,method="pearson")
