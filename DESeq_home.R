args <- commandArgs(trailingOnly = TRUE)
counts <- read.table(args[1], header = TRUE, sep = "\t", row.names = 1)

counts_no_zeroes <- counts
ls <- c()

for (i in 1:dim(counts_no_zeroes)[1]){
  if (sum(counts[i,] == 0)){
    ls <- append(ls, i)
  }
}
counts_no_zeroes <- counts[-ls, ]

#To make life easy, I used the geoMean() function in the EnvStats package
library(EnvStats)

#First, we assign holder values for the data
counts_geo_1 <- counts_no_zeroes
counts_geo_2 <- counts_no_zeroes

#Then we generate a vector of geometric means of each row/gene
pseudo <- apply(counts_no_zeroes, MARGIN = 1, function(x){geoMean(x[x != 0])})

#Then we divide each value by the geoMean of its respective row
for (i in 1:dim(counts_no_zeroes)[1]){
  counts_geo_1[i,] <- counts_no_zeroes[i,]/pseudo[i]
}

#Now we get a vector of the medians of each dataset
pseudo_2 <- apply(counts_geo_1, MARGIN = 2, function(x){median(x[x != 0])})

#Now we divide each value by the normalizing factor of that dataset
for (i in 1:dim(counts_no_zeroes)[2]){
  counts_geo_2[,i] <- counts_no_zeroes[,i]/pseudo_2[i]
}

write.table(counts_geo_2, file = paste0(args[1], "_DESeq_count.txt"))