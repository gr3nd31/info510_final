#This part grabs and opens the file, storign the counts in 'counts'
args <- commandArgs(trailingOnly = TRUE)
counts <- read.table(args[1], header = TRUE, sep = "\t", row.names = 1)

#This part divides each column by the upper quartile value (ignoring 0 values)
counts_q <- sapply(counts, function(x){x/quantile(x[x != 0])[4]})

#This part saves it as a file
write.table(counts_q, file = paste0(args[1], "_quartiled.txt"))