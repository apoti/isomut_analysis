normalizeGenomicPositions <- function(chr, pos, genome, which.chroms) {
  seqNames <- seqnames(genome)[which.chroms]
  seqLengths <-  seqlengths(genome)[which.chroms]
  
  chr <- paste0("chr", chr)
  pos <- as.numeric(pos)
  whichChrom <- which(seqNames %in% chr)
  earlierChromsLength <- sum(seqLengths[0:(whichChrom - 1)])
  return(as.numeric(earlierChromsLength) + pos)
}


delClassifier <- function(deleted, upstream) {
  if (length(deleted) != length(upstream)) {
    print("Sequence vectors have to be of equal length!")
  }
  ret <- vector(length = length(deleted))
  deleted <- as.character(deleted)
  upstream <- as.character(upstream)
  for (i in 1:length(deleted)) {
    ret[i] <- ifelse(
      substr(upstream[i], 1, nchar(deleted[i])) == deleted[i],
      "repeat",
      ifelse(
        substr(upstream[i], 1, 1) == substr(deleted[i], 1, 1),
        "homology",
        "no homology"
      )
    )
  }
  return(ret)
}
