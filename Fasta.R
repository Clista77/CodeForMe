install.packages("BiocManager")
BiocManager::install("Biostrings")
BiocManager::install("DECIPHER")
library(Biostrings)
library(DECIPHER)

fasta=readDNAStringSet('sequence.fasta')
writeXStringSet(fasta,'sample.fasta')