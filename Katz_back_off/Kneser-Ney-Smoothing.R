# download data

unigram <- read.csv("unigram.csv",
                    header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE) 
bigram <- read.csv("bigram.csv", 
                   header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE)
trigram <- read.csv("trigram.csv", 
                    header=FALSE, stringsAsFactors=FALSE,strip.white =TRUE)

# put columns names, delete rows names
colnames(unigram)= c("word","freq")
rownames(unigram) <- NULL
colnames(bigram)= c("word","freq")
rownames(bigram) <- NULL
colnames(trigram)= c("word","freq")
rownames(trigram) <- NULL

# unigram Pkn
query = unigram$word
unigram$Pkn <-unigram$freq[which(unigram$word == query)]/(sum(unigram$freq)-3)
unigram$Pkn[1]<- 0 

# bigram Pkn

bigram$Pkn <- 0
for (i in 1:nrow(bigram)){
  query2 = bigram$word [i]
  A <- bigram$freq[which(bigram$word == query2)]
  query1= unlist(strsplit(query2, " "))[1]
  B <- unigram$freq[which(unigram$word == query1)]
  N <- tabulate(bigram$freq)
  D <- N[1]/ (N[1] +2*N[2])
  N1plus <-  sum(tabulate(bigram$freq[grep(paste("^",query1,sep=""),bigram$word)]))
  query3 <- unlist(strsplit(query2, " "))[2]
  N1plus2 <- sum(tabulate(bigram$freq[grep(paste(query3,"$",sep=""),bigram$word)]))
  N1plus3 <- sum(N)
  Pkn <- max(A-D,0)/ B + D/B * N1plus*N1plus2/N1plus3
  bigram$Pkn[i] <- Pkn
}

# trigram Pkn 

trigram$Pkn <- 0
for (i in 1:nrow(trigram)){
  query3 = trigram$word [i]
  A <- trigram$freq[which(trigram$word == query3)]
  query2 <- paste(unlist(strsplit(query3, " "))[1],unlist(strsplit(query3, " "))[2])
  B <- bigram$freq[which(bigram$word == query2)]
  N <- tabulate(trigram$freq)
  D <- N[1]/ (N[1] +2*N[2])
  N1plus <- sum(tabulate(trigram$freq[grep(paste("^",query2,sep=""),trigram$word)]))
  query1 <- unlist(strsplit(query3, " "))[2]
  query2 <- paste(query1,unlist(strsplit(query3, " "))[3])
  query4 <- unlist(strsplit(query3, " "))[3]
  Aa <- bigram$freq[which(bigram$word == query2)]
  Bb <- unigram$freq[which(unigram$word == query1)]
  Nn <- tabulate(bigram$freq)
  Dd <- Nn[1]/ (Nn[1] +2*Nn[2])
  Nn1plus <-  sum(tabulate(bigram$freq[grep(paste("^",query1,sep=""),bigram$word)]))
  Nn1plus2 <- sum(tabulate(bigram$freq[grep(paste(query4,"$",sep=""),bigram$word)]))
  Nn1plus3 <- sum(Nn)
  Pkn <-  max(A-D,0)/ B + (D/B) * N1plus *(max(Aa-Dd,0)/ Bb + (Dd/Bb * Nn1plus*Nn1plus2/Nn1plus3)) 
  trigram$Pkn[i] <- Pkn
}  