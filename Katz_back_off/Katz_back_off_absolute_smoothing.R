ltcorpus <- readLines("little_test_corpus1.txt")

library(quanteda)

getNgramFreqs <- function(ng, dat, ignores=NULL,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
  if (is.null(ignores)) {
    dat.dfm <- dfm(dat, ngrams=ng, tolower=FALSE,
                   what="fasterword", verbose=FALSE)
  } else {
    dat.dfm <- dfm(dat, ngrams=ng, tolower=FALSE, ignoredFeatures = ignores,
                  what="fasterword", verbose=FALSE)
  }
  rm(dat)
  ngram.freq <- docfreq(dat.dfm)
  if (sort.by.freq) { ngram.freq <- sort(ngram.freq, decreasing = TRUE)}
  if (sort.by.ngram) { ngram.freq <- ngram.freq[sort(names(ngram.freq))]}
  rm(dat.dfm)
  return (ngram.freq)
}

getNgramTables <- function(ng, linesCorpus, prefixFilter = NULL) {
  ngrams <- getNgramFreqs(ng, linesCorpus)
  ngrams_dt <- data.table(ngram=names(ngrams), freq=ngrams)
  if(length(grep('^SOS', ngrams_dt$ngram)) > 0) {
    #ngrams_dt <- ngrams_dt[-grep('^SOS', ngrams_dt$ngram),]
  } 
  if (!is.null(prefixFilter)) {
    regex <- sprintf('%s%s', '^', prefixFilter)
    ngrams_dt <- ngrams_dt[grep(regex, ngrams_dt$ngram),]
  }
  return(ngrams_dt)
}

unigram <- getNgramTables(1, ltcorpus)
bigram <- getNgramTables(2, ltcorpus)
trigram <- getNgramTables(3, ltcorpus)

gamma2 <- 0.6 # bigram discount
gamma3 <- 0.6 # trigram discount
bigPre <- "sell_the"
uniPre <- str_split(bigPre, "_")[[1]][2] #"the"

getObsTrigs <- function(bigPre, trigram) {
  trigs.winA <- data.frame(ngrams=vector(mode="character", length=0),
                           freq=vector(mode="integer", length=0))
  regex <- sprintf("%s%s%s","^", bigPre, "_")
  print(regex)
  trigram_indices <- grep(regex, trigram$ngram)
  if(length(trigram_indices) > 0 ) {
    trigs.winA <- trigram[trigram_indices, ]
  }
  return(trigs.winA)
}

getObsTriProbs <- function(obs_trigs, bigram, bigPre, gamma3=0.5) {
  if(nrow(obs_trigs) < 1) return(NULL)
  obsCount <- bigram[ngram==bigPre,freq] # c(wi-2,wi-1) in bigram
  obsTrigProbs <- obs_trigs[, .(ngram, prob=((freq-gamma3)/obsCount))] # c(wi-2,wi-1,wi)-gamma3/c(wi-2,wi-1)
  return(obsTrigProbs)
}

obs_trigs <- getObsTrigs(bigPre, trigram)
print(obs_trigs)
qbo_obs_trigram <- getObsTriProbs(obs_trigs, bigram, bigPre, gamma3)
print(qbo_obs_trigram)

getUnobsTrigTails <- function(obs_trigs, unigram) {
  obs_trig_tails <- str_split_fixed(obs_trigs, "_", 3)[, 3]
  unobs_trig_tails <- unigram[!(unigram$ngram %in% obs_trig_tails), ]$ngram
  return(unobs_trig_tails)
}

unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigram)
print(unobs_trig_tails) # all the uniwords except pred in trigram

getAlphaBigram <- function(unig, bigram, gamma2 = 0.5) {
  obsCount <- unig$freq # c(wi-1) in unigram
  regex <- sprintf("%s%s%s","^",unig$ngram[1], "_") # wi-1_
  print(regex)
  bigsThatStartWithUnig <- bigram[grep(regex, bigram$ngram),] # find all wi-1_* in bigram
  print(bigsThatStartWithUnig)
  if(nrow(bigsThatStartWithUnig)<1) return(0)
  alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - gamma2)/obsCount)
  
  return(alphaBi)
}

getAlphaTrigram <- function(obs_trigs, bigram, gamma3 = 0.5) {
  obsCount <- bigram$freq[1] # c(wi-2, wi-1) in bigram
  if (nrow(obs_trigs) < 1) return (1) # obs_trigs is the all wi-2_wi-1_* in trigram
  alpha_Tri <- 1 - sum((obs_trigs$freq - gamma3) / bigram$freq[1])
  return(alpha_Tri)
}

unig <- unigram[ngram==uniPre,] # c(wi-1) in unigram
print(unig)
alpha_bi <- getAlphaBigram(unig, bigram, gamma2)
print(alpha_bi) # 0.125
big <- bigram[ngram%in%bigPre,] # c(wi-2,wi-1) in bigram
alpha_tri <- getAlphaTrigram(obs_trigs, big, gamma3)
print(alpha_tri) # 0.5

getallBi <- function(uniPre, unobs_trig_tails) {
  all_bigram <- data.table(ngram=paste(uniPre, unobs_trig_tails, sep="_"))
  return (all_bigram)
}

all_bigram <- getallBi(uniPre, unobs_trig_tails)
print(all_bigram)

getObsBOPBigram <- function(all_bigram, bigram) {
  obs_BOP_bigram <- bigram[ngram %in% all_bigram$ngram,]
  return(obs_BOP_bigram)
}

obs_all_bigram <- getObsBOPBigram(all_bigram, bigram)
print(obs_all_bigram)

getUnobsBOPBigram <- function(all_bigram, bigram) {
  unobs_BOP_bigram <- all_bigram[!(ngram %in% bigram$ngram),]
  return(unobs_BOP_bigram)
}

unobs_all_bigram <- getUnobsBOPBigram(all_bigram, obs_all_bigram)
print(unobs_all_bigram)

getObsBigProbs <- function(obs_all_bigram, unigram, gamma2=0.5) {
  first_words <- str_split_fixed(obs_all_bigram$ngram, "_", 2)[, 1]
  print(first_words)
  first_words_freqs <- unigram[ngram %in% first_words,]$freq # C(wi-1) in unigram
  print(first_words_freqs)
  obsBigProbs <- obs_all_bigram[,.(ngram,prob=(freq-gamma2)/first_words_freqs),]
  return(obsBigProbs)
}

qbo_obs_bigrams <- getObsBigProbs(obs_all_bigram, unigram, gamma2)
print(qbo_obs_bigrams)

getUnobsBigProbs <- function(unobs_all_bigram, unigram, alpha_bi) {
  qboUnobsBigs <- str_split_fixed(unobs_all_bigram$ngram,"_",2)[,2]
  #w_in_Aw_iminus1 <- unigram[!(ngram %in% qboUnobsBigs), ]
  qboUnobsBigs <- unigram[ngram %in% qboUnobsBigs,]
  print(qboUnobsBigs)
  denom <- sum(qboUnobsBigs$freq)
  qboUnobsBigs <- data.table(ngram=unobs_all_bigram$ngram, prob=(alpha_bi*qboUnobsBigs$freq/denom))
  return(qboUnobsBigs)
}

qbo_unobs_bigrams <- getUnobsBigProbs(unobs_all_bigram, unigram, alpha_bi)
print(qbo_unobs_bigrams)

qbo_bigram <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
print(qbo_bigram)

unobs <- qbo_bigram[-1,] # remove house to get all the unobserved bigrams
sum(unobs$prob) # all the unobserved bigrams = alpha_bi = 0.125, the discounting we did

getUnobsTriProbs <- function(bigPre, qbo_bigram, alpha_tri) {
  setorder(qbo_bigram,-prob)
  sumqboBi <- sum(qbo_bigram$prob) # alpha_tri-alpha_bi, pure used for bigram, alpha_tri = 1- observ_tri_prob;
  first_bigPre_word <- str_split(bigPre, "_")[[1]][1]
  print(first_bigPre_word)
  unobs_all_trigram <- qbo_bigram[,.(ngram= paste(first_bigPre_word, ngram, sep="_"),
                                     prob = alpha_tri * prob/sum(prob))]
  return(unobs_all_trigram)
}

qbo_unobs_trigram <- getUnobsTriProbs(bigPre, qbo_bigram, alpha_tri)
print(qbo_unobs_trigram)

qbo_trigram <- rbind(qbo_unobs_trigram, qbo_obs_trigram)
setorder(qbo_trigram, -prob)
print(qbo_trigram)

getPredictionMsg <- function(qbo_trigram) {
  prediction <- str_split(qbo_trigram$ngram[1],"_")[[1]][3]
  result <- sprintf("%s%s%s%.4f","highest prob prediction is >>> ", prediction, " <<< which has prob=", qbo_trigram$prob[1])
  return(result)
}

out_msg <- getPredictionMsg(qbo_trigram)
print(out_msg)
