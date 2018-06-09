library(RSQLite)
library(stringr)
library(quanteda)
library(tm)

removePattern <- function(x, pattern, replace= "") {
  gsub(pattern, replace, x)
}

ngram_stupid_backoff <- function(raw, m=3, db) {
  max = m-1
  abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
  
  if (raw == "") { 
    g <- readRDS(paste(abs_path,"/data/sample/scoretop5.gram1.rds", sep=""))
    return (g[1:3]$pred)
    }
  #repeat pre-processing same as train data here
  sentence <- iconv(raw, "latin1", "ASCII", sub="")
  test1 <- removePattern(sentence, "\\d+")
  test1 <- removePattern(test1, "_+")
  test1 <- removePattern(test1, ":", " ")
  test1 <- removePattern(test1, "\\.", " ")
  test1 <- removePattern(test1, "\\s([b-hj-z])\\1{0,}\\b"," ")
  test1 <- removePattern(test1, "([[:alpha:]])\\1{2,}", "\\1")
  sentence <- removePattern(test1, "\\s([a-z])\\1{1,2}('(s|ve|d|re))?"," ")
  
  sentence <- tolower(sentence) %>% 
    removePunctuation %>% 
    removeNumbers %>% 
    stripWhitespace %>%
    str_trim %>% # remove whitespace from start and end of string.
    strsplit(split= " ") %>%
    unlist # change type from list to character
  
  matched <- data.frame()
  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence,i), collapse = "_")
    print(gram)
    sql <- paste("SELECT word, score FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", i+1, " LIMIT 5", sep="")
    print(sql)
    res <- dbSendQuery(conn=db, sql)
    predicted <- dbFetch(res, n=-1)
    names(predicted) <- c("Next_Possible_Word", "Score (Adjusted Freq)")
    matched <- rbind(matched, predicted)
    matched <- matched[!duplicated(matched$Next_Possible_Word),]
    l <- nrow(matched)
    if(l>=3) {
      return(matched[1:3,])
    }
  }
  return("Sorry! Cannot Find it!")
}

ngram_katz_back_off <- function(raw, m=3, db) {
  # tri-gram models only
  max = m-1
  #repeat pre-processing same as train data here
  sentence <- iconv(raw, "latin1", "ASCII", sub="")
  test1 <- removePattern(sentence, "\\d+")
  test1 <- removePattern(test1, "_+")
  test1 <- removePattern(test1, ":", " ")
  test1 <- removePattern(test1, "\\.", " ")
  test1 <- removePattern(test1, "\\s([b-hj-z])\\1{0,}\\b"," ")
  test1 <- removePattern(test1, "([[:alpha:]])\\1{2,}", "\\1")
  sentence <- removePattern(test1, "\\s([a-z])\\1{1,2}('(s|ve|d|re))?"," ")
  
  sentence <- tolower(sentence) %>% 
    removePunctuation %>% 
    removeNumbers %>% 
    stripWhitespace %>%
    str_trim %>% # remove whitespace from start and end of string.
    strsplit(split= " ") %>%
    unlist # change type from list to character
  
  matched <- data.frame()
  abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
  leftover_prob <- readRDS(paste('/data/sample/leftover_prob.gram',max+1,".rds",sep=""))

  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence,i), collapse = "_")
    print(gram)
    sql <- paste("SELECT word, freq, d FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", i+1, " LIMIT 5", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- data.table(dbFetch(res, n=-1))
    if (i==max) {
      leftover_prob <- leftover_prob[w==gram]$leftprob[1]
      predicted <- predicted[,.(word,prob=freq*d/sum(freq))]
      max_pred <- predicted$word
    } else {
      remain <- predicted[!(word %in% max_pred)]
      all_freq = sum(predicted$freq)
      alpha = leftover_prob / sum(remain$freq*remain$d/all_freq)
      predicted <- predicted[,.(word, prob=alpha*freq*d/all_freq)]
    }
    names(predicted) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
    matched <- rbind(matched, predicted)
    setorder(matched, -Prob_with_Good_Turing_Smoothing)
    matched <- matched[!duplicated(matched$Next_Possible_Word),]
    l <- nrow(matched)
    if(l>=3) {
      return(matched[1:3,])
    }
  }
  return("Sorry! Cannot Find it!")
}