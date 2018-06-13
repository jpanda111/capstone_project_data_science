library(RSQLite)
library(stringr)
library(quanteda)
library(tm)
library(data.table)

removePattern <- function(x, pattern, replace= "") {
  gsub(pattern, replace, x)
}

preprocess_data <- function(raw) {
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
  sentence
}

ngram_stupid_backoff_sql <- function(raw, m=3, db) {
  
  uniword <- c("the","on","a")
  
  
  if (raw=="") {
    return (uniword)
  }
  
  max = m-1
  # repeat pre-processing same as train data here
  sentence <- preprocess_data(raw)
  matched <- data.table()
  
  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence,i), collapse = "_")
    sql <- paste("SELECT word, score FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", i+1, " LIMIT 3", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- data.table(dbFetch(res, n=-1))
    dbClearResult(res)
    matched <- rbind(matched, predicted)
    matched <- matched[!duplicated(matched$word),]
    l <- nrow(matched)
    if(l>=3) {
      names(matched) <- c("Next_Possible_Word", "Score")
      return(matched[1:3])
    }
  }
  sql <- "SELECT word, score FROM NGRAM WHERE n==1 LIMIT 3"
  res <- dbSendQuery(conn=db, sql)
  predicted <- data.table(dbFetch(res, n=-1))
  dbClearResult(res)
  matched <- rbind(matched, predicted)
  matched <- matched[!duplicated(matched$word),]
  names(matched) <- c("Next_Possible_Word", "Score")
  return(matched[1:3])
}

ngram_katz_backoff_sql <- function(raw) {
  
  # this prediction model only works for trigram!!!! So m is 3 always!!
  if (raw=="") {
    return (c("the","to","and"))
  } 
  
  sentence <- preprocess_data(raw) # change type from list to character
  l <- length(sentence)
  
  getfromBi <- function(gram) {
    # input only one word or not found in trigram at all, start from bigram instead of trigram then.
    matched <- data.table()
    db <- dbConnect(SQLite(), dbname="train_katz_back_off_bigram.db")
    sql <- paste("SELECT word, prob, freq, d, leftover FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- data.table(dbFetch(res, n=-1))
    matched <- predicted[,1:2]
    dbClearResult(res)
    dbDisconnect(db)
    if (nrow(matched)>= 3) {
      names(matched) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
      return(matched[1:3])
    } else {
      # not found at all in bigram
      if (nrow(matched)==0) return(c("the","to","and"))
      else {
        # total less than 3 in bigram
        beta_leftover <- predicted$leftover[1]
        pred_in2gram <- predicted$word
        db <- dbConnect(SQLite(), dbname="train_katz_back_off_unigram.db")
        sql <- "SELECT word, prob, freq, d FROM NGRAM "
        res <- dbSendQuery(conn=db, sql)
        predicted <- data.table(dbFetch(res, n=-1))
        remain = predicted[!(word %in% pred_in2gram)]
        all_freq <- sum(predicted$freq)
        alpha <- beta_leftover/sum((remain$freq*remain$d)/all_freq)
        remain[,prob:=prob*alpha]
        matched <- rbind(matched, remain[,1:2])
        dbClearResult(res)
        dbDisconnect(db)
        if (nrow(matched)>=3) {
          names(matched) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
          return(matched[1:3])
        } else return(c("the","to","and"))
      }
    }
  }
  getRemainFromBi <- function(gram, matched, beta_leftover, pred_in3gram) {
    # total less than 3 in trigram
    db <- dbConnect(SQLite(), dbname="train_katz_back_off_bigram.db")
    sql <- paste("SELECT word, prob, freq, d, leftover FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- data.table(dbFetch(res, n=-1))
    remain = predicted[!(word %in% pred_in3gram)]
    all_freq <- sum(predicted$freq) 
    alpha <- beta_leftover/sum((remain$freq*remain$d)/all_freq)
    remain[,prob:=prob*alpha]
    matched <- rbind(matched, remain[,1:2])
    dbClearResult(res)
    dbDisconnect(db)
    if (nrow(matched)>=3) {
      names(matched) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
      return(matched[1:3])
    } else {
      # not found or total less than 3 in trigram & bigram
      db <- dbConnect(SQLite(), dbname="train_katz_back_off_unigram.db")
      sql <- "SELECT word, prob, freq, d FROM NGRAM "
      res <- dbSendQuery(conn=db, sql)
      predicted <- data.table(dbFetch(res, n=-1))
      remain = predicted[!(word %in% pred_in3gram)]
      all_freq <- sum(predicted$freq)
      alpha <- beta_leftover/sum((remain$freq*remain$d)/all_freq)
      remain[,prob:=prob*alpha]
      matched <- rbind(matched, remain[,1:2])
      dbClearResult(res)
      dbDisconnect(db)
      if (nrow(matched)>=3) {
        names(matched) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
        return(matched[1:3])
      } else return(c("the","to","and"))
    }
  } 
  
  
  if (l==1) {
    gram <- paste(tail(sentence,1), collapse = "_")
    getfromBi(gram)
  } else {
    gram <- paste(tail(sentence,2), collapse = "_")
    db <- dbConnect(SQLite(), dbname="train_katz_back_off_trigram.db") 
    sql <- paste("SELECT word, prob, leftover FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " LIMIT 3", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- data.table(dbFetch(res, n=-1))
    matched <- predicted[,1:2]
    dbClearResult(res)
    dbDisconnect(db)
    if (nrow(matched)>= 3) {
      names(matched) <- c("Next_Possible_Word", "Prob_with_Good_Turing_Smoothing")
      return(matched[1:3])
    } else {
      gram <- paste(tail(sentence,1), collapse = "_")
      if (nrow(matched) == 0) {
        getfromBi(gram)
      } else {
        beta_leftover <- predicted$leftover[1]
        pred_in3gram <- predicted$word
        getRemainFromBi(gram,matched, beta_leftover, pred_in3gram)
      }
    }
  }
}