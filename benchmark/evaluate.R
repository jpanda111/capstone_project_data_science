require(digest)
require(stringi)
require(data.table)
require(tm)
require(stringr)

removePattern <- function(x, pattern, replace= "") {
  gsub(pattern, replace, x)
}

startParallelProcessing <- function() {
  noofCores <- detectCores() - 1
  cluster <- makeCluster(noofCores)
  registerDoParallel(cluster)
  cluster
}

stopParallelProcessing <- function(cluster) {
  stopCluster(cluster)
  stopImplicitCluster()
}

set.seed(12345)

abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
relative_path <- "data/sample_25_trim4/"
full_path <- paste(abs_path, relative_path, sep="")

ngram_stupid_backoff_sql <- function(raw, m=3, full_path) {

  if (raw=="") {
    return (c("the","on","a"))
  }
  
  db <- dbConnect(SQLite(), dbname=paste(full_path,"train_stupid_back_off.db", sep=""))
  max = m-1
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  matched <- c()
  
  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence,i), collapse = "_")
    sql <- paste("SELECT word FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", i+1, " LIMIT 3", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- dbFetch(res, n=-1)
    dbClearResult(res)
    matched <- c(matched, predicted$word)
    matched <- unique(matched)
    l <- length(matched)
    if(l>=3) {
      return(matched)
    }
  }
  dbDisconnect(db)
  matched <- c(matched, c("the","on","a"))
  matched <- unique(matched)
  return(matched[1:3])
}

ngram_stupid_backoff_sql_while <- function(raw, m=3, full_path) {
  
  if (raw=="") {
    return (c("the","on","a"))
  }
  
  db <- dbConnect(SQLite(), dbname=paste(full_path,"train_stupid_back_off.db", sep=""))
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  matched <- c()
  
  max = min(length(sentence), m-1)
  while (max >= 1) {
    gram <- paste(tail(sentence, max), collapse = "_")
    sql <- paste("SELECT word FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", max+1, " LIMIT 3", sep="")
    res <- dbSendQuery(conn=db, sql)
    predicted <- dbFetch(res, n=-1)
    dbClearResult(res)
    matched <- c(matched, predicted$word)
    if (length(unique(matched)>=3)) {
      return(matched)
    } else {max = max-1}
  }
  dbDisconnect(db)
  matched <- c(matched, c("the","on","a"))
  matched <- unique(matched)
  return(matched[1:3])
}

ngram_stupid_backoff_dt <- function(raw, m=3, full_path) {

  if (raw=="") {
    return (c("the","on","a"))
  }
  
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  matched <- c()
  max = min(length(sentence), m-1)
  
  for (i in max:1) {
    
    prefix <- paste(tail(sentence, i), collapse = "_")
    g5 <- readRDS(paste(full_path,"gram",(i+1),".scoretop5.rds", sep=""))
    predicted <- g5[w==prefix]$pred
    matched <- c(matched, predicted)  
    matched <- unique(matched)
    if (length(matched)>=3) {
      return(matched[1:3])
    }   
  }
  matched <- c(matched, c("the","on","a"))
  matched <- unique(matched)
  return(matched[1:3])
}

ngram_katz_backoff_sql <- function(raw, full_path) {
  
  # this prediction model only works for trigram!!!! So m is 3 always!!
  if (raw=="") {
    return (c("the","on","a"))
  } 
    
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  l <- length(sentence)

  getfromBi <- function(gram) {
    print("start from bigram!")
    # input only one word or not found in trigram at all, start from bigram instead of trigram then.
    matched <- data.table()
    db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off_bigram.db", sep=""))
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
      print("start from bigram and back off to unigram!")
      # not found at all in bigram
      if (nrow(matched)==0) return(c("the","on","a"))
      else {
        # total less than 3 in bigram
        beta_leftover <- predicted$leftover[1]
        pred_in2gram <- predicted$word
        db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off_unigram.db", sep=""))
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
        } else return(c("the","on","a"))
      }
    }
  }
  getRemainFromBi <- function(gram, matched, beta_leftover, pred_in3gram) {
    # total less than 3 in trigram
    print("start from trigram and back off to bigram!")
    db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off_bigram.db", sep=""))
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
      #not found or total less than 3 in trigram & bigram
      print("start from trigram and back off to unigram!")
      db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off_unigram.db", sep=""))
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
      } else return(c("the","on","a"))
    }
  } 
  
  
  if (l==1) {
    gram <- paste(tail(sentence,1), collapse = "_")
    getfromBi(gram)
  } else {
    print("start from trigram!")
    gram <- paste(tail(sentence,2), collapse = "_")
    print(gram)
    db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off_trigram.db", sep="")) 
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
      print(gram)
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
