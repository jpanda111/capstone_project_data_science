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
relative_path <- "data/sample_25_trim/"
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

ngram_katz_backoff_sql <- function(raw, m=3, relative_path) {
  
  # this prediction model only works for trigram!!!!
  if (raw=="") {
    return (c("the","on","a"))
  } 
    
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  db <- dbConnect(SQLite(), dbname=paste(full_path,"train_katz_back_off.db", sep="")) 
  l <- length(sentence)
  matched <- c()
  
  if (l==1) {
    gram=sentence
  } else {
    
  }
   
  
  
  
  
  
  leftover_prob <- readRDS(paste(full_path, 'gram',max+1,".alpha.rds",sep=""))
  
  for (i in min(length(sentence), max):1) {
    gram <- paste(tail(sentence,i), collapse = "_")
    sql <- paste("SELECT word, freq, d FROM NGRAM WHERE",
                 " pre=='", paste(gram), "'",
                 " AND n==", i+1, " LIMIT 3", sep="")
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
    l <- length(matched)
    if(l>=3) {
      return(matched$Next_Possible_Word[1:3,])
    }
  }
  dbClearResult(res)
  dbDisconnect(db)
  matched <- rbind(matched, unigram)
  return(matched$Next_Possible_Word[1:3])
}