require(digest)
require(stringi)
require(data.table)
require(tm)
require(stringr)
require(RSQLite)

removePattern <- function(x, pattern, replace= "") {
  gsub(pattern, replace, x)
}
################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################

abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
relative_path <- "benchmark/dsci-benchmark-master/"
full_path <- paste(abs_path, relative_path, sep="")

# 01b. Get text from randomly selected tweets
################################################################################################

tweets <- readLines(paste(full_path, 'data/tweets.txt',sep=""), encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
  "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"
sentence <- iconv(tweets, "latin1", "ASCII", sub="")
sentence <- tolower(sentence)
sentence <- removePattern(sentence, "http(s)?:.*\\b")
sentence <- removePattern(sentence, "[[:punct:]]", " ")
sentence <- removeNumbers(sentence)
sentence <- stripWhitespace(sentence)
tweets <- str_trim(sentence) # remove whitespace from start and end of string.

tweets <- tweets[1]
#sentence <- strsplit(sentence, split= " ")
#sentence <- unlist(sentence) # change type from list to character

# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
blogs <- readLines(paste(full_path, 'data/blogs.txt',sep=""), encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(blogs, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
  "14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2"
sentence <- iconv(blogs, "latin1", "ASCII", sub="")
sentence <- tolower(sentence)
sentence <- removePattern(sentence, "http(s)?:.*\\b")
sentence <- removePattern(sentence, "\\.\\.\\.")
sentence <- removePattern(sentence, "[[:punct:]]", " ")
sentence <- removeNumbers(sentence)
sentence <- stripWhitespace(sentence)
blogs <- str_trim(sentence)

blogs <- blogs[1]
################################################################################################
#
# 02. Define the functions used for benchmarking
#
################################################################################################

# 02a. Pre-processing functions
################################################################################################

# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
    require(stringi)
    # append a space to the sentence (to make sure we always create one result with only the 
    # last word missing)
    sent <- paste0(line, ' ')

    sep <- stri_locate_all_regex(line, 
                                 pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                 omit_empty=T, 
                                 case_insensitive=T)[[1]]
    sapply(seq_len(nrow(sep)), 
           function(i) {
               c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                    nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
               )
               })
}, options=list(optimize=3))


# 02b. Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.
benchmark <- compiler::cmpfun(function(FUN, ..., sent.list, ext.output=T) {
  require(stringi)
  require(digest)
  require(data.table)
  
  result <- rbindlist(lapply(names(sent.list), 
                             function(list.name) {  
                               sentences <- sent.list[[list.name]]
                               score <- 0
                               max.score <-0
                               hit.count.top3 <- 0
                               hit.count.top1 <- 0
                               total.count <- 0
                               time <- system.time({
                                 for (sent in sentences) {
                                   split <- split.sentence(sent[1])
                                   max.score <- max.score + ncol(split)*3
                                   total.count <- total.count + ncol(split)
                                   rank <- sapply(seq_len(ncol(split)),
                                                  function(i) {
                                                    min(which(FUN(split[1,i], ...)==split[2,i]),4)
                                                  })
                                   score <- score + sum(4-rank)
                                   hit.count.top3 <- hit.count.top3 + sum(rank<4)
                                   hit.count.top1 <- hit.count.top1 + sum(rank==1)
                                 }
                               })
                               
                               list('list.name' = list.name,
                                    'line.count' = length(sentences),
                                    'word.count' = sum(stri_count_words(sentences)),
                                    'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                                    'score' = score,
                                    'max.score' = max.score,
                                    'hit.count.top3' = hit.count.top3,
                                    'hit.count.top1' = hit.count.top1,
                                    'total.count' = total.count,
                                    'total.runtime' = time[3]
                               )               
                             }), use.names=T) #result
  
  setkey(result, list.name)
  
  # The overall scores are calculated weighting each data set equally (independent of the 
  # number of lines in each dataset).
  overall.score.percent = 100 * result[,sum(score/max.score)/.N]
  overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
  overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
  average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
  number.of.predictions = result[,sum(total.count)]
  total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                     function(x) {
                                       object.size(get(x,
                                                       envir = .GlobalEnv,
                                                       inherits = FALSE))
                                     })))/(1024^2)
  cat(sprintf(paste0('Overall top-3 score:     %.2f %%\n',
                     'Overall top-1 precision: %.2f %%\n',
                     'Overall top-3 precision: %.2f %%\n',
                     'Average runtime:         %.2f msec\n',
                     'Number of predictions:   %d\n',
                     'Total memory used:       %.2f MB\n'),
              overall.score.percent,
              overall.precision.top1,
              overall.precision.top3,
              average.runtime,
              number.of.predictions,
              total.mem.used
  ))
  
  cat('\nDataset details\n')
  for (p.list.name in result$list.name) {
    res <- result[list(p.list.name)]
    cat(sprintf(paste0(' Dataset "%s" (%d lines, %d words, hash %s)\n',
                       '  Score: %.2f %%, Top-1 precision: %.2f %%, Top-3 precision: %.2f %%\n'
    ),
    p.list.name,
    res$line.count,
    res$word.count,
    res$hash,
    100 * res$score/res$max.score,
    100 * res$hit.count.top1/res$total.count,
    100 * res$hit.count.top3/res$total.count
    ))
  }
  
  if (ext.output==T) {
    packages <- sort(stri_replace_first_fixed(search()[stri_detect_regex(search(), 
                                                                         '^package:')], 
                                              'package:', ''))
    
    cat(sprintf(paste0('\n\n%s, platform %s\n', 
                       'Attached non-base packages:   %s\n',
                       'Unattached non-base packages: %s'
    ),
    sessionInfo()$R.version$version.string,
    sessionInfo()$platform,
    paste0(sapply(sessionInfo()$otherPkgs, 
                  function(pkg) {
                    paste0(pkg$Package, ' (v', pkg$Version, ')')
                  }), 
           collapse = ', '),
    paste0(sapply(sessionInfo()$loadedOnly, 
                  function(pkg) { 
                    paste0(pkg$Package, ' (v', pkg$Version, ')')
                  }), 
           collapse = ', ')
    ))
  }
}, options=list(optimize =3))


################################################################################################
#
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
predict.baseline <- function(x){c('the', 'on', 'a')}

ngram_stupid_backoff_dt <- function(raw, m=3, full_path) {
  
  if (raw=="") {
    return (c("the","on","a"))
  }
  
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  matched <- c()
  g2 <- readRDS(paste(full_path,"gram2.trim5.scoretop5.rds", sep=""))
  getFromBi <- function(prefix, g2) {
    setkey(g2,w)
    predicted <- g2[w==prefix]$pred
    matched <- c(matched, predicted)
    if (length(matched)>=3) {
      return(matched[1:3])
    } else {
      matched <- c(matched, c("the","on","a"))
      matched <- unique(matched)
      return(matched[1:3])
    }
  }
  
  getRemainFromBi <- function(prefix, g2) {
    predicted <- g2[w==prefix]$pred
    matched <- c(matched, predicted)
    matched <- unique(matched)
    if (length(matched)>=3) {
      return(matched[1:3])
    } else {
      matched <- c(matched, c("the","on","a"))
      matched <- unique(matched)
      return(matched[1:3])
    }
  }
  
  if (length(sentence)==1) {
    prefix=sentence
    getFromBi(prefix, g2)
    } else {
      g3 <- readRDS(paste(full_path,"gram3.trim5.scoretop5.rds", sep=""))
      setkey(g3,w)
      prefix <- paste(tail(sentence, 2), collapse = "_")
      predicted <- g3[w==prefix]$pred
      matched <- c(matched, predicted)
      matched <- unique(matched)
      if (length(matched)>=3) {
        return(matched[1:3])
      } else {
        prefix <- paste(tail(sentence, 1), collapse = "_")
        getRemainFromBi(prefix, g2)
      }
    }
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
ngram_katz_backoff_sql <- function(raw, full_path) {
  
  # this prediction model only works for trigram!!!! So m is 3 always!!
  if (raw=="") {
    return (c("the","on","a"))
  } 
  
  sentence <- strsplit(raw, split= " ")
  sentence <- unlist(sentence) # change type from list to character
  l <- length(sentence)
  
  getfromBi <- function(gram) {
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
      # not found or total less than 3 in trigram & bigram
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
    gram <- paste(tail(sentence,2), collapse = "_")
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

################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
print(paste("START TIME:", Sys.time()))
benchmark(predict.baseline,
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets,
                           'blogs' = blogs),
          ext.output = T)
print(paste("END TIME:", Sys.time()))

################################################################################################
# sampling 15% with 3-grams model and freq > 1, directly using data table
################################################################################################

abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
 
# relative_path <- "data/sample_trim4/"
# full_path <- paste(abs_path, relative_path, sep="")
# 
# print("3-gram model, freq > 4, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))
# 
# print("4-gram model, freq > 4, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=4, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))
# 
# print("5-gram model, freq > 4, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=5, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))

relative_path <- "data/sample_25_trim4/"
full_path <- paste(abs_path, relative_path, sep="")

print("3-gram model, freq > 4, sql, 25% sampling")
print(paste("START TIME:", Sys.time()))
benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
          sent.list = list('tweets' = tweets,
                           'blogs' = blogs),
          ext.output = T)
print(paste("END TIME:", Sys.time()))

print("3-gram model, freq > 1, sql, katz, 25% sampling")
print(paste("START TIME:", Sys.time()))
benchmark(ngram_katz_backoff_sql, full_path=full_path,
          sent.list = list('tweets' = tweets,
                           'blogs' = blogs),
          ext.output = T)
print(paste("END TIME:", Sys.time()))

print("3-gram model, freq > 4, data table, 25% sampling")
print(paste("START TIME:", Sys.time()))
benchmark(ngram_stupid_backoff_dt, m=3, full_path=full_path,
          sent.list = list('tweets' = tweets,
                           'blogs' = blogs),
          ext.output = T)
print(paste("END TIME:", Sys.time()))
# print("4-gram model, freq > 4, sql, 25% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=4, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# print("5-gram model, freq > 4, sql, 25% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=5, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))

# relative_path <- "data/sample_50_trim4/"
# full_path <- paste(abs_path, relative_path, sep="")
# 
# print("3-gram model, freq > 4, sql, 50% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# print("4-gram model, freq > 4, sql, 50% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=4, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# print("5-gram model, freq > 4, sql, 50% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=5, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))


# relative_path <- "data/sample/"
# full_path <- paste(abs_path, relative_path, sep="")
# 
# print("3-gram model, freq >0, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))

# 
# relative_path <- "data/sample_25_trim/"
# full_path <- paste(abs_path, relative_path, sep="")
# 
# print("3-gram model, freq >1, sql, 25% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# print("4-gram model, freq >1, sql, 25% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=4, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# print("5-gram model, freq >1, sql, 25% sampling")
# print(paste("START TIME:", Sys.time()))
# benchmark(ngram_stupid_backoff_sql, m=5, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# print(paste("END TIME:", Sys.time()))
# 
# relative_path <- "data/sample_trim/"
# full_path <- paste(abs_path, relative_path, sep="")
# 
# print("3-gram model, freq >1, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=3, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))
# 
# print("4-gram model, freq >1, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=4, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))
# 
# print("5-gram model, freq >1, sql, 15% sampling")
# print(paste("START TIME:", Sys.time()))
# 
# benchmark(ngram_stupid_backoff_sql, m=5, full_path=full_path,
#           sent.list = list('tweets' = tweets,
#                            'blogs' = blogs),
#           ext.output = T)
# 
# print(paste("END TIME:", Sys.time()))
#