unigramModel <- generateTDM(docs, 1)
bigramModel <- generateTDM(docs, 2)
trigramModel <- generateTDM(docs, 3)
allNgrams <- c(trigramModel, unigramModel, bigramModel)

# The process I used also writes intermediate files. The general approach was:
#   
# 1.Build the corpus
# 2.create ngrams of a specific size
# 3.save the output to an rds file
# 4.delete the ngrams object
# 5.repeat steps 2 - 4 for different sizes of ngrams
# 6.After I created all the ngram files, I then used data.table to process them into base values and predicted values
# 7.using data.table features to summarize by frequency. 
# 8.Then I subset to the top n frequency, and saved this as my database for the Shiny app.

# It seems that performance can vary greatly depending on how one uses Quanteda's functions 
# given that the ngrams can be created with dfm(), tokenize() or ngrams().

# readRDS/saveRDS can restore the single object under a different name

# some commands related to quanteda package
require(quanteda)
require(quanteda.corpora)
require(lubridate)
require(ggplot2)

trigrams <- tokenize(corpus_data, what="word", remove_symbols=TRUE, remove_numbers=TRUE, remove_punct=TRUE, remove_twitter=TRUE, 
                     remove_url=TRUE, remove_separators=TRUE, ngrams=3, concatenator = " ")
trigrams <- dfm(trigrams, remove = profanity)
saveRDS(trigrams, "trigram.rds", ascii=FALSE, compress=TRUE)


wordMatbigr <- dfm(qCorpus, ngrams=2, remove_symbols=TRUE, remove_numbers=TRUE, remove_punct=TRUE, remove_twitter=TRUE, 
                   remove_url=TRUE, remove_separators=TRUE, remove_hyphens=TRUE)
ndoc(wordMatbigr) # how many documents or sentences in this data
nfeat(wordMatbigr) # how many tokens/words 
head(docnames(wordMatbigr),10)
head(featnames(wordMatbigr),10)
head(rowSums(wordMatbigr),10) # sum # of words for each documents or sentences
head(colSums(wordMatbigr),10) # sum frequency of each word
topfeatures(wordMatbigr, 10) # showing highest frequency of words
prop_wordMatbigr <- dfm_weight(wordMatbigr,scheme="prop")
topfeatures(prop_wordMatbigr[1,]) # showing highest proportion of words
wordMatbigr <- dfm_trim(wordMatbigr, min_count = 4)
wordVectorBigr <- sort(colSums(wordMatbigr), decreasing = TRUE)
nostop_wordMatbigr <- dfm_remove(wordMatbigr, stopwords("en"))
# You can generate n-grams in any lengths from a tokens using tokens_ngrams()
ngram <- tokens_ngrams(toks, n = 2:4)
head(ngram[[1]], 50)
# nostop_wordMatbigr <- dfm_select(wordMatbigr, stopwords("en"), selection="remove")
freq_wordMatbigr <- dfm_trim(wordMatbigr, min_termfreq = 4)
require(quanteda.corpora)
news_dfm <- dfm(corp, remove = stopwords('en'), remove_punct = TRUE)
news_dfm <- dfm_remove(news_dfm, c('*-time', 'updated-*', 'gmt', 'bst'))
news_dfm <- dfm_trim(news_dfm, min_termfreq = 100)
topfeatures(news_dfm)
news_fcm <- fcm(news_dfm)
dim(news_fcm) # like transition matrix??
feat <- names(topfeatures(news_fcm, 50))
news_fcm <- fcm_select(news_fcm, feat)
dim(news_fcm)
size <- log(colSums(dfm_select(news_dfm, feat)))
textplot_network(news_fcm, min_freq = 0.8, vertex_size = size / max(size) * 3)
tweet_dfm %>% textstat_frequency(n = 15) %>% ggplot(aes(x = reorder(feature, frequency), y = frequency)) + geom_point() +
  coord_flip() +labs(x = NULL, y = "Frequency") +theme_minimal()
textplot_wordcloud(tweet_dfm, max_words = 100)
# create document-level variables
docvars(tweet_corp, "dummy_english") <- factor(ifelse(docvars(tweet_corp, "lang") == "English", "English", "Not English"))
tweet_corp_language <- dfm(tweet_corp, select = "#*", groups = "dummy_english")
textplot_wordcloud(tweet_corp_language, comparison = TRUE, max_words = 200)
# textstat_lexdiv() calcuates lexical diversity in various measures based on the number of unique types of tokens and the length of a document.
inaug_toks <- tokens(data_corpus_inaugural)
inaug_dfm <- dfm(inaug_toks, remove = stopwords('en'))
lexdiv <- textstat_lexdiv(inaug_dfm)
tail(lexdiv, 5)
plot(lexdiv$TTR, type = 'l', xaxt = 'n', xlab = NULL, ylab = "TTR")
grid()
axis(1, at = seq_len(nrow(lexdiv)), labels = docvars(inaug_dfm, 'President'))
# textstat_dist() calcuates similarites of documents or features for various measures
inaug_toks <- tokens(data_corpus_inaugural)
inaug_dfm <- dfm(inaug_toks, remove = stopwords('en'))
dist <- textstat_dist(inaug_dfm)
clust <- hclust(dist)
plot(clust, xlab = "Distance", ylab = NULL)
# Using textstat_keyness(), you can compares frequencies of words between target and reference documents
news_toks <- tokens(news_corp, remove_punct = TRUE) 
news_dfm <- dfm(news_toks)

key <- textstat_keyness(news_dfm, year(docvars(news_dfm, 'date')) >= 2016)
attr(key, 'documents') <- c('2016', '2012-2015')

textplot_keyness(key)
# By collocation analysis, we can identify contiguous collocations of words.
news_toks <- tokens(news_corp, remove_punct = TRUE)
cap_col <- tokens_select(news_toks, '^[A-Z]', valuetype = 'regex', case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 100, size=2)
head(cap_col, 20)

# further study
# https://tutorials.quanteda.io/advanced-operations/twitter-user-similarity/


# basic R commands
# https://tutorials.quanteda.io/introduction/r-commands/
# load multiple text files and generate document-level variables
data <- readtext("/txt.EU_manifestos/*.txt", docvarsfrom = "filenames", docvarnames = c("unit","context","year","language","party"), dvsep="_", encoding="ISO-8859-1")
twitter_data <- readtext("content/data/twitter.json")
data2 <- readtext("pdf/UDHR/*.pdf", docvarsfrom = "filenames", docvarnames = c("document","language"), sep="_")
word_data <- readtext("/word/*.docx")
# some encoding not supported by R
filename <- gsub(".txt$","", filename)
encoding <- sapply(strsplit(filename, "_"), "[", 3)
setdiff(encoding, iconvlist())
# basic workflow of quanteda
# 1. corpus : save character strings and variables in a data frames and combines texts with document-level variables
# 2. Tokens : stores tokens in a list of vectors, preserves positions of words (positional analysis: string-of-words)
# 3. DFM(Document-feature matrix) : represents frequencies of features in documents in a matrix. (non-positional analysis: bag-of-words)
# if character vectors are given to dfm(), it internally constructs corpus and tokens, before a DFM.

# Corpus
# You can create a corpus from various available sources:
#   
# A character vector consisting of one document per element
# A data frame consisting of a character vector for documents, and additional vectors for document-level variables
# A VCorpus or SimpleCorpus class object created by the tm package
# A keywords-in-context object constructed by kwic()

immig_corp <- corpus(data_char_ukimmig2010, 
                     docvars = data.frame(party = names(data_char_ukimmig2010)))
summary(immig_corp)

# joining data using data table
setkey(X,"word")
setkey(Y,"word")
merge(X,Y, all=FALSE)
merge(x,Y, all.x=TRUE)
merge(X,Y, all=TRUE)
# split two columns in data table
library(tidyr)
separate(data=df, col=FOO, into=c("left","right"), sep="\\|")
require(reshape)
df<- transform(df, Foo=colsplit(FOO, split="\\|", names=c("a","b")))
within(df, Foo<- data.table(do.call("rbind", strsplit(as.character(df$FOO),"|",fixed = TRUE))))
# merge with sum of two columns
setkey(blogs3gram,"word")
setkey(news3gram,"word")
c<-merge(blogs3gram, news3gram, all=TRUE)
c[,freq := sum(freq.x, freq.y, na.rm = TRUE), by=1:NROW(c)]
# string manipulation
# c$pred <- paste(unlist(strsplit(c$word, " "))[1:2], collapse = " ")
d <- separate(data=c, col=word, into=c("word","pred"),sep="_[a-z]+$")
# Markov Assumption
# instead of based on previous words, just use previous one, two, three words....
# Discounting methods
M[order(M[,"a"],-M[,"b"]),]
df <- data.frame(Content = featnames(myDfm), Frequency = colSums(myDfm), 
                 row.names = NULL, stringsAsFactors = FALSE)
# extract the string in data table
# option 1
D[, queryWord:=strsplit(ngram,"_[^_]+$")[[1]],by=ngram]
# or
D[, queryWord := strsplit(ngram,"_[^_]+$")][] # you don't need to specify by=word, re-sort it again
# option 2
D[, queryWord := sub('(.*)_.*$','\\1',ngram)][]
D[, predict := lapply(strsplit(D$ngram,"_"), last)][]
D[, predict := sub('.*_(.*)$','\\1',ngram)][]
quanteda_options(threads= 7)
# tips on quiz 2
# First trial, I used only the Trigram model developed from last week, based on 10% of the samples from the source files, I was able to answer all 10 questions (i.e. found at least one of the 4 word choices in my Trigram model) and got 70% correct. I noticed those I got wrong usually have very low frequency or it's the only 1 from the 4 choices found in my model.
# Second trial, I used both my Trigram and Bigram models. If a term ranks highest in both models, I picked it as the answer. Otherwise, I made some judgement which model should have a higher weight in my decision. I got 90% correct this time.
# It seems clear to me the Probability of a N-gram should be the weighted average of the N-th order gram and all the lower order grams. In this case, the interpolation model is the one exactly doing that. The challenge is how I can determine the lambdas, which, according to the N-grams chapter from Prof. Jurafsky and Martin, are calculated with a held-out set data. Has anyone had any experience in finding the lambdas?
# Alternatively, the Katz backoff or the stupid backoff model is an option. But, I think they're less accurate as the data from the lower order grams wouldn't be in use when the higher order gram exist.
# While there is still a lot of study and work ahead, I do feel I understand a little more what I need to do compared to a week ago. It's hard, but it's also fun and fulfilling to figure things out a little by a little entirely on my own.

# algorithm for this project
# Using one smoothing technique to calculate probabilities that hopefully effectively consider more contexts. Thus, best avoid having zero probability.
# We build 1-gram, 2-gram, 3-gram (or even 4-gram?) models. Given a text, extract last 2 words, say: A-B, check these two words with 3-gram table, we may find: A-B-X, A-B-Y, A-B-Z. Calculating probabilities for these (Remind: smoothed probability), return the word with max probability.
# If in 3-gram, we don't have A-B, we step back to 2-gram table, and do the same process. We may find: B-K, B-R, B-U...
# Probably it's a good idea to give more options to end-users, rather than just the most relevant predicted word. Additionally, also the second-most? The third-most?
# In the n-gram table, e.g: 3-gram, we may have: A-B-X (100 times), A-B-Y (80 times), A-B-Z (70 times), A-B-L (10 times), A-B-M (5 times)... The A-B-L, A-B-M should be removed to save memory... (ShinyApps's max memory is 1 GB, free account).
# The difference is that instead of multiplying your lambda by the discounted probability on the unigram you will use the P Continuation (one which uses context to decrease probability of words which appear only after novel (low variety) contexts, like Francisco, which has a high Unigram Count, but only appears after San or General