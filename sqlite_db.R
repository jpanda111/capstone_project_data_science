library(RSQLite)

abs_path <- "C:/Users/yinjiang/Syncplicity Folders/personal/Script/JH_track/Capstone Project/pipeclean/"
relative_path <- "data/sample_trim/"
full_path <- paste(abs_path, relative_path, sep="")

mydrv <- dbDriver("SQLite"); 
db <- dbConnect(drv=mydrv, dbname=paste(full_path, "shiny-app/train_stupid_back_off.db", sep = ""))

dbSendQuery(conn=db,
            "CREATE TABLE NGRAM
            (pre TEXT,
            word TEXT,
            n INTEGER)")

for (n in 1:5) {
  g5 <- readRDS(paste(full_path, "gram", n, ".scoretop5.rds",sep=""))
  sql <- paste("INSERT INTO NGRAM VALUES ($w, $pred, ", n, ")", sep = "")
  dbBegin(db)
  dbSendPreparedQuery(db, sql, bind.data = data.frame(g5))
  dbCommit(db)
}

dbDisconnect(db)

# mydrv <- dbDriver("SQLite"); 
# db <- dbConnect(drv=mydrv, dbname=paste(full_path, "shiny-app/train_katz_back_off.db", sep = ""))
# 
# dbSendQuery(conn=db,
#             "CREATE TABLE NGRAM
#             (pre TEXT,
#             word TEXT,
#             freq INTEGER,
#             d REAL,
#             n INTEGER)")
# 
# for (n in 1:5) {
#   g5 <- readRDS(paste(full_path, "gram", n, ".probtop5.rds",sep=""))
#   sql <- paste("INSERT INTO NGRAM VALUES ($w, $pred, $freq, $discount, ", n, ")", sep = "")
#   dbBegin(db)
#   dbSendPreparedQuery(db, sql, bind.data = data.frame(g5))
#   dbCommit(db)
# }
# 
# dbDisconnect(db)
