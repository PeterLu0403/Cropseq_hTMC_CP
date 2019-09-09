## Read sql files:
library("RSQLite")

sql.path<-"../../Database/hTMC_analysis.db"
db <- dbConnect(RSQLite::SQLite(), sql.path)
tables<-dbListTables(db)
lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=db, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}
