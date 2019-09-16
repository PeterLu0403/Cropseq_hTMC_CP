setwd("/Volumes/Macintosh HD/Project/Cropseq_code/Cropseq_hTMC_CP")
## Read sql files:
library(RSQLite)
library(dplyr)
library(stringr)
sql.path<-"../../Database/hTMC_analysis.db"
db <- dbConnect(RSQLite::SQLite(), sql.path)
tables<-dbListTables(db)
lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=db, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

image<-lDataFrames[[which(str_detect(tables,"Image"))]]
object<-lDataFrames[[which(str_detect(tables,"Object"))]]
cell<-lDataFrames[[which(str_detect(tables,"Cell"))]]
nuclei<-lDataFrames[[which(str_detect(tables,"Nuclei"))]]
cytoplasm<-lDataFrames[[which(str_detect(tables,"Cytoplasm"))]]

## Remove location features:
loc<-c("Center","Location","Parent","Children")
cn<-colnames(object)
cn<-cn[which(!str_detect(cn, paste(loc,collapse = "|")))]
variable<-cn[-(1:2)]
write.table(variable,"features.txt",row.names = FALSE, sep = ",",col.names = F)

object<- object %>% select(cn)
metadata<-c("Imagenumber","Platenumber","Replicate","Well","treatment")
image<-image[which(str_detect(colnames(image),paste(metadata,collapse = "|")))]
names(image)<-c("ImageNumber","Platenumber","Replicate","Well","treatment")
s<-inner_join(object,image,by = "ImageNumber")
View(colnames(object$ImageNumber))

e<-object %>% filter(ImageNumber>480)
d<-object %>% filter(ImageNumber<=480)
e$ImageNumber<-e$ImageNumber-480
image_e<-image %>% filter(treatment == "dex-")
image_d<-image %>% filter(treatment =="dex+")

object_e<-left_join(e,image_e,by="ImageNumber")
object_d<-left_join(d,image_d,by="ImageNumber")






