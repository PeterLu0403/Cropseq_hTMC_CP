setwd("/Volumes/Macintosh HD/Project/Cropseq_code/Cropseq_hTMC_CP")
## Read sql files:
library(RSQLite)
library(dplyr)
library(stringr)
library(ggplot2)

sql.path<-"../../Database/MYOC_qc.db"
db <- dbConnect(RSQLite::SQLite(), sql.path)
tables<-dbListTables(db)
lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=db, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

image<-lDataFrames[[4]]
object<-lDataFrames[[5]]

meta_image<-c("ImageNumber","Platenumber","Replicate","Well","treatment")
image<-image[which(str_detect(colnames(image),paste(meta_image,collapse = "|")))]
names(image)<-c("ImageNumber","Platenumber","Replicate","Well","treatment")
image$ImageNumber<-as.integer(image$ImageNumber)

object<- left_join(object,image,by = "ImageNumber")
df<-read.csv("Metadata.csv",header = T,sep = ",")
object<- object %>% mutate(plate_well=paste(Replicate,Well,sep = "_"))
object<- inner_join(object,df,by="plate_well")
df_intensity<-data.frame(Knocked.Gene=object$Knocked.Gene,
                            treatment=object$treatment,
                            myoc_intensity_mean=object$Cytoplasm_Intensity_MeanIntensity_MYOC,
                            myoc_intensity_median=object$Cytoplasm_Intensity_MedianIntensity_MYOC,
                            nuclear_intensity_mean=object$Nucleus_Intensity_MeanIntensity_Nucleus,
                            nuclear_intensity_median=object$Nucleus_Intensity_MedianIntensity_Nucleus)
write.csv(df_intensity,"myoc_intensity.csv",row.names = F)
## Plot
df_intensity<-read.csv("myoc_intensity.csv",header = T,sep = ",")
df_t<-df_intensity %>% filter(!str_detect(Knocked.Gene,paste(c("pooled"),collapse = "|")))
df_t<-mutate(df_t,mean_r=df_t$myoc_intensity_mean/df_t$nuclear_intensity_mean)
df_t<-mutate(df_t,median_r=df_t$myoc_intensity_median/df_t$nuclear_intensity_median)

## Color positive and nc, re-order x-axis
df_t$Gene<-substr(df_t$Knocked.Gene,4,14)
m<-levels(as.factor(df_t$Gene))
m_nc<-m[str_detect(m,"Human")]
m_po<-c("MYOC","CYP1B1","FOXC1","LTBP2","TEK")
m<-m %>% setdiff(m_nc) %>% setdiff(m_po)
df_g<-data.frame(Gene=levels(as.factor(df_t$Gene)))
df_g<-df_g %>% mutate(color=case_when(Gene %in% m_nc ~"black",
                                      Gene %in% m_po ~"red",
                                      Gene %in% m ~"blue"))
colors<-df_g$color
#face<-df_g$face
#df_g<-read.csv("Gene_colour.csv",header = T,sep = ",")


p<-ggplot(df_t,aes(x=Gene,y=mean_r,fill=treatment))
p<-p + geom_boxplot(outlier.size = 0.05,
                    fatten=1.4,
                    lwd=0.4)
p<-p + theme_bw()+theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
             legend.position = "top",
             panel.grid.major.x = element_blank(), 
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),
             panel.border=element_blank(),
             axis.line=element_line(size=0.4,colour="black"),
             axis.text.x.bottom = element_text(color=colors,size = 10)
             )
p<-p + scale_fill_manual(values = c("#08D9D6","#E8222D"))
p<-p + xlab("") + ylab("myocilin intensity normalized to nucleus") + ggtitle("Dexamethasone induced myocilin expression in hTMCs")
p<-p + scale_x_discrete(breaks=levels(as.factor(df_t$Gene)),limits=c(m_po,m,m_nc))
p<-p + ylim(0,1.5)
p



pdf(file="dex_induced_myocilin_expression.pdf",width = 10,height = 7,bg="transparent")
p
dev.off()
