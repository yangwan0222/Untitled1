install.packages("swirl")
library(swirl)
install_course("The R Programming Environment")
swirl()

## Matrices
m <- 1:10
dim(m) <- c(2, 5)

m <- matrix(1:6, nrow = 2, ncol = 3) 

## Data Frames
x <- data.frame(foo = 1:4, bar = c(T, T, F, F))

## Names
m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d")) 

colnames(m) <- c("h", "f")
rownames(m) <- c("x", "z")

## Find diagnosis
d = read.csv("C://Users//wanya//Downloads//DXSUM_PDXCONV_ADNIALL.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]   

d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
library(Epi)
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d$DODX    = d$DATE

## Fischer verification v2
library(Epi)
library(readxl)
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//fischer_shipment//4 manifest//DXSUM_PDXCONV_ADNIALL.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d <- d[,c(2,4,9)]
d$EXAMDATE <- ymd(d$EXAMDATE)
d$DX <- as.character(d$DX)

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//fischer_shipment//4 manifest//global.xlsx"))
names(d2) <- c("RID","ADNI_ID","EXAMDATE","gender")

p <- c()

for(i in seq(1:50))
{
  d1 <- read.csv(paste0("C://Users//wanya//Desktop//fischer_shipment//4 manifest//New folder (2)//batches_",i,".csv"))
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000960"] <- "FA806S8C-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000961"] <- "FA806SJS-09"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000962"] <- "HA806TVG-11"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000963"] <- "FA806V2D-05"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000964"] <- "CA8074LF-02"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000965"] <- "CA8074KY-04"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000966"] <- "KA8074ZS-04"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000967"] <- "JA809PS3-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000968"] <- "GA809VMP-04"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC000969"] <- "CA809WBK-04"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1020"] <- "CA806RMR-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1021"] <- "AA806TDH-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1022"] <- "HA807HZL-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1023"] <- "KA807J58-03"
  d1$ADNI_ID[d1$ADNI_ID=="ADNIRARC1024"] <- "GA807NFP-03"
  
  d3 <- merge(d1,d2,by="ADNI_ID")
  d3$EXAMDATE <- ymd(d3$EXAMDATE)
  d3$DX <- NA
  for(i in 1:862)
  {
    t <- d[d$RID == d3$RID[i],]
    t <- na.omit(t)
    if(nrow(t)==0)
    {
      print(i)
      next
    }
    t$diff <- t$EXAMDATE - d3$EXAMDATE[i]
    t$diff <- abs(t$diff)
    t <- unique(t)
    d3$DX[i] <- unique(t$DX[t$diff==min(t$diff)])
    if(min(t$diff)>180)
    {
      print(i)
    }
  }
  
  x <- 0
  for(i in 1:21) # batch 1:21, batch 10 don't have shipment 3
  {
    for(j in 1:4)
    {
      if(j == 3)
      {
        x <- x + c("MCI") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 1]
        x <- x + c("MCI") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 2]
      }
      else
      {
        x <- x + as.integer(sum(c("MCI","AD","NL") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 1])==3)
        x <- x + as.integer(sum(c("MCI","AD","NL") %in% d3$DX[d3$shipment == j & d3$batch == i & d3$gender == 2])==3) 
      }
    }
  }
  x
  
  p <- c(p,x)
}

##

d <- data.frame(read_excel("C://Users//wanya//Desktop//temp//New folder//CP Lab AIM Project Box Key Jan-10-23 (1).xlsx"))

##

d <- read.csv("C://Users//wanya//Desktop//unblind//Duke 2//ADMC Duke Biocrates MxP Quant 500 Ultra Performance Liquis Chromatography Longitudinal ADNI-1-GO-2.csv")
d1 <- read.csv("C://Users//wanya//Desktop//unblind//adni_duke2_unblind.csv")
names(d1)[2] <- "Customer.Sample.Identification"

d2 <- merge(d,d1,by = "Customer.Sample.Identification")

d2$Collection.Date <- dmy(d2$Collection.Date)
inventory$EXAMDATE <- mdy(inventory$EXAMDATE)
inventory <- inventory[,c(1,4,7,8)]
inventory <- inventory[inventory$EVENT == "Blood",]

d2$VISCODE <- NA
for(i in seq(1:nrow(d2)))
{
  t <- na.omit(inventory[inventory$RID==d2$ID1[i],])
  t$diff <- abs(d2$Collection.Date[i] - t$EXAMDATE)
  t <- t[t$diff == min(t$diff),]
  
  d2$VISCODE[i] <- t$VISCODE2[1]
  
  if(t$diff[1]>180)
  {
    d2$VISCODE[i] <- NA
  }
}
d3 <- d2[,c(118:120,1:117)]

##

d <- read.csv("C://Users//wanya//Downloads//results (3).csv")

a1 <- d[d$ID2 == "ADNI 1",]
a2 <- d[d$ID2 == "ADNI 2",]
a3 <- d[d$ID2 == "ADNI3",]
ago <- d[d$ID2 == "ADNI GO",]

d$Global.Specimen.ID <- substr(d$Global.Specimen.ID,1,nchar(d$Global.Specimen.ID)-3)

a1$Global.Specimen.ID <- substr(a1$Global.Specimen.ID,1,nchar(a1$Global.Specimen.ID)-3)
a2$Global.Specimen.ID <- substr(a2$Global.Specimen.ID,1,nchar(a2$Global.Specimen.ID)-3)
a3$Global.Specimen.ID <- substr(a3$Global.Specimen.ID,1,nchar(a3$Global.Specimen.ID)-3)
ago$Global.Specimen.ID <- substr(ago$Global.Specimen.ID,1,nchar(ago$Global.Specimen.ID)-3)

d <- read.csv("C://Users//wanya//Downloads//results (2).csv")
d1 <- read.csv("C://Users//wanya//Downloads//results (3).csv")
d2 <- read.csv("C://Users//wanya//Downloads//results (4).csv")

##
d <- read.csv("C://Users//wanya//Downloads//results (6).csv")
d <- ptdemog[,c(1,3,5,12,13,21)]

##
d <- read.csv("C://Users//wanya//Desktop//Autopsy//autopsy2//CARD ADNI-CSF-inventory.csv")

dx <- rbind(d[d$LastCSF=="Last CSF sample" & d$NP == "PENDING",],d[d$LastCSF=="Last CSF sample" & d$NP == "AVAILABLE",])
dx$CSF.DATE <- mdy(dx$CSF.DATE)
dx <- dx[,c(1,3)]

d1 <- read.csv("C://Users//wanya//Downloads//results.csv")
d1$Collection.Date <- gsub("/"," ",d1$Collection.Date)
d1$Collection.Date <- dmy(d1$Collection.Date)
names(d1) <- c("RID","CSF.DATE","GID")
d1$GID <- substr(d1$GID,1,nchar(d1$GID)-3)
d1 <- unique(d1)

d2 <- merge(dx,d1,by = c("RID","CSF.DATE"))

d3 <- read.csv("C://Users//wanya//Downloads//results (1).csv")

##
library(readxl)
library(lubridate)

d <- read.csv("C://Users//wanya//Downloads//results (3).csv")
conversion <- data.frame(read_excel("C://Users//wanya//Desktop//AltPep//conversion.xlsx"))
fujicsf <- data.frame(read_excel("C://Users//wanya//Desktop//AltPep//fujicsf.xlsx"))
fujipla <- data.frame(read_excel("C://Users//wanya//Desktop//AltPep//fujipla.xlsx"))
nonad <- data.frame(read_excel("C://Users//wanya//Desktop//AltPep//nonad.xlsx"))

d$Collection.Date <- dmy(d$Collection.Date)
conversion$EXAMDATE <- ymd(conversion$EXAMDATE)
fujicsf$EXAMDATE <- ymd(fujicsf$EXAMDATE)
fujipla$EXAMDATE <- ymd(fujipla$EXAMDATE)
nonad$EXAMDATE <- ymd(nonad$EXAMDATE)
names(d)[1] <- "RID"
names(d)[2] <- "EXAMDATE"
d$Global.Specimen.ID <- substr(d$Global.Specimen.ID,1,nchar(d$Global.Specimen.ID)-3)
d <- unique(d)

conversion1 <- merge(conversion,d,by = c("RID","EXAMDATE"))
fujicsf1 <- merge(fujicsf,d,by = c("RID","EXAMDATE"))
fujipla1 <- merge(fujipla,d,by = c("RID","EXAMDATE"))
nonad1 <- merge(nonad,d,by = c("RID","EXAMDATE"))

write.csv(conversion1,"C://Users//wanya//Desktop//AltPep//conversion1.csv")
write.csv(fujicsf1,"C://Users//wanya//Desktop//AltPep//fujicsf1.csv")
write.csv(fujipla1,"C://Users//wanya//Desktop//AltPep//fujipla1.csv")
write.csv(nonad1,"C://Users//wanya//Desktop//AltPep//nonad1.csv")

#############################
library(lubridate)
d <- read.csv("C://Users//wanya//Desktop//Autopsy//autopsy3//CARD ADNI-CSF-inventory.csv")
d <- d[d$LastCSF=="Last CSF sample",]
d <- d[!(d$NP=="AVAILABLE" | d$NP=="PENDING"),]
d$CSF.DATE <- mdy(d$CSF.DATE)

d1 <- read.csv("C://Users//wanya//Downloads//results (5).csv")
d1$Global.Specimen.ID <- substr(d1$Global.Specimen.ID,1,nchar(d1$Global.Specimen.ID)-2)
d1 <- unique(d1)
d1$Collection.Date <- dmy(d1$Collection.Date)

names(d1)[1] <- "RID"
names(d1)[2] <- "CSF.DATE"

d2 <- merge(d,d1,by=c("RID","CSF.DATE"))
d2 <- d2[-c(1450),]
d2 <- d2[-c(962),]

#############################

d <- read.csv("C://Users//wanya//Desktop//dod//ADNIDOD_ALIQUOT_LIST_15Jun2023.csv")
library(stringr)
d$SCRNO <- str_pad(d$SCRNO, 7, pad = "0")
d1 <- read.csv("C://Users//wanya//Downloads//results.csv")
d1$ID1 <- str_pad(d1$ID1, 7, pad = "0")
d1$Global.Specimen.ID <- substr(d1$Global.Specimen.ID,1,nchar(d1$Global.Specimen.ID)-2)
d1 <- unique(d1)

library(lubridate)
d1$Collection.Date <- dmy(d1$Collection.Date)
d$EXAMDATE <- mdy(d$EXAMDATE)
names(d)[2] <- "ID1"
names(d)[4] <- "Collection.Date"

d2 <- merge(d,d1,by = c("ID1","Collection.Date"),all.x = TRUE)
write.csv(d2,"C://Users//wanya//Desktop//dod//ADNIDOD_ALIQUOT_LIST_15Jun2023_gid.csv")

#############################
library(lubridate)
library(readxl)

d <- inventory
d <- d[d$EVENT=="Blood" & d$TABLE == "biomark",]
d$EXAMDATE <- mdy(d$EXAMDATE)
d <- d[,c(1,4,8)]

d1 <- read.csv("C://Users//wanya//Downloads//results (3).csv")
d1$Collection.Date <- dmy(d1$Collection.Date)
names(d1) <- c("ID","RID","EXAMDATE")

d2 <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//Ostaszewski//NT1_ADNI.xlsx"))
d2$id <- 1:nrow(d2)

d2 <- merge(d2,d1,by = "ID", all.x = TRUE)
d2 <- merge(d2,d, by = c("RID","EXAMDATE"),all.x = TRUE)

#############################

library(usethis)
use_git_config(user.name = "yangwan0221", user.email = "yangwan0221@gmail.com")
git_vaccinate()


