######## ADNIMERGE FIX
library(ADNIMERGE)
inventory %>% mutate(EXAMDATE = case_when(RID == 2357 & VISCODE2 == 'bl' & EVENT == "CSF" ~ '05/10/2011',TRUE ~ EXAMDATE))
inventory %>% mutate(EXAMDATE = case_when(RID == 6606 & VISCODE2 == 'bl' & EVENT == "CSF" ~ '01/16/2019',TRUE ~ EXAMDATE))

########
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

#############################

d1 <- read.csv("C://Users//wanya//Desktop//Cruchaga_lab//Cruchaga_lab_ADNI_CSF_idlookup_SOMAscan_20_06_2023.csv")
d2 <- read.csv("C://Users//wanya//Desktop//Cruchaga_lab//Cruchaga_lab_ADNI_CSF_SOMAscan7k_Protein_matrix_postQC_20_06_2023.csv")
d3 <- inventory

d <- read.csv("C://Users//wanya//Downloads//results (3).csv")
names(d) <- c("GUSPECID","RID","EXAMDATE")
d$EXAMDATE <- dmy(d$EXAMDATE)

d1 <- merge(d1,d,by = "GUSPECID")
d3 <- d3[d3$EVENT == "CSF",]
d3 <- d3[,c(1,4,8)]
d3$EXAMDATE <- mdy(d3$EXAMDATE)

d4 <- merge(d1,d3,by = c("RID","EXAMDATE"),all.x = TRUE)

#############################

d1 <- data.frame(read_excel("C://Users//wanya//Desktop//emory//emory - shipping manifest.xls"))
d <- read.csv("C://Users//wanya//Desktop//temp//DXSUM_PDXCONV_ADNIALL_11Jul2023.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
library(Epi)
d$DX <- Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d$DODX <- d$DATE
d$EXAMDATE <- ymd(d$EXAMDATE)
d <- d[,c(2,4,9)]
d$DX <- as.character(d$DX)

d2 <- read.csv("C://Users//wanya//Desktop//temp//results (4).csv")
d2$Collection.Date <- dmy(d2$Collection.Date)

d1 <- merge(d1,d2,by = "Global.Specimen.ID")
d1 <- d1[,c(6,7)]

names(d1)[1] <- "RID"
names(d1)[2] <- "EXAMDATE"

d3 <- merge(d1,d,by = c("RID","EXAMDATE"),all.x = TRUE)

for(i in 1:nrow(d3))
{
  if(!is.na(d3[i,3]))
  {
    next
  } else
  {
    t <- d[d$RID==d3[i,1],]
    t$diff <- abs(t$EXAMDATE - d3[i,2])
    t <- t[!is.na(t$EXAMDATE),]
    if(min(t$diff)<90)
    {
      d3[i,3] <- t[t$diff==min(t$diff),]$DX[1]
    } else
    {
      d3[i,3] <- NA
    }
  }
}

#############################

d <- read.csv("C://Users//wanya//Desktop//unblind//emory//EMORY_PEPTIDERATIOS_SET2.csv")
d$id <- 1:nrow(d)
d1 <- read.csv("C://Users//wanya//Downloads//results (4).csv")
names(d1) <- c("LINK_ID","RID","EXAMDATE","VOL_ML")
d1$EXAMDATE <- dmy(d1$EXAMDATE)

d <- merge(d,d1,by = "LINK_ID",all.x = TRUE)

library(ADNIMERGE)
d2 <- force(inventory)
d2 <- d2[,c(1,4,7,8)]
d2 <- d2[d2$EVENT == "CSF",]
d2$EXAMDATE <- mdy(d2$EXAMDATE)

d <- d[,c(1,6:15,17:20)]
names(d)[13] <- "RID"
names(d)[14] <- "EXAMDATE"
names(d)[15] <- "VOL_ML"

d <- merge(d,d2,by = c("RID","EXAMDATE"), all.x = TRUE)
d <- d[,-c(17)]

t <- unique(d[is.na(d$VISCODE2),]$LINK_ID)
for(i in 1:length(t))
{
  t1 <- d1[d1$LINK_ID == t[i],]
  if(nrow(t1)==0)
  {
    next
  }
  
  t2 <- d2[d2$RID == t1$RID,]
  t2$diff <- abs(t2$EXAMDATE - t1$EXAMDATE)
  t2 <- t2[!is.na(t2$diff),]
  if(min(t2$diff)<180)
  {
    d[d$LINK_ID==t1$LINK_ID,]$VISCODE2 <- t2[t2$diff == min(t2$diff),]$VISCODE2
  }
}

for(i in 1:40)
{
  t1 <- d1[d1$LINK_ID == t[i],]
  print(i)
  print(t1)
  print(d2[d2$RID == t1$RID,])
  
}

d[d$LINK_ID=="BA80N5BT-02",]$VISCODE2 <- "bl"

#############################

d1 <- read.csv("C://Users//wanya//Desktop//Cruchaga_lab//Cruchaga_lab_ADNI_CSF_idlookup_Metabolon_20_06_2023_unblind.csv")
d1$id <- 1:nrow(d1)
d3 <- inventory

d <- read.csv("C://Users//wanya//Downloads//results (3).csv")
names(d) <- c("GUSPECID","RID","EXAMDATE")
d$EXAMDATE <- dmy(d$EXAMDATE)

d1 <- merge(d1,d,by = "GUSPECID")
d3 <- d3[d3$EVENT == "CSF",]
d3 <- d3[,c(1,4,8)]
d3$EXAMDATE <- mdy(d3$EXAMDATE)

d4 <- unique(merge(d1,d3,by = c("RID","EXAMDATE"),all.x = TRUE))

t <- d4[is.na(d4$VISCODE2),]
for(i in 1:nrow(t))
{
  t1 <- d3[d3$RID == t[i,1],]
  if(nrow(t1)==0)
  {
    next
  } else
  {
    t1$diff <- abs(t1$EXAMDATE-t$EXAMDATE[i])
    if(min(t1$diff)<90)
    {
      d4[d4$Somalogic_Barcode_A==t[i,4],]$VISCODE2 <- t1[t1$diff == min(t1$diff),]$VISCODE2
    }
  }
}

write.csv(d1,"C://Users//wanya//Desktop//Cruchaga_lab//Cruchaga_lab_ADNI_CSF_metabolomic_matrix_20_06_2023_unblind.csv")

d5 <- read.csv("C://Users//wanya//Desktop//Cruchaga_lab//Cruchaga_lab_ADNI_CSF_SOMAscan7k_Protein_matrix_postQC_20_06_2023.csv")

dx <- merge(d4,d5,by="Somalogic_Barcode_A")

#############################

d <- read.csv("C://Users//wanya//Downloads//Biomarker Samples_vertical.csv")[,c("site.id","subject.code","ts_last_modify")]
d <- unique(d)

d1 <- read.csv("C://Users//wanya//Downloads//results (7).csv")

d_edc <- data.frame(table(d$subject.code))
d_ldms <- data.frame(table(d1$ID1))
d_ldms <- d_ldms[-c(1),]

d_edc$Var1 <- as.character(d_edc$Var1)
d_edc$Var2 <- as.integer(substr(d_edc$Var1,nchar(d_edc$Var1)-3,nchar(d_edc$Var1)))

names(d_edc) <- c("PTID","edc_freq","Var1")
names(d_ldms)[2] <- "ldms_freq"

d2 <- merge(d_ldms,d_edc,by = "Var1", all.x = TRUE)
d2 <- d2[is.na(d2$edc_freq) | d2$ldms_freq > d2$edc_freq,]
write.csv(d2,"C://Users//wanya//Desktop//temp//PTIDlist.csv")

d3 <- merge(d_edc,d_ldms,by = "Var1", all.x = TRUE)
d3 <- d3[is.na(d3$ldms_freq) | d3$edc_freq > d3$ldms_freq,]

#############################

d <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_HighConfidence_lph ADNIGO2 .csv")
d$Sample.Name <- substr(d$Sample.Name,nchar(d$Sample.Name)-10,nchar(d$Sample.Name))
d$id <- 1:nrow(d)

d1 <- read.csv("C://Users//wanya//Downloads//results (9).csv")
d1$Collection.Date <- dmy(d1$Collection.Date)
names(d1) <- c("RID", "Sample.Name", "EXAMDATE")

d <- merge(d,d1,by = "Sample.Name", all.x = TRUE)

d2 <- force(inventory)
d2 <- d2[d2$EVENT == "Blood",]
d2$EXAMDATE <- mdy(d2$EXAMDATE)
d2 <- d2[,c(1,4,8)]

d <- merge(d,d2,by = c("RID","EXAMDATE"),all.x = TRUE)
t <- d[is.na(d$VISCODE2),]

for(i in 1:nrow(t))
{
  if(t$Sample.Name[i]=="Background" | t$Sample.Name[i]=="RSDQC")
  {
    next
  }
  t1 <- d2[d2$RID==t$RID[i],]
  if(nrow(t1)==0)
  {
    next
  }
  t1$diff <- abs(t1$EXAMDATE - t$EXAMDATE[i])
  if(min(t1$diff)<=90)
  {
    d$VISCODE2[d$RID==t$RID[i] & d$EXAMDATE==t$EXAMDATE[i]] <- t1[t1$diff == min(t1$diff),]$VISCODE2[1]
  } else
  {
    d[d$RID==t$RID[i] & d$EXAMDATE==t$EXAMDATE[i],][1,63] <- NA
  }
}

d1$FLDNAME[!d1$FLDNAME%in% names(d2)]
d <- d1[!d1$FLDNAME%in%d1$FLDNAME[!d1$FLDNAME%in% names(d2)],]

d1 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_WithCaution_lph ADNIGO2_dictionary.csv")
d1$Sample.Name <- substr(d1$Sample.Name,nchar(d1$Sample.Name)-10,nchar(d1$Sample.Name))
d2 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_WithCaution_lph ADNIGO2_unblind.csv")

#############################

d <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Data Dictionary ADNIGO2 .csv")
d <- na.omit(d)

d1 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_HighConfidence_hph ADNIGO2_dictionary.csv")
d2 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_HighConfidence_lph ADNIGO2_dictionary.csv")
d3 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_WithCaution_hph ADNIGO2_dictionary.csv")
d4 <- read.csv("C://Users//wanya//Desktop//unblind//ADMC//ADMC Leiden Oxylipins Baseline Metabolites_WithCaution_lph ADNIGO2_dictionary.csv")

#############################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Autopsy//autopsy3//temp.xlsx"))
d$o <- 1:nrow(d)

for (i in seq(nrow(d)))
{
  d$gid[i] <- paste(d$gid[i],toString(sprintf("%02d",d$id)[i]),sep = "")
}

#############################

library(readxl)
library(lubridate)

d <- data.frame(read_excel("C://Users//wanya//Desktop//BC NSC//file1179.xlsx"))
d$EXAMDATE <- ymd(d$EXAMDATE)
g <- read.csv("C://Users//wanya//Desktop//BC NSC//results.csv")
g$Collection.Date <- dmy(g$Collection.Date)

g$Global.Specimen.ID <- substr(g$Global.Specimen.ID,1,nchar(g$Global.Specimen.ID)-3)
g <- unique(g)
names(g) <- c("RID","EXAMDATE","GID")

t <- merge(d,g,by = c("RID","EXAMDATE"),all.x = TRUE)

t <- read.csv("C://Users//wanya//Desktop//BC NSC//1179_location.csv")
t$Storage.Unit <- substr(t$Storage.Unit,nchar(t$Storage.Unit)-1,nchar(t$Storage.Unit))
t$Sub.Level <- sapply(t$Sub.Level,function(i) strsplit(i," ")[[1]][5])
t$Container <- sapply(t$Container,function(i) strsplit(i," ")[[1]][2])

#############################

d <- data.frame(read_excel("C://Users//wanya//Desktop//BC NSC//file1065.xlsx"))
d$EXAMDATE <- ymd(d$EXAMDATE)
g <- read.csv("C://Users//wanya//Desktop//BC NSC//results.csv")
g$Collection.Date <- dmy(g$Collection.Date)

g$Global.Specimen.ID <- substr(g$Global.Specimen.ID,1,nchar(g$Global.Specimen.ID)-3)
g <- unique(g)
names(g) <- c("RID","EXAMDATE","GID")

t <- merge(d,g,by = c("RID","EXAMDATE"),all.x = TRUE)

t <- read.csv("C://Users//wanya//Desktop//BC NSC//1065_location.csv")
t$Storage.Unit <- substr(t$Storage.Unit,nchar(t$Storage.Unit)-1,nchar(t$Storage.Unit))
t$Sub.Level <- sapply(t$Sub.Level,function(i) strsplit(i," ")[[1]][5])
t$Container <- sapply(t$Container,function(i) strsplit(i," ")[[1]][2])

t1 <- read.csv("C://Users//wanya//Desktop//BC NSC//file1065.csv")

#############################

d1 <- read.csv("C://Users//wanya//Desktop//Yassine//New folder//Yassine_CSF-1.csv")
d2 <- read.csv("C://Users//wanya//Desktop//Yassine//New folder//Yassine_plasma-1.csv")
d1$id <- 1:nrow(d1)
d2$id <- 1:nrow(d2)

t <- read.csv("C://Users//wanya//Desktop//Yassine//New folder//results.csv")
names(t) <- c("RID","EXAMDATE","Sample_ID")
names(d1)[3] <- "Sample_ID"
names(d2)[3] <- "Sample_ID"

d1 <- merge(d1,t,by = "Sample_ID")
d2 <- merge(d2,t,by = "Sample_ID")
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d2$EXAMDATE <- dmy(d2$EXAMDATE)

t <- force(inventory)
t1 <- t[t$EVENT == "CSF",]
t2 <- t[t$EVENT == "Blood",]
t1$EXAMDATE <- mdy(t1$EXAMDATE)
t2$EXAMDATE <- mdy(t2$EXAMDATE)
t1 <- t1[,c(1,4,8)]
t2 <- t2[,c(1,4,8)]

csf <- merge(d1,t1,by = c("RID","EXAMDATE"),all.x = TRUE)
csf[is.na(csf$VISCODE2),]$VISCODE2 <- "UNK"
pla <- merge(d2,t2,by = c("RID","EXAMDATE"),all.x = TRUE)
pla[is.na(pla$VISCODE2),]$VISCODE2 <- "UNK"

write.csv(csf,"C://Users//wanya//Desktop//Yassine//New folder//Yassine_CSF-1_unblind.csv")
write.csv(pla,"C://Users//wanya//Desktop//Yassine//New folder//Yassine_plasma-1_unblind.csv")

############################## apply
# http://rstudio-pubs-static.s3.amazonaws.com/21347_418bc228038d4e94815018ad415bba49.html
tapply(iris$Sepal.Length, iris$Species, function(x) c(min(x), median(x), max(x)))


#############################
library(readxl)
library(lubridate)
library(ADNIMERGE)

d <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//oAb_ADNI//Copy of oAb_ADNI.xlsx"))
names(d)[1] <- "Sample_ID"
d1 <- read.csv("C://Users//wanya//Desktop//unblind//oAb_ADNI//results.csv")
names(d1) <- c("Sample_ID","RID","EXAMDATE")
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d2 <- force(inventory)
d2 <- d2[d2$EVENT == "Blood",]
d2 <- d2[,c(1,4,8)]
d2$EXAMDATE <- mdy(d2$EXAMDATE)

d[d$Sample_ID == "ADNIRARC000980",]$Sample_ID <- "BA808GL7-11"
d[d$Sample_ID == "ADNIRARC000981",]$Sample_ID <- "KA80C5XF-11"
d[d$Sample_ID == "ADNIRARC000982",]$Sample_ID <- "CA8072N5-06"
d[d$Sample_ID == "ADNIRARC000983",]$Sample_ID <- "KA807PQW-09"
d[d$Sample_ID == "ADNIRARC000984",]$Sample_ID <- "DA807DS8-04"
d[d$Sample_ID == "ADNIRARC000985",]$Sample_ID <- "HA807SPT-11"
d[d$Sample_ID == "ADNIRARC000986",]$Sample_ID <- "CA808482-14"
d[d$Sample_ID == "ADNIRARC000987",]$Sample_ID <- "AA8088Z7-07"
d[d$Sample_ID == "ADNIRARC000988",]$Sample_ID <- "DA808PPP-04"
d[d$Sample_ID == "ADNIRARC000989",]$Sample_ID <- "CA808SLN-04"
d[d$Sample_ID == "ADNIRARC000990",]$Sample_ID <- "HA80BS2X-11"
d[d$Sample_ID == "ADNIRARC000991",]$Sample_ID <- "HA80JLW9-02"
d[d$Sample_ID == "ADNIRARC000992",]$Sample_ID <- "JA80L826-02"
d[d$Sample_ID == "ADNIRARC000993",]$Sample_ID <- "AA80JZ2L-02"
d[d$Sample_ID == "ADNIRARC000994",]$Sample_ID <- "AA807P44-14"
d[d$Sample_ID == "ADNIRARC000995",]$Sample_ID <- "EA8073TJ-03"
d[d$Sample_ID == "ADNIRARC000996",]$Sample_ID <- "GA80C2J8-04"
d[d$Sample_ID == "ADNIRARC000997",]$Sample_ID <- "HA80CH7D-03"
d[d$Sample_ID == "ADNIRARC000998",]$Sample_ID <- "GA80NYFT-04"
d[d$Sample_ID == "ADNIRARC000999",]$Sample_ID <- "DA807N7W-06"

d <- merge(d,d1,by = "Sample_ID",all.x = TRUE)
d$EXAMDATE <- dmy(d$EXAMDATE)

d$VISCODE <- NA
for(i in 1:nrow(d))
{
  t <- na.omit(d2[d2$RID == d$RID[i],])
  if(nrow(t)==0)
  {
    d$VISCODE[i] <- "UNK"
    next
  }
  t$diff <- abs(t$EXAMDATE - d$EXAMDATE[i])
  if(min(t$diff)[1]>90)
  {
    d$VISCODE[i] <- "UNK"
    next
  } else
  {
    d$VISCODE[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}

############################## 

d <- read.csv("C://Users//wanya//Desktop//BC NSC//1065_location.csv")

d$GID <- substr(d$GID,1,nchar(d$GID)-2)

for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$id)[i]),sep = "")
}


##############################

d <- d[d$EVENT == "CSF",]
d <- d[,c(1,4,8)]
d$EXAMDATE <- mdy(d$EXAMDATE)

##
d1 <- read.csv("C://Users//wanya//Desktop//BIOMK//UPENNBIOMK9 - Copy.csv")
d1$EXAMDATE <- mdy(d1$EXAMDATE)

for(i in 1:nrow(d1))
{
  if(d1$COMMENT[i]!="")
  {
    d1$ABETA42[i] <- strsplit(d1$COMMENT[i], " +")[[1]][5]
  }
}
d1[d1$COMMENT!="",]$COMMENT <- "Abeta42>1700"
d1$EXAMDATE <- mdy(d1$EXAMDATE)

d1$VISCODE2 <- NA
for (i in 1:nrow(d1))
{
  t <- d[d$RID==d1$RID[i],]
  t$diff <- abs(t$EXAMDATE - d1$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    d1$VISCODE2[i] <- NA
    next
  }
  else
  {
    d1$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}
write.csv(d1,"C://Users//wanya//Desktop//BIOMK//UPENNBIOMK9_v1.csv")

##
d2 <- read.csv("C://Users//wanya//Desktop//BIOMK//UPENNBIOMK10.csv")
d2$EXAMDATE <- mdy(d2$EXAMDATE)
d2$BATCH <- "UPENNBIOMK10"
names(d2)[5] <- "ABETA42"
names(d2)[6] <- "ABETA40"
d2[d2$NOTE == "Abeta>1700",]$NOTE <- "Abeta42>1700"

t <- read.csv("C://Users//wanya//Desktop//BIOMK//results.csv")
t$EXAMDATE <- dmy(t$EXAMDATE)
t <- unique(t)
d2$PHASE <- NA
for(i in 1:nrow(d2))
{
  t1 <- t[t$RID == d2$RID[i],]
  t1$diff <- abs(t1$EXAMDATE-d2$EXAMDATE[i])
  if(min(t1$diff>90))
  {
    next
  }
  else
  {
    d2$PHASE[i] <- t1[t1$diff == min(t1$diff),]$PHASE[1]
  }
}
d2$VISCODE2 <- NA
for (i in 1:nrow(d2))
{
  t <- d[d$RID==d2$RID[i],]
  t$diff <- abs(t$EXAMDATE - d2$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    next
  }
  else
  {
    d2$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}
write.csv(d2,"C://Users//wanya//Desktop//BIOMK//UPENNBIOMK10_v1.csv")

##
d3 <- read.csv("C://Users//wanya//Desktop//BIOMK//UPENNBIOMK10_07_29_19.csv")
d3$EXAMDATE <- mdy(d3$EXAMDATE)

t <- read.csv("C://Users//wanya//Desktop//BIOMK//results.csv")
t$EXAMDATE <- dmy(t$EXAMDATE)
t <- unique(t)
d3$PHASE <- NA
for(i in 1:nrow(d3))
{
  t1 <- t[t$RID == d3$RID[i],]
  t1$diff <- abs(t1$EXAMDATE-d3$EXAMDATE[i])
  if(min(t1$diff>90))
  {
    next
  }
  else
  {
    d3$PHASE[i] <- t1[t1$diff == min(t1$diff),]$PHASE[1]
  }
}
d3$VISCODE2 <- NA
for (i in 1:nrow(d3))
{
  t <- d[d$RID==d3$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$EXAMDATE - d3$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    next
  }
  else
  {
    d3$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}
d3$BATCH <- "UPENNBIOMK11"
write.csv(d3,"C://Users//wanya//Desktop//BIOMK//UPENNBIOMK11_v1.csv")

##
d4 <- read.csv("C://Users//wanya//Desktop//BIOMK//UPENNBIOMK12.csv")
names(d4)[1] <- "PHASE"
names(d4)[5] <- "ABETA40"
names(d4)[6] <- "ABETA42"
d4$EXAMDATE <- mdy(d4$EXAMDATE)
d4 <- d4[d4$PHASE != "ADNI DOD",]

d4$VISCODE2 <- NA
for (i in 1:nrow(d4))
{
  t <- d[d$RID==d4$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$EXAMDATE - d4$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    next
  }
  else
  {
    d4$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}
d4$BATCH <- "UPENNBIOMK12"
write.csv(d4,"C://Users//wanya//Desktop//BIOMK//UPENNBIOMK12_v1.csv")

##
d5 <- read.csv("C://Users//wanya//Desktop//BIOMK//UPENNBIOMK13.csv")
d5$EXAMDATE <- mdy(d5$EXAMDATE)

t <- read.csv("C://Users//wanya//Desktop//BIOMK//results.csv")
t$EXAMDATE <- dmy(t$EXAMDATE)
t <- unique(t)
d5$PHASE <- NA
for(i in 1:nrow(d5))
{
  t1 <- t[t$RID == d5$RID[i],]
  t1$diff <- abs(t1$EXAMDATE-d5$EXAMDATE[i])
  if(min(t1$diff>90))
  {
    next
  }
  else
  {
    d5$PHASE[i] <- t1[t1$diff == min(t1$diff),]$PHASE[1]
  }
}
d5$VISCODE2 <- NA
for (i in 1:nrow(d5))
{
  t <- d[d$RID==d5$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$EXAMDATE - d5$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    next
  }
  else
  {
    d5$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}
d5$BATCH <- "UPENNBIOMK13"
write.csv(d5,"C://Users//wanya//Desktop//BIOMK//UPENNBIOMK13_v1.csv")

##
d6 <- data.frame(read_excel("C://Users//wanya//Desktop//BIOMK//UPenn_BIOMK9TO13_MASTERFILE_Roche.xlsx"))
d6 <- d6[rowSums(is.na(d6)) != ncol(d6), ]
d6[is.na(d6$COMMENT),]$COMMENT <- "NA"

d6[d6$COMMENT == "p-Tau <8",]$COMMENT <- "PTau<8"
d6[d6$COMMENT == "p-Tau >120",]$COMMENT <- "PTau>120"

write.csv(d6,"C://Users//wanya//Desktop//BIOMK//UPenn_BIOMK9TO13_MASTERFILE_Roche.csv")

##############################

d <- data.frame(read_excel("C://Users//wanya//Desktop//AMPRION//ADNI_AMPRION_SAA_DATA.xlsx"))
d$id <- 1:nrow(d)
d[d$Specimen.ID == "DA80FL8R01",]$Specimen.ID <- "DA80FL8R-01"
d[d$Specimen.ID == "REPLICATE 1",]$Specimen.ID <- "HA8080DP-02"
d[d$Specimen.ID == "REPLICATE 2",]$Specimen.ID <- "KA8080W0-06"
d[d$Specimen.ID == "REPLICATE 3",]$Specimen.ID <- "FA807QCG-02"
d[d$Specimen.ID == "REPLICATE 4",]$Specimen.ID <- "CA807QDJ-02"
d[d$Specimen.ID == "REPLICATE 5",]$Specimen.ID <- "GA806Z3H-03"
d[d$Specimen.ID == "REPLICATE 6",]$Specimen.ID <- "FA8078D1-02"
d[d$Specimen.ID == "REPLICATE 7",]$Specimen.ID <- "AA8078F5-02"
d[d$Specimen.ID == "REPLICATE 8",]$Specimen.ID <- "BA8078GC-02"
d[d$Specimen.ID == "REPLICATE 9",]$Specimen.ID <- "GA80G1P3-02"
d[d$Specimen.ID == "REPLICATE 10",]$Specimen.ID <- "DA80G23R-02"
d[d$Specimen.ID == "REPLICATE 11",]$Specimen.ID <- "DA80G24X-02"
d[d$Specimen.ID == "REPLICATE 12",]$Specimen.ID <- "GA80G2F4-02"
d[d$Specimen.ID == "REPLICATE 13",]$Specimen.ID <- "KA80G2TJ-02"
d[d$Specimen.ID == "REPLICATE 14",]$Specimen.ID <- "KA80G2VV-02"
d[d$Specimen.ID == "REPLICATE 15",]$Specimen.ID <- "EA80G6PL-02"
d[d$Specimen.ID == "REPLICATE 16",]$Specimen.ID <- "FA80G6R2-02"
d[d$Specimen.ID == "REPLICATE 17",]$Specimen.ID <- "CA80G6S7-02"
d[d$Specimen.ID == "REPLICATE 18",]$Specimen.ID <- "GA80G7CK-02"
d[d$Specimen.ID == "REPLICATE 19",]$Specimen.ID <- "AA80G7WZ-02"
d[d$Specimen.ID == "REPLICATE 20",]$Specimen.ID <- "BA80G7ND-02"


d1 <- read.csv("C://Users//wanya//Desktop//AMPRION//results.csv")
d1$Collection.Date <- dmy(d1$Collection.Date)

names(d)[1] <- "Sample_ID"
names(d1)[2] <- "Sample_ID"

d <- merge(d,d1,by = "Sample_ID")
names(d)[8] <- "EXAMDATE"
names(d)[7] <- "RID"

d1 <- force(inventory)
d1 <- d1[d1$EVENT == "CSF",][,c(1,4,8)]
d1$EXAMDATE <- mdy(d1$EXAMDATE)

d$VISCODE2 <- NA
for(i in 1:nrow(d))
{
  t <- d1[d1$RID == d$RID[i],]
  if(nrow(t)==0)
  {
    next
  }
  t$diff <- abs(t$EXAMDATE - d$EXAMDATE[i])
  if(min(t$diff)[1]>90)
  {
    next
  } else
  {
    d$VISCODE2[i] <- t[t$diff == min(t$diff),]$VISCODE2[1]
  }
}

write.csv(d,"C://Users//wanya//Desktop//AMPRION//ADNI_AMPRION_SAA_DATA_unblind.csv")

##############################

d1 <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_location.CSV")
d1$Global_ID <- substr(d1$Global_ID,1,nchar(d1$Global_ID)-3)
names(d1)[1] <- "GID"

d2 <- read.csv("C://Users//wanya//Desktop//BC_NSC//file1179.CSV")
d2 <- d2[,c(2,3,26)]

d3 <- merge(d1,d2,by = "GID")

d <- read.csv("C://Users//wanya//Desktop//update available+//amyloid_PET_min3Freq_gap6month_atleast6aliquot.csv")

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_location.CSV")
d1 <- read.csv("C://Users//wanya//Desktop//BC_NSC//results.csv")
names(d1)[3] <- "Global_ID"

d2 <- merge(d,d1,by = "Global_ID")
d2 <- d2[!is.na(d2$id),]

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//2FBPSub.csv")
d <- d[,c(1,2)]
d$id <- 1:nrow(d)
d$EXAMDATE <- mdy(d$EXAMDATE)
d1 <- read.csv("C://Users//wanya//Desktop//BC_NSC//results.csv")
names(d1) <- c("RID","EXAMDATE","GID")
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d1$GID <- substr(d1$GID,1,nchar(d1$GID)-3)


d2 <- unique(merge(d,d1,by = c("RID","EXAMDATE")))
d3 <- read.csv("C://Users//wanya//Desktop//BC_NSC//2FBPSub_v1_location.csv")
d2$GID <- paste0(d2$GID,"-05")
d3 <- merge(d2,d3,by = "GID")

##############################

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_location_v2.csv")

d1 <- d[!is.na(d$id),]
d1$order <- 1:nrow(d1)
t <- read.csv("C://Users//wanya//Desktop//BC_NSC//results.csv")
names(t) <- c("RID","EXAMDATE","Global_ID")

d2 <- merge(d1,t,by = "Global_ID")

##############################

d <- data.frame(read_excel("C://Users//wanya//Desktop//Roche//New folder//batch3.xlsx"))
d <- d[,-c(10)]
d$AB42 <- as.double(d$ABETA)
d$PTAU <- as.double(d$PTAU)
d$TAU <- as.double(d$TAU)
d$AB4240 <- as.double(d$AB4240)
d <- na.omit(d)

d1 <- data.frame(matrix(nrow = 1,ncol = 13))
names(d1) <- c("RID","AB40_original","AB42_original","PTAU_original","TAU_original","AB4240_original",
               "AB40_latest","AB42_latest","PTAU_latest","TAU_latest","AB4240_latest","original","latest")

for(i in seq(nrow(d)))
{
  t <- d[d$RID == d$RID[i] & d$EXAMDATE == d$EXAMDATE[i],]
  if(nrow(t)==1) next
  
  d1[i,1] <- t$RID[1]
  
  d1[i,2] <- mean(t$AB40[t$RUNDATE==min(t$RUNDATE)])
  d1[i,3] <- mean(t$AB42[t$RUNDATE==min(t$RUNDATE)])
  d1[i,4] <- mean(t$PTAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,5] <- mean(t$TAU[t$RUNDATE==min(t$RUNDATE)])
  d1[i,6] <- mean(t$AB4240[t$RUNDATE==min(t$RUNDATE)])
  
  d1[i,7] <- mean(t$AB40[t$RUNDATE==max(t$RUNDATE)])
  d1[i,8] <- mean(t$AB42[t$RUNDATE==max(t$RUNDATE)])
  d1[i,9] <- mean(t$PTAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,10] <- mean(t$TAU[t$RUNDATE==max(t$RUNDATE)])
  d1[i,11] <- mean(t$AB4240[t$RUNDATE==max(t$RUNDATE)])
  
  d1[i,12] <- min(t$RUNDATE)
  d1[i,13] <- max(t$RUNDATE)
}

d1 <- na.omit(d1)
d1 <- unique(d1)

timeDate <- as.POSIXct("2021-01-01")   # convert date to large number
t <- unclass(timeDate)[[1]]

d1 <- d1[d1$original < t & d1$latest > t,] # only 1st rundate before 2021 and last rundate after 2021 are selected
model2 <- lm(d1[,8]~d1[,3],data = d1)

p2 <- ggplot(d1,aes(AB42_original, AB42_latest)) + geom_point() + geom_smooth(method='lm') +
  annotate("text", x = 400, y = 3000, label = "AB42_2021 = 1.168*AB42_original - 86.12", hjust = 0) +
  annotate("text", x = 400, y = 2750, label = paste("R-squared:",round(summary(model2)$r.squared,4)), hjust = 0)

##############################

d <- read.csv("C://Users//wanya//Desktop//BIOMK//UPenn_BIOMK9TO13_MASTERFILE_Roche_withoutBIOMK10.csv")

d1 <- d[d$BATCH == "UPENNBIOMK9",]
d2 <- d[d$BATCH == "UPENNBIOMK11",]

d1$t <- paste(d1$RID,d1$EXAMDATE)
d2$t <- paste(d2$RID,d2$EXAMDATE)

##############################

d1 <- read.csv("C://Users//wanya//Desktop//BIOMK//Roche Elecsys ADNI1_GO_2_3 CSF.csv")
d2 <- read.csv("C://Users//wanya//Desktop//BIOMK//UCBERKELEY_AMY_6MM_12Oct2023.csv")

d2 <- d2[,c(2,5,13)]
d1$EXAMDATE <- mdy(d1$EXAMDATE)
d2$EXAMDATE <- ymd(d2$SCANDATE)
d2 <- d2[!is.na(d2$SUMMARY_SUVR),]

d1$SUMMARY_SUVR <- NA
d1$result <- NA
for(i in 1:nrow(d1))
{
  t <- d2[d2$RID == d1$RID[i],]
  if(nrow(t) == 0)
  {
    d1$SUMMARY_SUVR[i] <- NA
    next
  }
  t$diff <- abs(t$EXAMDATE - d1$EXAMDATE[i])
  if(min(t$diff)>180)
  {
    d1$SUMMARY_SUVR[i] <- NA
    next
  } else 
  {
    d1$SUMMARY_SUVR[i] <- t[t$diff == min(t$diff),]$SUMMARY_SUVR[1]
    if(d1$SUMMARY_SUVR[i]<1.11)
    {
      d1$result[i] <- "-"
    } else
    {
      d1$result[i] <- "+"
    }
  }
}

write.csv(d1,"C://Users//wanya//Desktop//BIOMK//UPenn_BIOMK9TO13_MASTERFILE_Roche_v2.csv")

d2 <- d1[!is.na(d1$ABETA42) & !is.na(d1$SUMMARY_SUVR),]

d3 <- read.csv("C://Users//wanya//Desktop//BIOMK//DXSUM_PDXCONV_ADNIALL_12Oct2023.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d3$DXCURREN[is.na(d3$DXCURREN)] = d3$DXCHANGE[is.na(d3$DXCURREN)]
d3$DXCURREN[is.na(d3$DXCURREN)] = d3$DIAGNOSIS[is.na(d3$DXCURREN)]

library(Epi) 
d3$DX      = Relevel(factor(d3$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d3$DODX    = d3$DATE
d3$EXAMDATE <- ymd(d3$EXAMDATE)

d2$DX <- NA
for(i in 1:nrow(d2))
{
  t <- d3[d3$RID == d2$RID[i],]
  t <- t[!is.na(t$EXAMDATE),]
  if(nrow(t)==0)
  {
    d2$DX[i] <- NA
    next
  }
  t$diff <- abs(t$EXAMDATE - d2$EXAMDATE[i])
  if(min(t$diff)>180)
  {
    d2$DX[i] <- NA
    next
  } else
  {
    d2$DX[i] <- as.character(t[t$diff == min(t$diff),]$DX[1])
  }
}

d3 <- d2[!is.na(d2$DX),]
d3[d3$DX == "NL",]$DX <- "HC"
names(d3)[14] <- "Diagnosis"
d3$rid_date <- paste(d3$RID,d3$EXAMDATE)
d3 <- d3[!duplicated(d3$rid_date),]
## ABeta 42
plot1 <- ggplot(d3, aes(x=ABETA42, y=SUMMARY_SUVR, color=Diagnosis)) + 
  geom_point(alpha=0.75,size=1) + labs(
    title = "Aβ42 vs FBP PET",
    subtitle = "n = 2067",
    # caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 1",
    x = "Aβ42 (pg/mL)",
    y = "FBP SUVR",
    colour = "Diagnosis"
  ) + theme_bw()

plot1 <- plot1 + geom_hline(yintercept=1.1,linetype = 2, color = 2) + annotate("text", x=4500, y=1.15, label="FBP SUVR = 1.1") +
  geom_vline(xintercept=980,linetype = 2, color = 2) + annotate("text", x=1450, y=2.5, label="Abeta 42 = 980")

plot1 + annotate("text", x = 500, y = 2.875, label = "FBP+/CSF+\nn = 852") +
  annotate("text", x = 500, y = 0.75, label = "FBP-/CSF+\nn = 194") + 
  annotate("text", x = 4500, y = 2.875, label = "FBP+/CSF-\nn = 155") + 
  annotate("text", x = 4500, y = 0.75, label = "FBP-/CSF-\nn = 866")

## Ptau 181 / ABeta 42
d4 <- d2[!is.na(d2$ABETA42) & !is.na(d2$PTAU) & !is.na(d2$DX),]
d4$Ptau181_Abeta42 <- d4$PTAU/d4$ABETA42
d4$rid_date <- paste(d4$RID,d4$EXAMDATE)
d4 <- d4[!duplicated(d4$rid_date),]
d4[d4$DX == "NL",]$DX <- "HC"
names(d4)[14] <- "Diagnosis"
plot2 <- ggplot(d4, aes(x=Ptau181_Abeta42, y=SUMMARY_SUVR, color=Diagnosis)) + 
  geom_point(alpha=0.75,size=1) + labs(
    title = "Ptau181/Abeta42 vs FBP PET",
    subtitle = "n = 2053",
    # caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 2",
    x = "Ptau181/Abeta42 Ratio",
    y = "FBP SUVR",
    colour = "Diagnosis"
  ) + theme_bw()

plot2 <- plot2 + geom_hline(yintercept=1.1,linetype = 2, color = 2) + annotate("text", x=0.2, y=1.15, label="FBP SUVR = 1.1") +
  geom_vline(xintercept=0.025,linetype = 2, color = 2) + annotate("text", x=0.0625, y=2.5, label="Ptau181/Abeta42 = 0.025")

plot2 + annotate("text", x = 0.01, y = 2.875, label = "FBP+/Ratio-\nn = 143") +
  annotate("text", x = 0.01, y = 0.75, label = "FBP-/Ratio-\nn = 980") + 
  annotate("text", x = 0.25, y = 2.875, label = "FBP+/Ratio+\nn = 863") + 
  annotate("text", x = 0.25, y = 0.75, label = "FBP-/Ratio+\nn = 67")

## ABeta 42 / 40
d5 <- d2[!is.na(d2$ABETA40) & !is.na(d2$ABETA42) & !is.na(d2$DX) & d2$PHASE == "ADNI3",]
d5$rid_date <- paste(d5$RID,d5$EXAMDATE)
d5 <- d5[!duplicated(d5$rid_date),]
d5[d5$DX == "NL",]$DX <- "HC"
names(d5)[14] <- "Diagnosis"

d5$ab42_ab40 <- d5$ABETA42/d5$ABETA40
plot3 <- ggplot(d5, aes(x=ab42_ab40, y=SUMMARY_SUVR, color=Diagnosis)) + 
  geom_point(alpha=0.75,size=1) + labs(
    title = "Abeta42/Abeta40 vs FBP PET",
    subtitle = "n = 687",
    # caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 2",
    x = "Abeta42/Abeta40 Ratio",
    y = "FBP SUVR",
    colour = "Diagnosis"
  ) + theme_bw()

plot3 <- plot3 + geom_hline(yintercept=1.1,linetype = 2, color = 2) + annotate("text", x=0.125, y=1.15, label="FBP SUVR = 1.1") +
  geom_vline(xintercept=0.06093,linetype = 2, color = 2) + annotate("text", x=0.08, y=2, label="Abeta42/Abeta40 = 0.061")

plot3 + annotate("text", x = 0.02, y = 2.1, label = "FBP+/Ratio+\nn = 266") +
  annotate("text", x = 0.02, y = 0.8, label = "FBP-/Ratio+\nn = 67") + 
  annotate("text", x = 0.13, y = 2.1, label = "FBP+/Ratio-\nn = 18") + 
  annotate("text", x = 0.13, y = 0.8, label = "FBP-/Ratio-\nn = 336")

## abeta42/40 cutoff
out <- em(d5$ab42_ab40,"normal","normal")
confint(out)
cutoff(out)

## TAU/ab42
d3 <- d3[!is.na(d3$TAU),]
d3$TAU_ab42 <- d3$TAU/d3$ABETA42
plot1 <- ggplot(d3, aes(x=TAU_ab42, y=SUMMARY_SUVR, color=Diagnosis)) + 
  geom_point(alpha=0.75,size=1) + labs(
    title = "TAU/Abeta42 vs FBP PET",
    subtitle = "n = 2063",
    # caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 3",
    x = "TAU/Abeta42 Ratio",
    y = "FBP SUVR",
    colour = "Diagnosis"
  ) + theme_bw()

plot1 <- plot1 + geom_hline(yintercept=1.1,linetype = 2, color = 2) + annotate("text", x=2.25, y=1.15, label="FBP SUVR = 1.1") +
  geom_vline(xintercept=0.27,linetype = 2, color = 2) + annotate("text", x=0.55, y=2.5, label="TAU/Abeta42 = 0.27")

plot1 + annotate("text", x = 2.375, y = 2.75, label = "FBP+/CSF+\nn = 866") +
  annotate("text", x = 2.375, y = 0.7, label = "FBP-/CSF+\nn = 73") + 
  annotate("text", x = 0.125, y = 2.75, label = "FBP+/CSF-\nn = 141") + 
  annotate("text", x = 0.125, y = 0.7, label = "FBP-/CSF-\nn = 983")


## ROC
library(plotROC)

d6 <- d2[!is.na(d2$DX),]
d6[d6$DX == "NL",]$DX <- "HC"
names(d6)[14] <- "Diagnosis"
d6$rid_date <- paste(d6$RID,d6$EXAMDATE)
d6 <- d6[!duplicated(d6$rid_date),]

d6$Ptau181_Abeta42 <- d6$PTAU/d6$ABETA42*100
d6$ab42_ab40 <- d6$ABETA42/d6$ABETA40*100
d6[!d6$PHASE == "ADNI3",]$ab42_ab40 <- NA

d6$result2[d6$SUMMARY_SUVR < 1.11] <- 0
d6$result2[d6$SUMMARY_SUVR >= 1.11] <- 1

d6 <- d6[]


longtest <- melt_roc(d6, "result2", c("ABETA42","Ptau181_Abeta42","ab42_ab40"))

p <- ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc() +
  ggtitle(gsub("  ", 2067, "pT181P and pT181P.QUANTERIX and pT181P.ROCHE (n=  )")) + 
  geom_abline(slope=1, intercept = 0, linetype = "dashed") + scale_color_manual(values=c("#CC6666","#9999CC","#1d91c0")) + 
  scale_x_continuous("1 - Specificity",breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity",breaks = seq(0, 1, by = .1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

library(OptimalCutpoints)
results1 <- optimal.cutpoints(X = "ABETA42", status = "result2",tag.healthy = 0, method = "Youden", data = d6)
results2 <- optimal.cutpoints(X = "Ptau181_Abeta42", status = "result2",tag.healthy = 0, method = "Youden", data = d6)
results3 <- optimal.cutpoints(X = "ab42_ab40", status = "result2",tag.healthy = 0, method = "Youden", data = d6)

p+geom_point(aes(x=1-data.frame(summary(results1)[5])[3,1], y=data.frame(summary(results1)[5])[2,1]),shape = 18,size = 3,color="slateblue1") + # manually change
  geom_point(aes(x=1-data.frame(summary(results2)[5])[3,1], y=data.frame(summary(results2)[5])[2,1]),shape = 18,size = 3,color="tomato") +
  geom_point(aes(x=1-data.frame(summary(results3)[5])[3,1], y=data.frame(summary(results3)[5])[2,1]),shape = 18,size = 3,color="darkblue") +
  geom_label(label=paste("95%CI ABETA42: ",data.frame(summary(results1)[5])[1,2],
                         "\n95%CI Ptau181/Abeta42 %: ",data.frame(summary(results2)[5])[1,2],
                         "\n95%CI Abeta42/Abeta40 %: ",data.frame(summary(results3)[5])[1,2]),
             x=0.5,
             y=0.2,
             label.padding = unit(0.5, "lines"), # Rectangle size around 
             label.size = 0.3,
             color = "black",
             fill="white",
             hjust = 0
  )
# https://rdrr.io/cran/plotROC/man/geom_roc.html
p <- ggplot(t, aes(m = M, d = D, color = Name)) + geom_roc()+ style_roc() +
  ggtitle("ABETA 42, Abeta42/40 ratio and Ptau181/Abeta42 ratio") + 
  geom_abline(slope=1, intercept = 0, linetype = "dashed") + scale_color_manual(values=c("#CC6666","#9999CC","#1d91c0")) + 
  scale_x_continuous("1 - Specificity",breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity",breaks = seq(0, 1, by = .1)) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=15))

##############################

d <- read.csv("C://Users//wanya//Desktop//C2N//C2N_Plasma_MainCohort.csv")

d$EXAMDATE <- ymd(d$EXAMDATE)

d1 <- read.csv("C://Users//wanya//Desktop//C2N//results (1).csv")
names(d1) <- c("GID","RID","EXAMDATE")
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d1$GID <- substr(d1$GID,1,nchar(d1$GID)-3)
d1 <- unique(d1)

d2 <- merge(d,d1,by = c("RID","EXAMDATE"))
d2[d2$RID == 4565,]$GID <- "EA808TQ2"
d2 <- unique(d2)

##############################

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_location_v3.csv")

d$id2 <- d$id+5
names(d)[1] <- "GID"
d$GID <- substr(d$GID,1,nchar(d$GID)-2)

for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$id2)[i]),sep = "")
}

##############################

d <- read.csv("C://Users//wanya//Desktop//C2N//C2N_Plasma_AutopsyCohort.csv")
d1 <- read.csv("C://Users//wanya//Desktop//C2N//results (1).csv")
d$EXAMDATE <- ymd(d$EXAMDATE)
d1$EXAMDATE <- dmy(d1$EXAMDATE)
d1$GID <- unique(substr(d1$GID,1,nchar(d1$GID)-3))
d2 <- merge(d,d1,by = c("RID","EXAMDATE"))

t <- read.csv("C://Users//wanya//Desktop//C2N//location_100.csv")
t$Storage.Unit <- substr(t$Storage.Unit,nchar(t$Storage.Unit)-1,nchar(t$Storage.Unit))
t[t$Container == "BOX2",]$Container <- "BOX 2"
t[t$Container == "BOX3",]$Container <- "BOX 3"
t[t$Level == "6 X 6 SHELF  3 PLASMA",]$Level <- "6 X 6 SHELF 3 PLASMA"
t[t$Level == "6X6 SHELF 4 PLASMA030816",]$Level <- "6 X 6 SHELF 4 PLASMA"
t[t$Level == "6 X 6 SHELF  4 PLASMA",]$Level <- "6 X 6 SHELF 4 PLASMA"


t$Sub.Level <- sapply(t$Sub.Level,function(i) strsplit(i," ")[[1]][5])
t$Container <- sapply(t$Container,function(i) strsplit(i," ")[[1]][2])
t$Level <- sapply(t$Level,function(i) strsplit(i," ")[[1]][5])

##############################

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_manifest_v1.csv")

x <- d1

t <- data.frame(table(d$RID))

t <- read.csv("C://Users//wanya//Downloads//results (11).csv")

d3 <- read.csv("C://Users//wanya//Desktop//BIOMK//DXSUM_PDXCONV_ADNIALL_12Oct2023.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d3$DXCURREN[is.na(d3$DXCURREN)] = d3$DXCHANGE[is.na(d3$DXCURREN)]
d3$DXCURREN[is.na(d3$DXCURREN)] = d3$DIAGNOSIS[is.na(d3$DXCURREN)]

library(Epi) 
d3$DX      = Relevel(factor(d3$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d3$DODX    = d3$DATE
d3$EXAMDATE <- ymd(d3$EXAMDATE)
d3 <- d3[,c(2,4,9)]

d2 <- d1

d2$DX <- NA
for(i in 1:nrow(d2))
{
  t <- d3[d3$RID == d2$RID[i],]
  t <- t[!is.na(t$EXAMDATE),]
  if(nrow(t)==0)
  {
    d2$DX[i] <- NA
    next
  }
  t$diff <- abs(t$EXAMDATE - d2$EXAMDATE[i])
  if(min(t$diff)>180)
  {
    d2$DX[i] <- NA
    next
  } else
  {
    d2$DX[i] <- as.character(t[t$diff == min(t$diff),]$DX[1])
  }
}



d <- data.frame(read_excel("C://Users//wanya//Desktop//BC_NSC//1065_manifest.xlsx"))
d1 <- read.csv("C://Users//wanya//Desktop//BC_NSC//results.csv")
names(d1) <- c("RID","EXAMDATE","GID")

d <- merge(d,d1,by = "GID")
d2 <- data.frame(unique(d$RID),1:length(unique(d$RID)))
names(d2) <- c("RID","id2")

d3 <- merge(d,d2,by = "RID")

##############################

library(readxl)
library(lubridate)
library(ADNIMERGE)

d <- data.frame(read_excel("C://Users//wanya//Desktop//Janssen//t.xlsx"))

d$id <- 1:nrow(d)

d[d$Sample.ID == "QCA",]$Sample.ID <- "FSQQCA"
d[d$Sample.ID == "QCB",]$Sample.ID <- "FSQQCB"
d[d$Sample.ID == "EQC1",]$Sample.ID <- "FSQEQC1"
d[d$Sample.ID == "EQC2",]$Sample.ID <- "FSQEQC2"

d$Sample.ID <- substr(d$Sample.ID,4,nchar(d$Sample.ID))

d1 <- read.csv("C://Users//wanya//Downloads//results (12).csv")
names(d)[2] <- "Sample_ID"
names(d1)[1] <- "Sample_ID"
names(d1)[2] <- "RID"
names(d1)[3] <- "EXAMDATE"

d <- merge(d,d1,by = "Sample_ID",all.x = TRUE)

d$EXAMDATE <- dmy(d$EXAMDATE)

d2 <- force(inventory)
d2 <- d2[d2$EVENT == "Blood",]
d2$EXAMDATE <- mdy(d2$EXAMDATE)
d2 <- d2[,c(1,4,8)]

d$VISCODE2 <- NA
for (i in 1:nrow(d))
{
  t <- na.omit(d2[d2$RID==d$RID[i],])
  if(nrow(t)==0)
  {
    d$VISCODE2[i] <- NA
    next
  }
  t$diff <- abs(t$EXAMDATE - d$EXAMDATE[i])
  if(min(t$diff)>90)
  {
    d$VISCODE2[i] <- NA
    next
  }
  else
  {
    d$VISCODE2[i] <- t[t$diff==min(t$diff),]$VISCODE2[1]
  }
}

##############################

d1 <- read.csv("C://Users//wanya//Desktop//BIOMK//Roche Elecsys ADNI1_GO_2_3 CSF.csv")
d1$EXAMDATE <- mdy(d1$EXAMDATE)

d = read.csv("C://Users//wanya//Downloads//DXSUM_PDXCONV_ADNIALL_08Nov2023.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]   
d$DXCURREN[is.na(d$DXCURREN)] = d$DXCHANGE[is.na(d$DXCURREN)]
d$DXCURREN[is.na(d$DXCURREN)] = d$DIAGNOSIS[is.na(d$DXCURREN)]
library(Epi)
d$DX      = Relevel(factor(d$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d$DODX    = d$DATE
d <- d[,c(2,4,9)]
d$EXAMDATE <- ymd(d$EXAMDATE)
d$DX <- as.character(d$DX)

d1$DX <- NA
for (i in 1:nrow(d1))
{
  t <- na.omit(d[d$RID==d1$RID[i],])
  if(nrow(t)==0)
  {
    d1$DX[i] <- NA
    next
  }
  t$diff <- abs(t$EXAMDATE - d1$EXAMDATE[i])
  if(min(t$diff)>180)
  {
    d1$DX[i] <- NA
    next
  }
  else
  {
    d1$DX[i] <- t[t$diff==min(t$diff),]$DX[1]
  }
}

##############################

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_location_v3.csv")

d$id2 <- d$id+5
names(d)[1] <- "GID"
d$GID <- substr(d$GID,1,nchar(d$GID)-2)

for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$id2)[i]),sep = "")
}

##############################

d <- read.csv("C://Users//wanya//Desktop//BC_NSC//1179_manifest_v2.csv")

d$aliquot <- substr(d$GID,10,nchar(d$GID))
d$aliquot <- as.integer(d$aliquot)
d$a1 <- d$aliquot - 2

d$GID <- substr(d$GID,1,nchar(d$GID)-2)
for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$a1)[i]),sep = "")
}
write.csv(d,"C://Users//wanya//Desktop//BC_NSC//1179_manifest_v2_r.csv")


##############################
d <- read.csv("O://YW//1179_Manifest_Roche.csv")
d$aliquot <- substr(d$GID,10,nchar(d$GID))
d$aliquot <- as.integer(d$aliquot)
d$a1 <- d$aliquot - 4
d$GID <- substr(d$GID,1,nchar(d$GID)-2)
for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$a1)[i]),sep = "")
}
write.csv(d,"O://YW//1179_Manifest_C1.csv")

##############################

library(readxl)
library(ADNIMERGE)
library(lubridate)

d <- data.frame(read_excel("C://Users//wanya//Desktop//unblind//Janssen//janssen.xlsx"))

d1 <- read.csv("C://Users//wanya//Downloads//results (19).csv")
names(d1) <- c("Sample_ID","RID","EXAMDATE")

d <- merge(d,d1,by = "Sample_ID", all.x = TRUE)
d$EXAMDATE <- dmy(d$EXAMDATE)
d$RID <- as.integer(d$RID)

d2 <- force(inventory)
d2 <- d2[d2$EVENT == "Blood",]
d2 <- d2[,c(1,4,8)]
d2$EXAMDATE <- mdy(d2$EXAMDATE)

d$VISCODE2 <- NA
for(i in 1:nrow(d))
{
  t <- na.omit(d2[d2$RID == d$RID[i],])
  if(nrow(t) == 0)
  {
    next
  }
  t$diff <- abs(t$EXAMDATE - d$EXAMDATE[i])
  if(min(t$diff)>180)
  {
    next
  } else
  {
    d$VISCODE2[i] <- t[t$diff == min(t$diff),]$VISCODE2[1]
  }
}

##############################

library(lubridate)
d <- read.csv("C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints.csv")
d$EXAMDATE <- mdy(d$EXAMDATE)

d1 <- read.csv("C://Users//wanya//Downloads//results (20).csv")
d1$Global.Specimen.ID <- substr(d1$Global.Specimen.ID,1,nchar(d1$Global.Specimen.ID)-3)
d1 <- unique(d1)
names(d1) <- c("GID","EXAMDATE","RID")
d1$EXAMDATE <- dmy(d1$EXAMDATE)

d <- merge(d,d1,by = c("RID","EXAMDATE"),all.x = TRUE)
write.csv(d,"C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints_v1.csv")

d$GID <- paste0(d$GID,"-05")
d1 <- read.csv("C://Users//wanya//Downloads//results (21).csv")
names(d1) <- c("GID","F","S","R","B","P")

d <- merge(d,d1,by = "GID",all.x = TRUE)
write.csv(d,"C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints_location.csv")

##############################

d <- read.csv("C://Users//wanya//Downloads//SHManifest (2).csv")
d1 <- read.csv("C://Users//wanya//Desktop//AMPRION//AMP1//manifest_v1.csv")

d <- read.csv("C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints_location.csv")

d <- d[d$RID != "Shipped:",]
d1 <- read.csv("O://YW//1179_Manifest_Roche.csv")

##############################

d <- read.csv("C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints_location.csv")

d$GID <- substr(d$GID,1,nchar(d$GID)-2)
for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$id)[i]),sep = "")
}
write.csv(d,"C://Users//wanya//Desktop//AMPRION//AMP2//AMPRION_ASYN_SAA_LongitudinalTimePoints_location.csv")

##############################

d <- read.csv("O://YW//1179_Manifest_C1.csv")
d$a1 <- d$a1 - 1

d$GID <- substr(d$GID,1,nchar(d$GID)-2)
for (i in seq(nrow(d)))
{
  d$GID[i] <- paste(d$GID[i],toString(sprintf("%02d",d$a1)[i]),sep = "")
}

write.csv(d,"O://YW//1179_Manifest_C2.csv")

##############################

d <- data.frame(read_excel("O://YW//label.xlsx"))

d$a1 <- substr(d$GID,nchar(d$GID)-1,nchar(d$GID))

d$a1 <- as.integer(d$a1)
d$a1 <- d$a1 + 1

d$a2 <- d$a1 + 1

d$GID <- substr(d$GID,1,nchar(d$GID)-2)
for (i in seq(nrow(d)))
{
  d$GID2[i] <- paste(d$GID2[i],toString(sprintf("%02d",d$a2)[i]),sep = "")
}

##############################

d <- data.frame(read_excel("C://Users//wanya//Desktop//C2N//C2N_manifest_100_ship.xlsx"))
d1 <- read.csv("C://Users//wanya//Downloads//SHManifest (4).csv")

##############################

test <- function (x = c("a","b"))
{
  x <- match.arg(x,1)
  print(x)
}

##############################
library(lubridate)

d <- read.csv("C://Users//wanya//Desktop//C2N//C2N_Plasma_AutopsyCohort.csv")
d <- d[d$NP == "Available",]
d$EXAMDATE <- ymd(d$EXAMDATE)

d1 <- read.csv("C://Users//wanya//Downloads//results (22).csv")
d1 <- unique(d1)
d1$Collection.Date <- dmy(d1$Collection.Date)
for (i in seq(nrow(d)))
{
  d$RID2[i] <- toString(sprintf("%04d",d$RID)[i])
}

names(d1)[1] <- "RID2"
d2 <- merge(d,d1,by = "RID2",all.x = TRUE)

d <- read.csv("C://Users//wanya//Downloads//temp.csv")
d$EXAMDATE <- mdy(d$EXAMDATE)
d$Collection.Date <- mdy(d$Collection.Date)
d <- d[!d$EXAMDATE == d$Collection.Date,]

x <- data.frame(table(d$RID))
d$mark <- NA
for(i in x$Var1)
{
  t <- d[d$RID == i,]
  t$diff <- t$EXAMDATE - t$Collection.Date
  t[t$diff == max(t$diff),]$mark <- "bl"
  t[t$diff == min(t$diff),]$mark <- "last"
  t1 <- rbind(t1,t)
}


d <- read.csv("C://Users//wanya//Downloads//results (23).csv")
d1 <- data.frame(table(d$ID1))
for(i in 2:nrow(d))
{
  if(d$ID1[i] == d$ID1[i-1])
  {
    d$Collection.Date[i] <- NA
  }
}
d <- d[!is.na(d$Collection.Date),]

##############################

d <- read.csv("C://Users//wanya//Downloads//results (25).csv")
d <- unique(d)
d$type <- NA
d[d$Primary == "BLD" & d$Additive == "NON",]$type <- "SER"
d[d$Primary == "BLD" & d$Additive == "EDT",]$type <- "PLA"
d[d$Primary == "CSF",]$type <- "CSF"

d1 <- read.csv("C://Users//wanya//Downloads//DXSUM_PDXCONV_ADNIALL_02Feb2024.csv")[,c("Phase","RID","VISCODE","EXAMDATE","USERDATE","DXCURREN","DXCHANGE","DIAGNOSIS")]
d1$DXCURREN[is.na(d1$DXCURREN)] = d1$DXCHANGE[is.na(d1$DXCURREN)]
d1$DXCURREN[is.na(d1$DXCURREN)] = d1$DIAGNOSIS[is.na(d1$DXCURREN)]
library(Epi) 
d1$DX      = Relevel(factor(d1$DXCURREN), list(NL=c(1,7,9), MCI=c(2,4,8), AD=c(3,5,6), EMCI=0))
d1$DODX    = d1$DATE

library(dplyr)
d2 <- d1 %>% group_by(RID) %>% filter(EXAMDATE == min(EXAMDATE, na.rm = TRUE))
d1 <- d1[,c(2,9)]

d <- merge(d,d1,by = "RID",all.x = TRUE)
##############################

d <- read.csv("C://Users//wanya//Downloads//2023 ship summary//Amprion//SHManifest3.csv")
d <- merge(d,d2,by = "RID",all.x = TRUE)
table(d$DX)

##############################
library(lubridate)

d <- read.csv("C://Users//wanya//Downloads//results (27).csv")
names(d)[1] <- "RID"
names(d)[2] <- "EXAMDATE"
d <- unique(d)
d$EXAMDATE <- dmy(d$EXAMDATE)
d <- d[!(d$Primary == "BLD" & d$Additive == "NON"),]

db <- d[d$Primary == "BLD",]
dc <- d[d$Primary == "CSF",]

library(dplyr)
d1 <- db %>% group_by(RID) %>% filter(EXAMDATE == min(EXAMDATE))
d2 <- db %>% group_by(RID) %>% filter(EXAMDATE == max(EXAMDATE))
d3 <- db %>% group_by(RID) %>% filter(EXAMDATE > min(EXAMDATE) & EXAMDATE < max(EXAMDATE))

dt <- merge(d1,d2,by = "RID")
dt$diff <- dt$EXAMDATE.y - dt$EXAMDATE.x
names(dt)[2] <- "time1"
names(dt)[5] <- "time2"

d1 <- read.csv("C://Users//wanya//Downloads//ADNI_DOD_07Feb2024.csv")
d1 <- d1[d1$VISCODE == "bl",][,c(1,9)]
names(d1)[1] <- "RID"
d1$EXAMDATE <- mdy(d1$EXAMDATE)
d1 <- db %>% group_by(RID) %>% filter(EXAMDATE == min(EXAMDATE))

dx <- merge(dt,d1,by = "RID",all.x = TRUE)
dx$diff2 <- dx$time1 - dx$EXAMDATE

dx <- read.csv("C://Users//wanya//Downloads//ADNI_DOD_07Feb2024.csv")

temp <- function(i)
{
  if(i<200)
  {
    dx$ct <- "0~200"
  } else if(i < 400)
  {
    dx$ct <- "200~400"
  } else if(i < 600)
  {
    dx$ct <- "400~600"
  } else if(i < 800)
  {
    dx$ct <- "600~800"
  } else
  {
    dx$ct <- "Above 800"
  }
}

dx$ct <- unlist(lapply(dx$Gap.day., temp))

ggplot(dx, aes(x=Gap.day.)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100))

diamonds %>% group_by(cut) %>% summarise(mean = mean(price))
tapply(diamonds$price, diamonds$cut, mean)
