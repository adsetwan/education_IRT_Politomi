######################
### Data Preparation #
######################
library(xlsx)
dat <- read.xlsx("D:\\Adi\\Seleksi Assessment Analytics - Ruang Guru\\data.xlsx", sheetIndex = 1)
jawaban <- read.xlsx("D:\\Adi\\Seleksi Assessment Analytics - Ruang Guru\\data.xlsx", sheetIndex = 2)
str(jawaban)
table(jawaban$Correct.Answer)
head(dat)
str(dat)
ubahA <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == "A"){
      a[i] = 2
    }
    else if(x[i] == "-"){
      a[i] = 1
    }
    else a[i] = 0
  }
  return(a)
}
ubahB <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == "B"){
      a[i] = 2
    }
    else if(x[i] == "-"){
      a[i] = 1
    }
    else a[i] = 0
  }
  return(a)
}
ubahC <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == "C"){
      a[i] = 2
    }
    else if(x[i] == "-"){
      a[i] = 1
    }
    else a[i] = 0
  }
  return(a)
}
ubahD <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == "D"){
      a[i] = 2
    }
    else if(x[i] == "-"){
      a[i] = 1
    }
    else a[i] = 0
  }
  return(a)
}
ubahE <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == "E"){
      a[i] = 2
    }
    else if(x[i] == "-"){
      a[i] = 1
    }
    else a[i] = 0
  }
  return(a)
}
ubah <- function(x, jawaban){
  b <- NULL
  if(jawaban == "A"){
    b = ubahA(x)
  }
  else if(jawaban == "B"){
    b = ubahB(x)
  }
  else if(jawaban == "C"){
    b = ubahC(x)
  }
  else if(jawaban == "D"){
    b = ubahD(x)
  }
  else b = ubahE(x)
  return(b)
}
ganti <- function(data, jawaban){
  hasil <- matrix(rep(0, ncol(data)*nrow(data)), ncol = ncol(data), nrow = nrow(data))
  for(i in 1:length(jawaban)){
    hasil[,i] <- ubah(data[,i], jawaban[i])
  }
  return(hasil)
}
dat2 <- ganti(dat, jawaban$Correct.Answer)
head(dat2,3)
head(dat,3)
jawaban$Correct.Answer
dat2 <- as.data.frame(dat2)
colnames(dat2)<- paste("no", 1:20, sep="")
head(dat,3)
head(dat2,3)
#############################
### Statistika Deskriptif ###
#############################
str(dat2)
apply(dat2, 2, table)
#frekuensi jawaban toko
library(tidyverse)
frek <- dat2 %>% gather(nomor, intensitas, "no1":"no20")
frek <- frek %>% group_by(nomor,intensitas) %>% count(intensitas)
frek <- as.data.frame(frek)
str(frek)
frek$intensitas <- as.factor(frek$intensitas)
bah <- function(x){
  a <- NULL
  for(i in 1:length(x)){
    if(x[i] == 0){
      a[i] = "Salah"
    }
    else if(x[i] == 1){
      a[i] = "Tidak menjawab"
    }
    else a[i] = "Benar"
  }
  return(a)
}
frek$intensitas <- bah(frek$intensitas)
head(frek)
str(frek)
unique(frek$intensitas)
frek$intensitas <- factor(frek$intensitas, levels =c("Salah", "Tidak menjawab", "Benar"))
head(frek)
str(frek)
frek$Soal <- factor(frek$Soal, levels = c(paste("no",1:20, sep = "")))
colnames(frek) <- c("Soal", "Hasil", "n")
#Percentage Stacked Bar Chart
library(ggplot2)
ggplot(frek, aes(fill=Hasil, y=n, x=Soal)) + 
  geom_bar(position="fill", stat="identity") + xlab("Soal")+ylab("Proporsi")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
#######################
### Kecocokan Model ###
#######################
#PCM
library(mirt)
pcm_fit <- mirt(data = dat2, model = "F=1-20",
                itemtype = "Rasch", SE = TRUE)
ifit_pcm <- itemfit(pcm_fit)
ifit_pcm
write.xlsx(data.frame(round(ifit_pcm$S_X2,2), round(ifit_pcm$p.S_X2,2)),
           "D:\\Adi\\Seleksi Assessment Analytics - Ruang Guru\\fitpcm.xlsx")
table(ifelse(round(ifit_pcm$p.S_X2 >= 0.05,2), "Cocok", "Tidak Cocok"))
#GPCM
gpcm_fit <- mirt(data = dat2, model = "F = 1-20",
                 itemtype = "gpcm", SE = TRUE)
ifit_gpcm <- itemfit(gpcm_fit)
ifit_gpcm
write.xlsx(data.frame(round(ifit_gpcm$S_X2,2), round(ifit_gpcm$p.S_X2,2)),
           "D:\\Adi\\Seleksi Assessment Analytics - Ruang Guru\\fitgpcm.xlsx")

table(ifelse(ifit_gpcm$p.S_X2 >= 0.05, "Cocok", "Tidak Cocok"))
#GRM
grm_fit <- mirt(data = dat2, model = "F = 1-20",
                itemtype = "graded", SE = TRUE)
ifit_grm <- itemfit(grm_fit)
ifit_grm
table(ifelse(ifit_grm$p.S_X2 >= 0.05, "Cocok", "Tidak Cocok"))
#Perbandingan
M2(pcm_fit)
M2(gpcm_fit)
M2(grm_fit)
#
############
### GPCM ###
############
gpcm_params <- coef(gpcm_fit, IRTpars = TRUE, simplify = TRUE)
gpcm_items <- gpcm_params$items
gpcm_items
write.xlsx(round(gpcm_items,3),
           "D:\\Adi\\Seleksi Assessment Analytics - Ruang Guru\\pargpcm.xlsx")
plot(gpcm_fit, type = "trace",main = "",
     par.settings = simpleTheme(lty = 1:3, lwd = 2),
     auto.key=list(points = FALSE, lines = TRUE, columns = 3))
itemplot(gpcm_fit, type = "infoSE", 5, main = "")
itemplot(gpcm_fit, type = "infoSE", 6, main = "")
teta <- fscores(gpcm_fit, method="EAP", full.scores=T, full.scores.SE = T)
hist(teta[,1], main = "", xlab = "Kemampuan Peserta", ylab = "Frekuensi", col = "grey")
itemplot(gpcm_fit, type = "info",20)
plot(gpcm_fit, type = "infoSE",
     par.settings = simpleTheme(lty = 1:3, lwd = 2),
     auto.key=list(points = FALSE, lines = TRUE, columns = 3))
plot(gpcm_fit, type = "infoSE",which.items = 19, theta_lim = c(-6, 6))
