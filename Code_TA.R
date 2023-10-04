#Packages
library(readxl)


#Input Data
DATA_TA <- read_excel("D:/OneDrive - mhs.unsyiah.ac.id/Semester 7/Draft TA/Data/Code/Data_gabung _filter.xlsx")


View(DATA_TA)

#Melihat ringkasan dan tipe data
summary(DATA_TA)
str(DATA_TA)


# Convert character variables to categorical variables
DATA_TA$`Jumlah Anggota Rumah Tangga (ART)` <- as.factor(DATA_TA$`Jumlah Anggota Rumah Tangga (ART)`)
DATA_TA$`Jumlah Keluarga` <- as.factor(DATA_TA$`Jumlah Keluarga`)
DATA_TA$`Status bangunan tempat tinggal` <- as.factor(DATA_TA$`Status bangunan tempat tinggal`)
DATA_TA$`Status lahan tempat tinggal` <- as.factor(DATA_TA$`Status lahan tempat tinggal`)
#DATA_TA$`Luas lantai` <- as.factor(DATA_TA$`Luas lantai`)
DATA_TA$`Jenis lantai terluas` <- as.factor(DATA_TA$`Jenis lantai terluas`)
DATA_TA$`Jenis dinding terluas` <- as.factor(DATA_TA$`Jenis dinding terluas`)
DATA_TA$`Kondisi dinding` <- as.factor(DATA_TA$`Kondisi dinding`)
DATA_TA$`Jenis atap terluas` <- as.factor(DATA_TA$`Jenis atap terluas`)
DATA_TA$`Kondisi atap` <- as.factor(DATA_TA$`Kondisi atap`)
DATA_TA$`Jumlah kamar tidur` <- as.factor(DATA_TA$`Jumlah kamar tidur`)
DATA_TA$`Sumber air minum` <- as.factor(DATA_TA$`Sumber air minum`)
DATA_TA$`Cara memperoleh air minum` <- as.factor(DATA_TA$`Sumber penerangan utama`)
DATA_TA$`Sumber penerangan utama` <- as.factor(DATA_TA$`Sumber penerangan utama`)
DATA_TA$`Daya listrik terpasang` <- as.factor(DATA_TA$`Daya listrik terpasang`)
DATA_TA$`Bahan bakar untuk memasak` <- as.factor(DATA_TA$`Bahan bakar untuk memasak`)
DATA_TA$`Penggunaan fasilitas BAB` <- as.factor(DATA_TA$`Penggunaan fasilitas BAB`)
DATA_TA$`Jenis kloset` <- as.factor(DATA_TA$`Jenis kloset`)
DATA_TA$`Tempat pembuangan akhir tinja` <- as.factor(DATA_TA$`Tempat pembuangan akhir tinja`)
DATA_TA$`Tabung gas 5,5kg atau lebih` <- as.factor(DATA_TA$`Tabung gas 5,5kg atau lebih`)
DATA_TA$`Lemari es/kulkas` <- as.factor(DATA_TA$`Lemari es/kulkas`)
DATA_TA$AC <- as.factor(DATA_TA$AC)
DATA_TA$`Pemanas air` <- as.factor(DATA_TA$`Pemanas air`)
DATA_TA$Televisi <- as.factor(DATA_TA$Televisi)
DATA_TA$`Emas/perhiasan/tabungan` <- as.factor(DATA_TA$`Emas/perhiasan/tabungan`)
DATA_TA$Komputer <- as.factor(DATA_TA$Komputer)
DATA_TA$Sepeda <- as.factor(DATA_TA$Sepeda)
DATA_TA$`Sepeda motor` <- as.factor(DATA_TA$`Sepeda motor`)
DATA_TA$Mobil <- as.factor(DATA_TA$Mobil)
DATA_TA$Perahu <- as.factor(DATA_TA$Perahu)
DATA_TA$`Motor tempel` <- as.factor(DATA_TA$`Motor tempel`)
DATA_TA$`Perahu motor` <- as.factor(DATA_TA$`Perahu motor`)
DATA_TA$Kapal <- as.factor(DATA_TA$Kapal)
DATA_TA$`Aset lahan` <- as.factor(DATA_TA$`Aset lahan`)
#DATA_TA$`Luas lahan` <- as.factor(DATA_TA$`Luas lahan`) 
DATA_TA$`Rumah di tempat lain` <- as.factor(DATA_TA$`Rumah di tempat lain`)
DATA_TA$`ART memiliki usaha sendiri/bersama` <- as.factor(DATA_TA$`ART memiliki usaha sendiri/bersama`)
DATA_TA$`Status Kesejahteraan` <- as.factor(DATA_TA$`Status Kesejahteraan`)

summary(DATA_TA)
str(DATA_TA)


# melihat ada atau tidak data duplikat
any(duplicated(DATA_TA))
sum(duplicated(DATA_TA)) #Jumlah Data duplikat

#Data tanpa duplikat
DATA_TA_1 <- unique(DATA_TA)
any(duplicated(DATA_TA_1)) #Cek data duplikat

dim(DATA_TA_1)
dim(DATA_TA)

#eksplor ke excel
library(writexl)
write_xlsx(DATA_TA_1, path="DATA_CLEAN.xlsx")

#===============PERHITUNGAN NILAI ENTROPY===============#
#install.packages("dplyr")
library(dplyr)
entropy <- function(target) {
  freq <- table(target)/length(target)
  vec <- as.data.frame(freq)[,2]
  vec<-vec[vec>0]
  -sum(vec * log2(vec))
}
print(entropy(DATA_TA_1$`Jumlah Anggota Rumah Tangga (ART)`))
print(entropy(DATA_TA_1$`Jumlah Keluarga`))
print(entropy(DATA_TA_1$`Status bangunan tempat tinggal`))
print(entropy(DATA_TA_1$`Status lahan tempat tinggal`))
print(entropy(DATA_TA_1$`Jenis lantai terluas`))
print(entropy(DATA_TA_1$`Jenis dinding terluas`))
print(entropy(DATA_TA_1$`Kondisi dinding`))
print(entropy(DATA_TA_1$`Jenis atap terluas`))
print(entropy(DATA_TA_1$`Kondisi atap`))
print(entropy(DATA_TA_1$`Jumlah kamar tidur`))
print(entropy(DATA_TA_1$`Sumber air minum`))
print(entropy(DATA_TA_1$`Cara memperoleh air minum`))
print(entropy(DATA_TA_1$`Sumber penerangan utama`))
print(entropy(DATA_TA_1$`Daya listrik terpasang`))
print(entropy(DATA_TA_1$`Bahan bakar untuk memasak`))
print(entropy(DATA_TA_1$`Penggunaan fasilitas BAB`))
print(entropy(DATA_TA_1$`Jenis kloset`))
print(entropy(DATA_TA_1$`Tempat pembuangan akhir tinja`))
print(entropy(DATA_TA_1$`Tabung gas 5,5kg atau lebih`))
print(entropy(DATA_TA_1$`Lemari es/kulkas`))
print(entropy(DATA_TA_1$AC))
print(entropy(DATA_TA_1$`Pemanas air`))
print(entropy(DATA_TA_1$Televisi))
print(entropy(DATA_TA_1$`Emas/perhiasan/tabungan`))
print(entropy(DATA_TA_1$Komputer))
print(entropy(DATA_TA_1$Sepeda))
print(entropy(DATA_TA_1$`Sepeda motor`))
print(entropy(DATA_TA_1$Mobil))
print(entropy(DATA_TA_1$Perahu))
print(entropy(DATA_TA_1$`Motor tempel`))
print(entropy(DATA_TA_1$`Perahu motor`))
print(entropy(DATA_TA_1$Kapal))
print(entropy(DATA_TA_1$`Aset lahan`))
print(entropy(DATA_TA_1$`Rumah di tempat lain`))
print(entropy(DATA_TA_1$`ART memiliki usaha sendiri/bersama`))
print(entropy(DATA_TA_1$`Status Kesejahteraan`))

#=============== INFORMATION GAIN===============#

#installed a Java version that matches the type of R version you are using
#https://www.java.com/en/download/manual.jsp
#running this command in Windows cmd: setx PATH 
#C:\Program Files\Java\jre1.8.0_211\bin\server;%PATH%
#run this command before loading the FSelector package: 
#Sys.setenv (JAVA_HOME="")
#Reinstall the rJava package and then the FSelector package.
#install.packages("rJava")
#install.packages("FSelector")
#library(FSelector)

install.packages("FSelector")
library(FSelector)
InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}


