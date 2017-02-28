# analysis factor ----
library(MVN)
library(psych)
library(nFactors)
library(corrplot)
library(nortest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)      # library for boxcox
library(plotly)
library(forecast)

# cek kelurahan----
kelurahan <- as.data.frame(unique(qlue_ts_edit_kat$detail_name_kel))
qlue_ts_edit_kat$detail_name_kel[qlue_ts_edit_kat$detail_name_kel=="Setia budi"] ="Setia Budi"
qlue_ts_edit_kat$detail_name_kel <- as.character(unlist(qlue_ts_edit_kat$detail_name_kel))
qlue_ts_edit_kat <- subset(qlue_ts_edit_kat,qlue_ts_edit_kat$detail_name_kel!="")

#Cek kategori----
kategori <- as.data.frame(unique(qlue_ts_edit_kat$kategori))
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Teroris"] ="Potensi Teroris"
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Makanan Tak Sehat"] ="Makanan Non Hygienis"
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Pajak Kost"] ="Pajak Kos2an"
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Sinyal Buruk"] ="Listrik / Air / Telko"
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Mati Lampu"] ="Listrik / Air / Telko"
qlue_ts_edit_kat$kategori[qlue_ts_edit_kat$kategori=="Air Kotor"] ="Listrik / Air / Telko"
qlue_ts_edit_kat <- subset(qlue_ts_edit_kat,qlue_ts_edit_kat$kategori!="")

# prerparing data for time series auto arima----
qlue_ts_edit_kat$timestamp <- as.POSIXlt(qlue_ts_edit_kat$timestamp)
qlue_ts_edit_kat$time <- strftime(qlue_ts_edit_kat$timestamp,format = "%Y-%m-%d")
qlue_ts_edit_kat$time <- as.POSIXct(qlue_ts_edit_kat$time)
qlue_ts_edit_kat$timestamp <- as.POSIXct(qlue_ts_edit_kat$timestamp)
data_ts <- qlue_ts_edit_kat %>% group_by(kategori,time) %>% summarise(freq = sum(number_of_record))
data_ts <- data_ts %>% spread(kategori,freq)
data_ts[is.na(data_ts)]=0
data_ts <- data_ts %>% gather(kategori,freq,-time)

# split data into insample and outsample----

data_ts_inSample <- data_ts %>% filter(time<"2017-01-01")
data_ts_inSample$freq <- data_ts_inSample$freq+1
data_ts_outSample <- data_ts %>% filter(time>="2017-01-01")
data_ts_inSample <- split.data.frame(data_ts_inSample,data_ts_inSample$kategori) # change from dataframe into list


#plot time series----

plotTS <- function(number){
  ggplot(data_ts_inSample[number][[1]],aes(time,freq))+geom_line()
}

plot_ly(data_ts_inSample[9][[1]],x=~time,y=~freq,type = "scatter",mode="lines")

plotTS(9)

#boxcox and get value of lambda each variable----

lambda<-powerTransform(data_ts_inSample[1][[1]]$freq~1,data_ts_inSample[1][[1]])
fasum <- sum(data_ts_inSample[8][[1]]$freq)
fasum$freq <- fasum$freq+1
var2 <- data_ts_inSample[2][[1]]
lamdaFasum = powerTransform(var2$freq~1,var2)

unname(lamda[[1]])

lamda <- list()
for(i in 1:length(data_ts_inSample)){
  if(sum(data_ts_inSample[i][[1]]$freq)==366){
    lamda[i] <- list(1)
  } else {
      lamda[i]<- list(powerTransform(data_ts_inSample[i][[1]]$freq~1,data_ts_inSample[i][[1]]))
      lamda[i]<- list(lamda[[i]]$lambda)
    }
}

for(i in 1:length(lamda)){
  lamda[[i]]<- unname(lamda[[i]])
}



for(i in 1:length(data_ts_inSample)){
  data_ts_inSample[[i]]<-list(data_ts_inSample[[i]],lamda=lamda[[i]])
}

# adf test for stationarty in mean

#ARIMA----

hasilARIMA=list()
for(i in 1:length(data_ts_inSample)){
  hasilARIMA[i]=list(auto.arima(data_ts_inSample[i][[1]][[1]]$freq,stationary = TRUE,seasonal=TRUE,
                                lambda=data_ts_inSample[[i]]$lamda))
}


prediksi_arima = list()
for(i in 1:length(hasilARIMA)){
  prediksi_arima[i]=list(predict(hasilARIMA[i][[1]],n.ahead=53))
}



data_predik <- data.frame(bencana_banjir=prediksi_arima[5][[1]]$pred,
                          fasum=prediksi_arima[9][[1]]$pred,
                          ikli = prediksi_arima[12][[1]]$pred,
                          jalan_rusak = prediksi_arima[14][[1]]$pred,
                          kaki_lima_liar = prediksi_arima[16][[1]]$pred,
                          kemacetan = prediksi_arima[20][[1]]$pred,
                          PJURusak = prediksi_arima[23][[1]]$pred,
                          parli = prediksi_arima[33][[1]]$pred,
                          pelanggaran = prediksi_arima[34][[1]]$pred,
                          sampah = prediksi_arima[48][[1]]$pred)

sum(data_predik[32:53,10])
data_predik <- round(data_predik)
bencana_banjir<- data.frame(bencana_banjir)

# aggregate kategori vs kelurahan
kat_kel <- qlue_ts_edit_kat %>% group_by(kategori,detail_name_kel) %>% summarise(freq = sum(number_of_record))
kat_kel <- kat_kel %>% spread(kategori,freq)
kat_kel[is.na(kat_kel)]=0
kat_kel[is.na(kat_kel)] <- 0

?halt
