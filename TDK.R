################
# Preparations #
################
install.packages("WeightedPortTest")
install.packages("moments")
install.packages("gridExtra")
install.packages("rmgarch")
install.packages("rugarch")
install.packages("readxl")
install.packages("PerformanceAnalytics")
install.packages("FinTS")
install.packages("pastecs")
install.packages("timeSeries")
install.packages("Hmisc")
installed.packages("fpp")
install.packages("mgarchBEKK")
install.packages("quantmod")
install.packages("MTS")
library(WeightedPortTest)
library(tidyverse)
library(moments)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(viridis)
library(PerformanceAnalytics)
library(rugarch)
library(FinTS)
library(rmgarch)
library(timeSeries) 
library(pastecs)
library(tseries)
library(Hmisc)
library(gogarch)
library(parallel)
library(fpp)
library(ggplot2)
library(tseries)
library(rugarch)
library(rmgarch)
library(readxl)
library(mgarchBEKK)
library(quantmod)
library(MTS)
library(gridExtra)

###############
# Import data #
###############

setwd("/Users/okovacsgabor/Desktop/research/volatility_spillover")

data = read_excel("d.xlsx")

data_new <- data[!duplicated(data), ]

data_zoo <- read.zoo(data_new, order.by = as.Date(rownames(data_new), "%m/%d/%Y"))

#####################################
# Calculating returns and plot data #
#####################################

wig = data_zoo[, "WIG", drop = FALSE]
r_wig = CalculateReturns(wig, method="log")

bet = data_zoo[, "BET", drop = FALSE]
r_bet = CalculateReturns(bet, method="log")

px = data_zoo[, "PX", drop = FALSE]
r_px = CalculateReturns(px, method="log")

crobex = data_zoo[, "CROBEX", drop = FALSE]
r_crobex = CalculateReturns(crobex, method="log")

sax = data_zoo[, "SAX", drop = FALSE]
r_sax = CalculateReturns(sax, method="log")

sbitop = data_zoo[, "SBITOP", drop = FALSE]
r_sbitop = CalculateReturns(sbitop, method="log")

pfts = data_zoo[, "PFTS", drop = FALSE]
r_pfts = CalculateReturns(pfts, method="log")

bux = data_zoo[, "BUX", drop = FALSE]
r_bux = CalculateReturns(bux, method="log")

sp = data_zoo[, "S&P500", drop = FALSE]
r_sp = CalculateReturns(sp, method="log")

dax = data_zoo[, "DAX", drop = FALSE]
r_dax = CalculateReturns(dax, method="log")

moex = data_zoo[, "MOEX", drop = FALSE]
r_moex = CalculateReturns(moex, method="log")

sse = data_zoo[, "SSE", drop = FALSE]
r_sse = CalculateReturns(sse, method="log")

atx = data_zoo[, "ATX", drop = FALSE]
r_atx = CalculateReturns(atx, method="log")

#Plot data
##########

pbux = fortify.zoo(r_bux)
plotbux = pbux %>%
  ggplot( aes(x=Index, y=r_bux)) +
  geom_line(color="#69b3a2") +
  ggtitle("BUX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

pwig = fortify.zoo(r_wig)
plotwig = pwig %>%
  ggplot( aes(x=Index, y=r_wig)) +
  geom_line(color="#69b3a2") +
  ggtitle("WIG") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

pbet = fortify.zoo(r_bet)
plotbet = pbet %>%
  ggplot( aes(x=Index, y=r_bet)) +
  geom_line(color="#69b3a2") +
  ggtitle("BET") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

ppx = fortify.zoo(r_px)
plotpx = ppx %>%
  ggplot( aes(x=Index, y=r_px)) +
  geom_line(color="#69b3a2") +
  ggtitle("PX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

pcrobex = fortify.zoo(r_crobex)
plotcrobex = pcrobex %>%
  ggplot( aes(x=Index, y=r_crobex)) +
  geom_line(color="#69b3a2") +
  ggtitle("CROBEX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

psax = fortify.zoo(r_sax)
plotsax = psax %>%
  ggplot( aes(x=Index, y=r_sax)) +
  geom_line(color="#69b3a2") +
  ggtitle("SAX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

psbitop = fortify.zoo(r_sbitop)
plotsbitop = psbitop %>%
  ggplot( aes(x=Index, y=r_sbitop)) +
  geom_line(color="#69b3a2") +
  ggtitle("SBITOP") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

ppfts = fortify.zoo(r_pfts)
plotpfts = ppfts %>%
  ggplot( aes(x=Index, y=r_pfts)) +
  geom_line(color="#69b3a2") +
  ggtitle("PFTS") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

patx = fortify.zoo(r_atx)
plotatx = patx %>%
  ggplot( aes(x=Index, y=r_atx)) +
  geom_line(color="#69b3a2") +
  ggtitle("ATX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

grid.arrange(plotbux, plotwig, plotbet, plotpx, plotcrobex, plotsax, plotsbitop, plotpfts, plotatx,
             ncol = 3, nrow = 3)

psp = fortify.zoo(r_sp)
colnames(psp) = c("Index", "r_sp")
plotsp = psp %>%
  ggplot( aes(x=Index, y=r_sp)) +
  geom_line(color="#69b3a2") +
  ggtitle("S&P500") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

pdax = fortify.zoo(r_dax)
plotdax = pdax %>%
  ggplot( aes(x=Index, y=r_dax)) +
  geom_line(color="light blue") +
  ggtitle("DAX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

pmoex = fortify.zoo(r_moex)
plotmoex = pmoex %>%
  ggplot( aes(x=Index, y=r_moex)) +
  geom_line(color="#69b3a2") +
  ggtitle("MOEX") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

psse = fortify.zoo(r_sse)
plotsse = psse %>%
  ggplot( aes(x=Index, y=r_sse)) +
  geom_line(color="#69b3a2") +
  ggtitle("SSE") +
  ylab("") +
  xlab("Dátum") +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme_ipsum()

grid.arrange(plotsp, plotdax, plotmoex, plotsse,
             ncol = 2, nrow = 2)
###################################
# Table 1. Descriptive statistics#
##################################

k=ncol(des)

options(digits=8)

SummaryStatistics = function(data){
  moments = matrix(NA, ncol=ncol(data), nrow=14)
  colnames(moments)=colnames(data)
  rownames(moments)=c("Mean","Variance","Skewness","","Kurtosis","","JB","","ERS","","Q(20)","","Q2(20)","")
  for (i in 1:ncol(data)){
    moments[1,i] = mean(data[,i])
    moments[2,i] = var(data[,i])
    skew = moments::agostino.test(data[,i])
    moments[3,i] = skew$statistic[1]
    moments[4,i] = skew$p.value
    kurt = moments::anscombe.test(data[,i])
    moments[5,i] = kurt$statistic[1]-3
    moments[6,i] = kurt$p.value
    jb = moments::jarque.test(as.numeric(data[,i]))
    moments[7,i] = jb$statistic
    moments[8,i] = jb$p.value
    ers = urca::ur.ers(data[,i],type="DF-GLS",model="constant")
    moments[9,i] = ers@teststat
    moments[10,i]= ers@testreg$coefficients[1,4]
    bt = WeightedPortTest::Weighted.Box.test(data[,i], type="Ljung-Box", lag=20)
    moments[11,i] = bt$statistic
    moments[12,i] = bt$p.value
    bt2 = WeightedPortTest::Weighted.Box.test(data[,i], type="Ljung-Box", lag=20, sqrd.res=T)
    moments[13,i] = bt2$statistic
    moments[14,i] = bt2$p.value
  }
  
  cc=c(4,6,8,10,12,14)
  moments = round(moments,3)
  moments1 = moments
  for (j in 1:k){
    for (i in 1:length(cc)){
      i = cc[i]
      if (moments[i,j]<=0.01) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"***",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else if (moments[i,j]<=0.05) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"**",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else if (moments[i,j]<=0.10) {
        moments1[(i-1),j] = paste(format(round(moments[(i-1),j],3),nsmall=3),"*",sep="")
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      } else {
        moments1[(i-1),j] = format(round(moments[(i-1),j],3),nsmall=3)
        moments1[i,j] = paste("(",format(round(moments[i,j],3),nsmall=3),")",sep="")
      }
    }
  }
  
  for (j in 1:k){
    i = 9
    if (moments[i,j]<=-2.57) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"***",sep="")
    } else if (moments[i,j]<=-1.96) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"**",sep="")
    } else if (moments[i,j]<=-1.62) {
      moments1[i,j] = paste(format(round(moments[i,j],3),nsmall=3),"*",sep="")
    } else {
      moments1[i,j] = format(round(moments[i,j],3),nsmall=3)
    }
  }
  moments1
}



des = merge(r_wig, r_bet, r_px, r_crobex, r_sax, r_sbitop, r_pfts, r_bux, r_atx, r_sp, r_dax, r_moex, r_sse)
des = des[-1,]
path_to_tables = '/Users/okovacsgabor/Desktop/research/volatility_spillover'

table_1 <- data.frame(SummaryStatistics(des))
write.csv(table_1, file.path(path_to_tables, 'Table_1.csv'), row.names = TRUE)


#####################
# Stationarity test #
#####################

merged=merge(r_wig, r_bet, r_px, r_crobex, r_sax, r_sbitop, r_pfts, r_bux, r_sp, r_dax, r_moex, r_sse, r_atx, all=FALSE)
merged= fortify.zoo(merged)
merged=subset(merged, select = -c(Index) )
merged=merged[-c(1), ]

ers_result = matrix(NA, ncol=ncol(merged), nrow=1)
colnames(ers_result)=colnames(merged)

for (i in 1 :ncol(merged)){
ers = urca::ur.ers(merged[,i],type="P-test",model="constant")
ers_result[1,i]=ers@teststat
}

##############
# BEKK-GARCH #
##############

rec = list(r_wig, r_bet, r_px, r_crobex, r_sax, r_sbitop, r_pfts, r_bux,r_atx)

varname = c("r_wig", "r_bet", "r_px", "r_crobex", "r_sax", "r_sbitop", "r_pfts", "r_bux","r_atx")

z = "bekk_r_sp"

for (i in 1:length(rec)){
  cat(" r_sp/", i)
  y = fortify.zoo(merge(r_sp, rec[[i]], all=FALSE))
  y = subset(y, select = -c(Index) )
  y = y[-c(1), ]
  y = as.matrix(y)
  x = BEKK(y, order = c(1,1))
  assign(paste(z, varname[i], sep = ""),x)
}

z = "daxbekk"

for (i in 1:length(rec)){
  cat(" r_dax/", i)
  y = fortify.zoo(merge(r_dax, rec[[i]], all=FALSE))
  y = subset(y, select = -c(Index) )
  y = y[-c(1), ]
  y = as.matrix(y)
  x = BEKK(y, order = c(1,1))
  assign(paste(z, varname[i], sep = ""),x)
}

z = "moexbekk"

for (i in 1:length(rec)){
  cat(" r_moex/", i)
  y = fortify.zoo(merge(r_moex, rec[[i]], all=FALSE))
  y = subset(y, select = -c(Index) )
  y = y[-c(1), ]
  y = as.matrix(y)
  x = BEKK(y, order = c(1,1))
  assign(paste(z, varname[i], sep = ""),x)
}


z = "ssebekk"

for (i in 1:length(rec)){
  cat(" r_sse/", i)
  y = fortify.zoo(merge(r_sse, rec[[i]], all=FALSE))
  y = subset(y, select = -c(Index) )
  y = y[-c(1), ]
  y = as.matrix(y)
  x = BEKK(y, order = c(1,1))
  assign(paste(z, varname[i], sep = ""),x)
}

#############
# DCC-GARCH #
#############

model1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), 
                    variance.model = list(garchOrder = c(1,1), 
                                          model = "sGARCH"), 
                    distribution.model = "std")

dcc_model = dccspec(uspec = multispec(replicate(2, model1)), 
                    dccOrder = c(1,1), 
                    distribution = "mvt")

z = "dccsp"

for (i in 1:length(rec)){
  y = merge(r_sp, rec[[i]], all=FALSE)
  y = y[-1,]
  cl = makePSOCKcluster(10)
  x = dccfit(dcc_model, data = y, cluster = cl)
  stopCluster(cl)
  assign(paste(z, varname[i], sep = ""),x)
}  

z = "dccdax"

for (i in 1:length(rec)){
  y = merge(r_dax, rec[[i]], all=FALSE)
  y = y[-1,]
  cl = makePSOCKcluster(10)
  x = dccfit(dcc_model, data = y, cluster = cl)
  stopCluster(cl)
  assign(paste(z, varname[i], sep = ""),x)
}  
  
z = "dccmoex"

for (i in 1:length(rec)){
  y = merge(r_moex, rec[[i]], all=FALSE)
  y = y[-1,]
  cl = makePSOCKcluster(10)
  x = dccfit(dcc_model, data = y, cluster = cl)
  stopCluster(cl)
  assign(paste(z, varname[i], sep = ""),x)
}  

z = "dccsse"

for (i in 1:length(rec)){
  y = merge(r_sse, rec[[i]], all=FALSE)
  y = y[-1,]
  cl = makePSOCKcluster(10)
  x = dccfit(dcc_model, data = y, cluster = cl)
  stopCluster(cl)
  assign(paste(z, varname[i], sep = ""),x)
}  

##########
#Map-plot#
##########

mapper = read_excel("mapper.xlsx")
mapdata = map_data("world")
mapdata = left_join(mapdata, mapper, by="region")
mapdata = subset(mapdata, select = -c(subregion) )
filter(mapdata, region=="Poland" |region=="Romania" |region=="Czech Republic" |region=="Croatia"|region=="Slovakia"  |region=="Slovenia" |region=="Ukraine" |region=="Hungary" |region=="Austria")
mapdata1 <- mapdata[mapdata$protectedness %in% c(0,1,2,3,4,5),]


view(mapdata1)

map = ggplot(mapdata1, aes( x=long, y=lat, group=group))+
  geom_polygon(aes(fill = protectedness), color = "black")
map

map2 <- map + scale_fill_gradient(name = "", low = "red", high =  "green", na.value = "grey50")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        rect = element_blank(),
        legend.position="none")
map2
