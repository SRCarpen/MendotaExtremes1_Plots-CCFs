# Merge PB+YW discharge for years of overlap, roughly 1990 - 2021

rm(list = ls())
graphics.off()

# load data
load(file='Disch_PB+YP_1976-2021_gaps.Rdata')
# 
print('PB discharge',quote=F)
print(PB.dis[1,])
print(dim(PB.dis))
print(unique(PB.dis$year))
print('',quote=F)
print('YW discharge',quote=F)
print(YW.dis[1,])
print(dim(YW.dis))
print(unique(YW.dis$year))

windows()
plot(PB.dis$Date,PB.dis$m3day,type='l')

windows()
plot(YW.dis$Date,YW.dis$m3day,type='l')

# clean data
PB0 = subset(PB.dis,select=c('Date','year','doy','m3day'))
PB0$PBm3day = PB0$m3day
PB0$dindex = PB0$year + (PB0$doy/366)
PB1 = subset(PB0,subset=(year>=1990 & year<=2021))
#
YW0 = subset(YW.dis,select=c('Date','year','doy','m3day'))
YW0$YWm3day = YW0$m3day
YW0$dindex = YW0$year + (YW0$doy/366)
YW1 = subset(YW0,subset=(year>=1990 & year<=2021))

# attempt merge
PBYW0 = merge(PB1,YW1,by=c('year','doy'))
print(c('dim PBYW0 ',dim(PBYW0)),quote=F)
PBYW1 = na.omit(PBYW0)
print(c('dim na.omit(PBYW1) ',dim(PBYW1)),quote=F)

# keep the years with data for both sites
PBYW2 = PBYW1[order(PBYW1[,1],PBYW1[,2]),]
PBYW2$dindex = PBYW2$year + (PBYW2$doy/366)
# try adding the two sites
PBYW2$Totdis = PBYW2$PBm3day + PBYW2$YWm3day
print(c('dim for years 1989 onward',dim(PBYW2)),quote=F)

windows()
plot(PBYW2$dindex,PBYW2$Totdis,type='l',xlab='Date',ylab='PB + YW disch')

windows()
ylim = range(PBYW2$PBm3day,PBYW2$YWm3day,na.rm=T)
xlim = range(PBYW2$Date.x,PBYW2$Date.y,na.rm=T)
plot(PBYW2$Date.x,PBYW2$PBm3day,type='l',log='y',lwd=1,col='blue',xlab='Date',
     ylab='Discharge m3/day')
points(PBYW2$Date.y,PBYW2$YWm3day,type='l',lwd=1,col='forestgreen')

windows()
plot(PBYW2$Date.x,PBYW2$Date.y,type='l')

print('check differences of PB dates & YW dates',quote=F)
dD = PBYW2$dindex.x - PBYW2$dindex.y
print(summary(dD))
print(mean(dD))


# Save merged data
save(PB1,YW1,PBYW2,file='Discharge_PB+YW_1990-2021.Rdata')
