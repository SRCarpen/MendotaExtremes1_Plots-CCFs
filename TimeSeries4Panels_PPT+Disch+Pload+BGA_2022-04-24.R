# Plot time series

rm(list = ls())
graphics.off()

# load data
#save(dat3,file='PPT_Disch_Pload_BGAdark_2008-2021.Rdata')
load(file='PPT_Disch_Pload_BGAdark_2008-2021.Rdata')

print(colnames(dat3),quote=F)
print(dim(dat3),quote=F)

# Plot 2008-2021 time series
windows(height=10,width=7)
par(mfrow=c(4,1),mar=c(3, 4.5, 1, 2) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(dat3$dindex,dat3$PRCP,type='l',lwd=2,col='blue',xlab=' ',
     ylab='Precipitation, mm')
abline(v=c(2008:2022),lty=3,lwd=2,col='darkgrey')
text(x=2021.4,y=88,'A',cex=1.8)
plot(dat3$dindex,dat3$Totdis,type='l',lwd=2,log='y',col='darkgoldenrod',xlab=' ',
     ylab='Discharge, m3/d')
abline(v=c(2008:2022),lty=3,lwd=2,col='darkgrey')
text(x=2021.4,y=4.2e6,'B',cex=1.8)
plot(dat3$dindex,dat3$TotPload,type='l',lwd=2,log='y',col='red',xlab=' ',
     ylab='P Load, kg/d')
abline(v=c(2008:2022),lty=3,lwd=2,col='darkgrey')
par(mar=c(4, 4.5, 1, 2))
text(x=2021.4,y=4200,'C',cex=1.8)
plot(dat3$dindex,dat3$zlBGA,type='l',lwd=2,col='forestgreen',xlab='Year',
     ylab='Phycocyanin')
abline(v=c(2008:2022),lty=3,lwd=2,col='darkgrey')
text(x=2021.4,y=6,'D',cex=1.8)

# Read and plot precip, Disch and P load datasets from all years

#dat0 = read.csv("Madison_DCRA_precip_1940-2015.csv", stringsAsFactors=FALSE)
load('DCRA_Precip_1940-2021.Rdata')
dat0=datall  # rename for compatibility with existing code
print('Madison DCRA precip dataset',quote=F)
print(dat0[1,]) # show first line with column names

windows(width=10,height=4)
par(mfrow=c(1,1),mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(dat0$dindex,dat0$PRCP,type='l',lwd=2,col='blue',xlab='Year',
     ylab='precipitation, mm')

# Load discharge
# save(PB1,YW1,PBYW2,file='Discharge_PB+YW_1990-2021.Rdata')
load(file='Discharge_PB+YW_1990-2021.Rdata')
print(PBYW2[1,])

windows(width=10,height=4)
par(mfrow=c(1,1),mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(PBYW2$dindex.y,PBYW2$YWm3day,type='l',lwd=2,log='y',col='darkgoldenrod',xlab='Year',
     ylab='Discharge, m3/d')

# read data
#dat1 = read.csv("Yahara_Windsor_Flow+P.csv", stringsAsFactors=FALSE)
load('AnnLoads_PB+YP_1995-2021.Rdata')
dat1 = Pload  # rename for compatibility with existing code
dat1$TotPload = dat1$PB.kg.d + dat1$YW.kg.d   # PB + YW combined loads
rm(Pload)
print('Yahara-Windsor P load dataset',quote=F)
print(dat1[1,]) # show first line with column names
dat1$dindex = dat1$year + (dat1$doy/366)

windows(width=10,height=4)
par(mfrow=c(1,1),mar=c(4.5, 4.5, 2, 2) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(dat1$dindex,dat1$TotPload,type='l',lwd=2,log='y',col='red',xlab='Year',
     ylab='P Load, kg/d')

