# Make a file of daily data for all years, where 
#  day = 00:00 - 04:00 & 22:00 - 24:00 each day
# SRC 2020-04-27

rm(list = ls())
graphics.off()

# functions =============================================================================

# Function to extract daily mean BGA between time window for a year
trimBGA = function(dat0) {
  dat1 = subset(dat0,subset=(dat0$DOY >= 152 & dat0$DOY <= 258))  # keep 1 June - 15 Sept
  dat1$dec = dat1$DOY - floor(dat1$DOY)
  #dat2 = subset(dat1,subset=(dec >= 0.375 & dec <= 0.625))  # keep 0900 - 1500
  dat2 = subset(dat1,subset=(dec >= 0.917 | dec <= 0.167))  # keep 2200 - 0400
  # center logs
  ymean = mean(dat2$lBGA,na.rm=T)
  ysd = sd(dat2$lBGA,na.rm=T)
  dat2$clBGA = dat2$lBGA - ymean
  dat2$zlBGA = dat2$clBGA/ysd
  # means by day
  udoy = unique(trunc(dat2$DOY))
  ndoy = length(udoy)
  dat3 = matrix(0,nrow=ndoy,ncol=6) 
  for(i in 1:ndoy) {
    dayBGA = subset(dat2,subset=(trunc(dat2$DOY) == udoy[i]))
    meanBGA = mean(dayBGA$BGA,na.rm=T)
    meanlBGA = mean(dayBGA$lBGA,na.rm=T)
    meanclBGA = mean(dayBGA$clBGA,na.rm=T)
    meanzlBGA = mean(dayBGA$zlBGA,na.rm=T)
    dat3[i,]=c(dayBGA$Year[1],udoy[i],meanBGA,meanlBGA,meanclBGA,meanzlBGA)
  }
  dat4 = as.data.frame(dat3)
  return(dat4) 
}

# Function to extract daily mean Chl between time window for a year
trimChl = function(dat0) {
  dat1 = subset(dat0,subset=(dat0$DOY >= 152 & dat0$DOY <= 258))  # keep 1 June - 15 Sept
  dat1$dec = dat1$DOY - floor(dat1$DOY)
  #dat2 = subset(dat1,subset=(dec >= 0.375 & dec <= 0.625))  # keep 0900 - 1500
  dat2 = subset(dat1,subset=(dec >= 0.917 | dec <= 0.167))  # keep 2200 - 0400
  # center logs
  ymean = mean(dat2$lChl,na.rm=T)
  ysd = sd(dat2$lChl,na.rm=T)
  dat2$clChl = dat2$lChl - ymean
  dat2$zlChl = dat2$clChl/ysd
  # means by day
  udoy = unique(trunc(dat2$DOY))
  ndoy = length(udoy)
  dat3 = matrix(0,nrow=ndoy,ncol=6) 
  for(i in 1:ndoy) {
    dayChl = subset(dat2,subset=(trunc(dat2$DOY) == udoy[i]))
    meanChl = mean(dayChl$Chl,na.rm=T)
    meanlChl = mean(dayChl$lChl,na.rm=T)
    meanclChl = mean(dayChl$clChl,na.rm=T)
    meanzlChl = mean(dayChl$zlChl,na.rm=T)
    dat3[i,]=c(dayChl$Year[1],udoy[i],meanChl,meanlChl,meanclChl,meanzlChl)
  }
  dat4 = as.data.frame(dat3)
  return(dat4) 
}

# end of functions

# make list of file names
datnames = c('Me_1min_2008.Rdata','Me_1min_2009.Rdata','Me_1min_2010.Rdata',
             'Me_1min_2011.Rdata','Me_1min_2012.Rdata','Me_1min_2013.Rdata',
             'Me_1min_2014.Rdata','Me_1min_2015.Rdata','Me_1min_2016.Rdata',
             'Me_1min_2017.Rdata','Me_1min_2018.Rdata','Me_1min_2019.Rdata',
             'Me_1min_2020.Rdata','Me_1min_2021.Rdata')
NY = length(datnames)
#
# Make composite file
bgachl = load(file=datnames[1])
BGAdark = trimBGA(MeBGA) # create first year as seed for calculation 
Chldark = trimChl(MeChl)
rm(bgachl,MeBGA,MeChl)

for(j in 2:NY) {
  bgachl = load(file=datnames[j])
  BGAnext = trimBGA(MeBGA)
  Chlnext = trimChl(MeChl)
  BGAdark = rbind(BGAdark,BGAnext)
  Chldark = rbind(Chldark,Chlnext)
  rm(bgachl,MeBGA,MeChl)
}

colnames(BGAdark) = c('year','doy','BGA','lBGA','clBGA','zlBGA')
colnames(Chldark) = c('year','doy','Chl','lChl','clChl','zlChl')

# Doy index for lBGA
dmin = min(BGAdark$doy)
dmax = max(BGAdark$doy)
BGAdark$dindex = BGAdark$year + (BGAdark$doy - dmin)/(dmax-dmin+1)

# Doy index for lChl
dmin = min(Chldark$doy)
dmax = max(Chldark$doy)
Chldark$dindex = Chldark$year + (Chldark$doy - dmin)/(dmax-dmin+1)

windows()
par(mfrow=c(2,1),mar=c(4, 4.5, 1.5, 1) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(BGAdark$dindex,BGAdark$lBGA,type='l',lwd=1,col='blue',xlab='Year',
     ylab='log10 Phycocyanin',main='not centered by year (sensor)')
plot(Chldark$dindex,Chldark$lChl,type='l',lwd=1,col='forestgreen',xlab='Year',
     ylab='log10 Chlorophyll')

windows()
par(mfrow=c(2,1),mar=c(4, 4.5, 1.5, 1) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(BGAdark$dindex,BGAdark$clBGA,type='l',lwd=1,col='blue',xlab='Year',
     ylab='log10 Phycocyanin',main='Centered by year (sensor)')
plot(Chldark$dindex,Chldark$clChl,type='l',lwd=1,col='forestgreen',xlab='Year',
     ylab='log10 Chlorophyll')

windows()
par(mfrow=c(2,1),mar=c(4, 4.5, 1.5, 1) + 0.1, cex.axis=1.8,cex.lab=1.8)
plot(BGAdark$dindex,BGAdark$zlBGA,type='l',lwd=1,col='blue',xlab='Year',
     ylab='log10 Phycocyanin',main='Z scored by year (sensor)')
plot(Chldark$dindex,Chldark$zlChl,type='l',lwd=1,col='forestgreen',xlab='Year',
     ylab='log10 Chlorophyll')

windows(width=12,height=9)
par(mfrow=c(3,1),mar=c(2.8, 4.5, 1.5, 1) + 0.1, cex.axis=1.8,cex.lab=1.8,
    cex.main=1.6)
plot(BGAdark$dindex,BGAdark$lBGA,type='l',lwd=2,col='blue',xlab=' ',
     ylab='log10 Phycocyanin',main='No Transformation')
grid()
plot(BGAdark$dindex,BGAdark$clBGA,type='l',lwd=2,col='blue',xlab=' ',
     ylab='log10 Phycocyanin',main='Centered by year')
grid()
par(mar=c(4, 4.5, 1.5, 1))
plot(BGAdark$dindex,BGAdark$zlBGA,type='l',lwd=2,col='blue',xlab='Year',
     ylab='log10 Phycocyanin',main='Z scored by year')
grid()
#
save(BGAdark,Chldark,file='BGA+Chl_dark_centered_Z_2008-2021.Rdata')


