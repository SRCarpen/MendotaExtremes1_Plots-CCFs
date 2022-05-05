# Read & organize Mendota 1-minute sonde data
# SRC 2018-10-01

rm(list = ls())
graphics.off()

sonde0 = read.csv('DavidBuoy2021.csv')
fname = c('Me_1min_2021.Rdata')
print('rows in HF sonde file',quote=F)
print(sonde0[1,])

# Create DoY
#sdate2 = as.POSIXlt(sonde0$sampledate, format="%Y-%m-%d")
#sonde0$daynum = sdate2$yday + 1
#sonde0$year4 = sdate2$year + 1900

# Create time of day
ToD = as.POSIXlt(sonde0$sampletime, format="%H:%M:%S")
pday = (ToD$hour*60 + ToD$min)/(60*24)  # decimal fraction of a day
sonde0$DoY = sonde0$daynum + pday  # decimalday of year

# select variables to keep
sonde1 = subset(sonde0,select=c(year4,DoY,wind_speed,chlor_rfu,phyco_rfu))

# variables to analyze
Year = sonde1$year4
DOY = sonde1$DoY
v.wind = sonde1$wind_speed
Chl = sonde1$chlor_rfu
BGA = sonde1$phyco_rfu

# log transform
adjChl = min(Chl,na.rm=T)-1
Chl = Chl-adjChl
lChl = log10(Chl)
adjBGA = min(BGA,na.rm=T) - 1
BGA = BGA - adjBGA
lBGA = log10(BGA)

# Build datasets with no missing values
#
MeBGA0 = as.data.frame( cbind(Year,DOY,v.wind,BGA,lBGA) )
MeBGA1 = na.omit(MeBGA0)
MeBGA = MeBGA1[with(MeBGA1, order(Year,DOY)), ]
print('',quote=F)
print('dims of BGA dataframes with & w/o NA',quote=F)
print(dim(MeBGA0))
print(dim(MeBGA))
#
MeChl0 = as.data.frame( cbind(Year,DOY,v.wind,Chl,lChl) )
MeChl1 = na.omit(MeChl0)
MeChl = MeChl1[with(MeChl1, order(Year,DOY)), ]
print('',quote=F)
print('dims of Chl dataframes with & w/o NA',quote=F)
print(dim(MeChl0))
print(dim(MeChl))

# Test plots
windows()
par(mfrow=c(2,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
plot(DOY,v.wind,type='l')
grid()
plot(DOY,lBGA,type='l')
grid()

# Save cleaned-up Mendota 1-minute data frames
# Each data frame has year, decimal DOY, pigment, log10 pigment-minimum+1
save(MeBGA,MeChl,file=fname)

