# CCFs by year

rm(list = ls())
graphics.off()

# load data
#save(dat3,file='PPT_Pload_BGAdark_2008-2021.Rdata')
load(file='PPT_Pload_BGAdark_2008-2021.Rdata')

print(colnames(dat3),quote=F)

# Direct relationships of BGA indices with P load
datsub = subset(dat3,select=c('lBGA','zlBGA','TotPload'))
print(cor(datsub))
print(cor.test(datsub$lBGA,datsub$TotPload))
print(cor.test(datsub$zlBGA,datsub$TotPload))

windows(width=12,height=4)
par(mfrow=c(1,3),mar=c(4, 4.2, 1, 2) + 0.1, cex.axis=1.5,cex.lab=1.5)
# Note ccf(x,y) at lag k estimates r for x(t+k) vs y(t)
ccf0 = ccf(dat3$PRCP,dat3$TotPload,lag.max=30,plot=T,main='Precip x Pload')
abline(v=0,lty=2,col='blue')
# note ccf1$acf is the vector of correlation coefficients, 
# ccf1$n.used is N, ccf1$lag is the vector of lags
ccf1 = ccf(dat3$PRCP,dat3$zlBGA,lag.max=30,plot=T,main='Precip x zlBGA')
abline(v=0,lty=2,col='blue')
ccf2 = ccf(dat3$TotPload,dat3$zlBGA,lag.max=30,plot=T,main='Pload x zlBGA')
abline(v=0,lty=2,col='blue')

# Try to combine year-by-year CCFs 
yvec = c(2008:2021)
ny = length(yvec)
cmat = matrix(0,nr=31,nc=ny)
vmat = matrix(0,nr=31,nc=ny)
Nvec = rep(0,ny)
unit = rep(1,31)
lags = ccf0$lag[1:31]
#
# loop over years
for(i in 1:ny) {
  dxy = subset(dat3,subset=(year == yvec[i]))
  # Choose x ("driver") variate
  #x = dxy$PRCP
  #x = log10(dxy$PRCP+1)
  x = log10(dxy$TotPload)
  # choose y ("response") variate
  #y = log10(dxy$TotPload)
  y = dxy$zlBGA
  corfun = ccf(x,y,lag.max=30,plot=F)
  cmat[,i] = corfun$acf[1:31]
  N = corfun$n.used
  Nvec[i] = N
  df = N-2
  vmat[,i] = (unit - corfun$acf[1:31]^2)/df
}

ccf.pool = rep(0,31)
sd.pool = rep(0,31)
# Calculate pooled ccf by inverse-variance weighting over all columns of cmat
for(i in 1:31) {  # loop over lags
  rvec = cmat[i,]
  vvec = vmat[i,]
  den = rep(0,ny) # dummy variable to hold denominator of weighted mean
  num = rep(0,ny) # dummy variable to hold numerator of weighted mean
  for(j in 1:ny)  { #loop over years
    wts = 1/vmat[i,j] # inverse variance
    den[j] = wts
    num[j] = cmat[i,j]*wts
  }
  # calculate pooled r for the lag
  ccf.pool[i] = sum(num,na.rm=T)/sum(den,na.rm=T)
  varspool = vmat[i,]/Nvec # square of s.e.
  sd.pool[i] = sqrt(sum(varspool,na.rm=T))
}

windows()
par(mfrow=c(1,1),mar=c(4, 4.2, 2, 2) + 0.1, cex.axis=1.8,cex.lab=1.8,cex.main=1.6)
plot(lags,ccf.pool,type='b',lwd=2,pch=19,cex=1.1,col='blue',xlab='Lag, days',
     ylab='Pooled Correlation',main='P Load Leads Phycocyanin')
abline(h=sd.pool,lwd=2,lty=2,col='red')
abline(h=-1*sd.pool,lwd=2,lty=2,col='red')
# for chattering sd
#musdpool = mean(sd.pool)
#abline(h=musdpool,lwd=2,lty=2,col='red')
#abline(h=-1*musdpool,lwd=2,lty=2,col='red')
grid()
