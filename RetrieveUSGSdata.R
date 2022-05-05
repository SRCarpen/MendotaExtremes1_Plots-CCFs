# Test dataRetrieval() package
# SRC 2019-07-20

# see https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html 

rm(list = ls())
graphics.off()

library(dataRetrieval)

# Pheasant Branch
sN.PB = '05427948'
PB.info = readNWISsite(sN.PB)

# Yahara Windsor
sN.YW = '05427718'
YW.info = readNWISsite(sN.YW)

# discharge cfs
pCD.dis = '00060'

# Pload short tons/day
#pCD.pload = '91007'  # tons/day
pCD.pload = '91050'  # lbs/day

# orthophosphate load 
pCD.ortho = '91060' # lbs/day

# dates
day0 = '1995-01-01'
#day0 = '2008-01-01'
#day0 = '2020-01-01'
dayN = '2021-12-31'  

# Read Pheasant Branch
PB.dis = readNWISdv(siteNumber = sN.PB, parameterCd = pCD.dis, startDate = day0, endDate = dayN)
PB.pload = readNWISdv(siteNumber = sN.PB, parameterCd = pCD.pload, startDate = day0, endDate = dayN)
PB.pload$PB.kg.d = PB.pload$X_91050_00003*0.453592
PB.ortho = readNWISdv(siteNumber = sN.PB, parameterCd = pCD.ortho, startDate = day0, endDate = dayN)
PB.ortho$PB.po4kg.d = PB.ortho$X_91060_00003*0.453592
# convert date to year and doy
PBdate1 = as.POSIXlt(PB.pload$Date, format="%Y-%m-%d")
PB.pload$year = PBdate1$year + 1900
PB.pload$doy = PBdate1$yday + 1
#
print(c('dim PB.pload',dim(PB.pload)),quote=F)
print('rows in PB.pload',quote=F)
print(PB.pload[1:3,])

# test plot
windows()
plot(PBdate1,PB.pload$PB.kg.d,type='l')

# Read Yahara Windsor
YW.dis = readNWISdv(siteNumber = sN.YW, parameterCd = pCD.dis, startDate = day0, endDate = dayN)
YW.pload = readNWISdv(siteNumber = sN.YW, parameterCd = pCD.pload, startDate = day0, endDate = dayN)
YW.pload$YW.kg.d = YW.pload$X_91050_00003*0.453592
YW.ortho = readNWISdv(siteNumber = sN.YW, parameterCd = pCD.ortho, startDate = day0, endDate = dayN)
YW.ortho$YW.po4kg.d = YW.ortho$X_91060_00003*0.453592
# convert date to year and doy
YWdate1 = as.POSIXlt(YW.pload$Date, format="%Y-%m-%d")
YW.pload$year = YWdate1$year + 1900
YW.pload$doy = YWdate1$yday + 1
#
print(c('dim YW.pload',dim(YW.pload)),quote=F)
print('rows in YW.pload',quote=F)
print(YW.pload[1:3,])

# test plot
windows()
plot(YWdate1,YW.pload$YW.kg.d,type='l')

# Merge data we need into a single file 
PB.sub = subset(PB.pload,select=c(year,doy,PB.kg.d))
PB.po4 = PB.ortho$PB.po4kg.d                
YW.sub = subset(YW.pload,select=c(YW.kg.d))
YW.po4 = YW.ortho$YW.po4kg.d 
# Choose columns for Pload data frame
#Pload = cbind(PB.sub,PB.po4,YW.sub,YW.po4)
Pload = cbind(PB.sub,YW.sub)

# Check file extent in time
print('note last day sampled',quote=F)
PLdim = dim(Pload)
rlast = PLdim[1]
print(Pload[rlast,])

#save(Pload,file='Ploads+ortho_PB+YP_March-Sept_2021.Rdata')
#save(Pload,file='Ploads+ortho_PB+YP_2021.Rdata')
save(Pload,PB.sub,YW.sub,file='AnnLoads_PB+YP_1995-2021.Rdata')

