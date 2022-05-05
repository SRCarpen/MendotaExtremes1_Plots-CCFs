# Merge Precip, PLoad, and BGA for as many years as possible

rm(list = ls())
graphics.off()

# Load pigments
#save(BGAdark,Chldark,file='BGA+Chl_dark_centered_v2_2008-2021.Rdata')
# clBGA is centered log BGA, z
load(file='BGA+Chl_dark_centered_Z_2008-2021.Rdata')
print('Mendota Phycocyanin daily dark hours',quote=F)
print(BGAdark[1,])

# Load precip
load('DCRA_Precip_1940-2021.Rdata')
print('Madison DCRA precip dataset',quote=F)
print(datall[1,]) # show first line with column names
# Rename year and doy to match the other data frames
datall$year = datall$YEAR
datall$doy = datall$DOY
PPT = subset(datall,select=c('year','doy','PRCP'))

# Load discharge
# save(PB1,YW1,PBYW2,file='Discharge_PB+YW_1990-2021.Rdata')
load(file='Discharge_PB+YW_1990-2021.Rdata')
print(PBYW2[1,])
Dis0 = subset(PBYW2,subset=(year>=2008 & year<=2021),
              select=c('year','doy','Totdis'))

# Load P loads
load('AnnLoads_PB+YP_1995-2021.Rdata')
Pload$TotPload = Pload$PB.kg.d + Pload$YW.kg.d   # PB + YW combined loads
print('Yahara-Windsor P load dataset',quote=F)
print(Pload[1,]) # show first line with column names

# Merge Precip and Pload for 2008-2021
PPT0821 = subset(PPT,subset=(year>=2008 & year<=2021))
PL0821 = subset(Pload,subset=(year>=2008 & year<=2021))
dat00 = merge(PPT0821,PL0821,by=c('year','doy'))
dat0 = merge(dat00,Dis0,by=c('year','doy'))
print('dimensions of merged precip & disch & P load',quote=F)
print(dim(dat0))
dat1 = na.omit(dat0)
print('dimensions after na.omit',quote=F)
print(dim(dat1))

# Merge Precip +sch + Pload with darkBGA
dat2 = merge(BGAdark,dat1,by=c('year','doy'))
print('dimensions of merged precip & P load & BGAdark',quote=F)
print(dim(dat2))
dat3 = na.omit(dat2)
print('dimensions after na.omit',quote=F)
print(dim(dat3))

print('all variates',quote=F)
print(dat3[1,])

save(dat3,file='PPT_Disch_Pload_BGAdark_2008-2021.Rdata')


