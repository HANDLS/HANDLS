# zRenameVar = function(dat, oldName, newName) {
#
# 	names(dat)[names(dat)==oldName] = newName
# 	dat
# 	}

library(date)
library(RMySQL)

HNDdb = dbConnect(MySQL(), group='HND')

DXAdat = dbGetQuery(HNDdb, 'select * from Hologic_Dexa.HologicDXAdata_complete order by HNDid')
DXArep = dbGetQuery(HNDdb, 'select * from Wave03unrep.DXAreport order by HNDid')
IDSdat = dbGetQuery(HNDdb, 'select cast(HNDid as char) as HNDid from Hologic_Dexa.HologicDXAdata_complete order by HNDid')
IDSrep = dbGetQuery(HNDdb, 'select cast(HNDid as char) as HNDid from Wave03unrep.DXAreport order by HNDid')

DXArep$HNDid = as.numeric(IDSrep$HNDid)

DXAdat = within(DXAdat, {
	HNDid = as.numeric(IDSdat$HNDid)
	HNDwave = 3
	whnStudy = as.Date(whnStudy)
	})

# zQuick(DXAdat)
# zQuick(DXArep)

dim(DXAdat)
summary(DXAdat$whnStudy)
DXAdat = DXAdat[DXAdat$whnStudy>as.Date('2009-04-01'),]
dim(DXAdat)
summary(DXAdat$whnStudy)

w03DXA = merge(DXAdat, DXArep, by='HNDid')

varDXA = zQ(HNDid, HNDwave, BodyFatPercent)
varDXA = c(varDXA, zNamesRangeSel(w03DXA, 'dx.x', 'dxZ.x'), zNamesRangeSel(w03DXA, 'hipNeckArea', 'bodyBCF'))
w03DXA = w03DXA[,varDXA]
zQuick(w03DXA)

w03DXA = zRenameVar(w03DXA, 'dx.x',  'dx')
w03DXA = zRenameVar(w03DXA, 'dxB.x', 'dxB')
w03DXA = zRenameVar(w03DXA, 'dxT.x', 'dxT')
w03DXA = zRenameVar(w03DXA, 'dxZ.x', 'dxZ')

save(w03DXA, file='/prj/hnd/zrdata/w03DXA.rdata')

#------------

library(HANDLS)

# HNDdataTbl = function(dat) {
#
# 	dfn.nam = deparse(substitute(dat))
# 	dfn.dat = HNDdataDoc(dat, dfn.nam)
#
# 	print(dfn.nam)
# 	print(dfn.dat)
#
# 	library(RMySQL)
#
# 	dbHNDdataDoc = dbConnect(MySQL(), group='HNDdataDoc')
#
# 	dbWriteTable(dbHNDdataDoc, 'varNames', dfn.dat$vars, row.names=F, overwrite=F, append=T)
#
# 	if (nrow(dfn.dat$vals)>0) dbWriteTable(dbHNDdataDoc, 'fmtNames', dfn.dat$vals, row.names=F, overwrite=F, append=T)
# 	}
#
# HNDdataTbl(w03DXA)
#
# test = function(dat) {
#
#
# 	return(dfn)
# 	}
#
# (test(w03DXA))