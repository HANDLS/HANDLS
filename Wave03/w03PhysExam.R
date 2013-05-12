getPEdat = function(db, tbl) {

	withCallingHandlers({
		PE = dbGetQuery(db, paste('select * from Wave03.', tbl, sep=''))
		message('continuing')
		},
		warning=function(w) {
			message('handling: ', conditionMessage(w))
			invokeRestart("muffleWarning")
		})

	print(dim(PE))
	# print(zUnique(PE, 'HNDid'))
	return(PE)
	}

getPEids = function(db, dat, tbl) {

	ID = dbGetQuery(db, paste('select cast(HNDid as char) as HNDid from Wave03.', tbl, sep=''))
	print(dim(ID))
	print(dim(dat))
	dat$HNDid = as.numeric(ID$HNDid)
	dat$seqN  = sequence(rle(dat$HNDid)$lengths)
	dat$jnk   = dat$seqN
	datMelt = melt(dat, id.vars=zQ(HNDid,seqN), measure.vars='jnk')
	datCast = cast(datMelt, HNDid~seqN)
	datCast$seqN = ncol(datCast) - rowSums(is.na(datCast)) - 1
	dat = merge(dat, datCast[,zQ(HNDid,seqN)], by=zQ(HNDid,seqN))
	print(dim(dat))
	return(dat[order(dat$HNDid),!(names(dat) %in% zQ(seqN,physID,dtsPHPSQL,datetimein,datetimeout,vdatemo,vdatedy,vdateyr,WhnStamp,WhnXfer,jnk))])
	}

library(doBy)
library(RMySQL)
library(reshape)
library(zStat)

db = dbConnect(MySQL(), group='HND-Wave03')

PE1 = getPEdat(db, 'PhysExmGeneral')
PE2 = getPEdat(db, 'PhysExmEyesEar')
PE3 = getPEdat(db, 'PhysExmHeadNeck')
PE4 = getPEdat(db, 'PhysExmAbdomen')
PE5 = getPEdat(db, 'PhysExmMuscle')
PE6 = getPEdat(db, 'PhysExmNervous')
PE7 = getPEdat(db, 'PhysExmLymph')

PE1 = getPEids(db, PE1, 'PhysExmGeneral')
PE2 = getPEids(db, PE2, 'PhysExmEyesEar')
PE3 = getPEids(db, PE3, 'PhysExmHeadNeck')
PE4 = getPEids(db, PE4, 'PhysExmAbdomen')
PE5 = getPEids(db, PE5, 'PhysExmMuscle')
PE6 = getPEids(db, PE6, 'PhysExmNervous')
PE7 = getPEids(db, PE7, 'PhysExmLymph')

PE1$username = NULL

PE = merge(PE1, PE2, by='HNDid', all=T)
PE = merge(PE,  PE3, by='HNDid', all=T)
PE = merge(PE,  PE4, by='HNDid', all=T)
PE = merge(PE,  PE5, by='HNDid', all=T)
PE = merge(PE,  PE6, by='HNDid', all=T)
PE = merge(PE,  PE7, by='HNDid', all=T)

PE$HNDwave = 3

dim(PE)
PE = PE[PE$HNDid>=8030000000 & PE$HNDid<=8229999999,]
dim(PE)

source('/prj/hnd/Wave03/znlz/PhysExam/2013-02-16-labels.txt')

chkRange = function(dat) {
	dat[which(dat==0)] = NA
	}

summary(PE$hip)
PE$waist = chkRange(PE$hip)

summary(PE$waist)

head(PE[,zQ(HNDid,waist, hip, whratio)])
head(PE[,zQ(HNDid,seqN,physID,dtsPHPSQL,datetimeout,vdatemo,vdatedy,vdateyr)])

PE1b = PE1a[,!(names(PE1a) %in% zQ(HNDid,seqN,physID,dtsPHPSQL,datetimein,datetimeoutvdatemo,vdatedy,vdateyr))]
PE1b = subset(PE1a, -zQ(HNDid,seqN,physID,dtsPHPSQL,datetimeout,vdatemo,vdatedy,vdateyr))
names(PE1a)

for Marc:

names(PE)

Marc = PE[,zQ(HNDid,waist,hip,whratio,weight,height,bmi)]
dim(Marc)
summary(Marc)
save(Marc, file='~/Desktop/Wave3-PE.rdata', compress=F)
