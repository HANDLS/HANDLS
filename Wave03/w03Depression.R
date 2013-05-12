library(doBy)
library(RMySQL)
library(zStat)

db = dbConnect(MySQL(), group='HND-xform')

varSQL = zNameSeq('CES', 1:20)

str = varSQL[1]

for	(j in 2:length(varSQL)) {
	str = paste(str, ',', varSQL[j], sep='')
	}

SQL1 = paste0('select cast(HNDid as char) as HNDid, CESTAG,', str, ',WhnVrf from COGW03V01CES')
SQL3 = paste0('select cast(HNDid as char) as HNDid, CESTAG,', str, ',WhnVrf from COGW03V03CES')

w03v01CES = dbGetQuery(db, SQL1)
w03v03CES = dbGetQuery(db, SQL3)

w03v01CES$HNDid = as.numeric(w03v01CES$HNDid)
w03v03CES$HNDid = as.numeric(w03v03CES$HNDid)

# w03v01CES = w03v01CES[!is.na(w03v01CES$WhnVrf),]
# w03v03CES = w03v03CES[!is.na(w03v03CES$WhnVrf),]

w03v01CES$WhnVrf = NULL
w03v03CES$WhnVrf = NULL

str(w03v01CES)
str(w03v03CES)

dim(w03v01CES)
summary(w03v01CES)
dim(w03v03CES)
summary(w03v03CES)

w03Dep = rbind(w03v01CES, w03v03CES)
itmRng = zNamesRange(w03Dep, 'CES01', 'CES20')

for	(j in itmRng) {
	w03Dep[!is.na(w03Dep[,j])&w03Dep[,j]>3,j] = NA
	}

w03Dep$HNDwave = 3

dim(w03Dep)
summary(w03Dep)

w03Dep = unique(w03Dep)

dim(w03Dep)
summary(w03Dep)

zCor(w03Dep[,itmRng])

w03Dep$CES = rowSums(w03Dep[,zQ(CES01,CES02,CES03,CES05,CES06,CES07,CES09,CES10,CES11,CES13,CES14,CES15,CES17,CES18,CES19,CES20)])
w03Dep$CES = w03Dep$CES + 12 - rowSums(w03Dep[,zQ(CES04,CES08,CES12,CES16)])

w03Dep$CEScut16 = cut2(w03Dep$CES, cuts=c(16))
w03Dep$CEScut20 = cut2(w03Dep$CES, cuts=c(20))

summaryBy(CES~CEScut16, data=w03Dep[!is.na(w03Dep$CES),], FUN=zByFUN)
summaryBy(CES~CEScut20, data=w03Dep[!is.na(w03Dep$CES),], FUN=zByFUN)

levels(w03Dep$CEScut16) = zQ('Not depressed', Depressed)
levels(w03Dep$CEScut20) = zQ('Not depressed', Depressed)

varOrder = zQ(HNDid,HNDwave,CESTAG,CES,CEScut16,CEScut20,CES01,CES02,CES03,CES04,CES05,CES06,CES07,CES08,CES09,CES10,CES11,CES12,CES13,CES14,CES15,CES16,CES17,CES18,CES19,CES20)

w03Dep = w03Dep[order(w03Dep$HNDid),varOrder]

for	(k in zNamesRange(w03Dep, 'CES01', 'CES20')) {
	w03Dep[,k] = zR(w03Dep[,k], FUN=factor, levels=0:3, labels=zQ(Rarely, Sometimes, Occasionally, Mostly), skip=T)
	}

head(w03Dep)

label(w03Dep$HNDid)	= 'HANDLS identification'
label(w03Dep$HNDwave)	= 'HANDLS wave'
label(w03Dep$CES)	= 'CES-Depression: Scale score'
label(w03Dep$CES01)	= 'CES-Depression: Bothered by things'
label(w03Dep$CES02)	= 'CES-Depression: Not feel like eating, poor appetite'
label(w03Dep$CES03)	= 'CES-Depression: Could not shake off blues'
label(w03Dep$CES04)	= 'CES-Depression: Felt just as good as other people'
label(w03Dep$CES05)	= 'CES-Depression: Trouble keeping mind on task'
label(w03Dep$CES06)	= 'CES-Depression: Felt depressed'
label(w03Dep$CES07)	= 'CES-Depression: Everything was an effort'
label(w03Dep$CES08)	= 'CES-Depression: Hopeful about the future'
label(w03Dep$CES09)	= 'CES-Depression: Life had been a failure'
label(w03Dep$CES10)	= 'CES-Depression: Felt fearful'
label(w03Dep$CES11)	= 'CES-Depression: Sleep was restless'
label(w03Dep$CES12)	= 'CES-Depression: Happy'
label(w03Dep$CES13)	= 'CES-Depression: Talked less than usual'
label(w03Dep$CES14)	= 'CES-Depression: Felt lonely'
label(w03Dep$CES15)	= 'CES-Depression: People unfriendly'
label(w03Dep$CES16)	= 'CES-Depression: Enjoyed life'
label(w03Dep$CES17)	= 'CES-Depression: Had crying spells'
label(w03Dep$CES18)	= 'CES-Depression: Felt sad'
label(w03Dep$CES19)	= 'CES-Depression: Felt people disliked me'
label(w03Dep$CES20)	= 'CES-Depression: Could not get going'
label(w03Dep$CEScut16)	= 'CES-Depression: Symptoms of depression (16 cut-off)'
label(w03Dep$CEScut20)	= 'CES-Depression: Symptoms of depression (20 cut-off)'
label(w03Dep$CESTAG)	= 'CES-Depression: Data tag'

describe(w03Dep)
# HNDdataTbl(w03Dep)
save(w03Dep, file='/prj/hnd/zrdata/w03Depression.rdata')