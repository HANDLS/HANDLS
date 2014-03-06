library(RMySQL)

w00Dem = HNDload()

source('/git/Wave03/MedHx/w03MedHx-functions.R')

HNDdb = dbConnect(MySQL(), group='HND')

dbDat = dbGetQuery(HNDdb, 'select * from Wave03.hist_tda_marij order by handls_id asc, WhnStamp asc')
dbIDs = dbGetQuery(HNDdb, 'select cast(handls_ID as char) as HNDid from Wave03.hist_tda_marij order by handls_id asc, WhnStamp asc')

head(dbDat)
head(dbIDs)

mhx = HNDidLegit(cbind(dbIDs, dbDat))
mhx = HNDdupsDelete(mhx)

head(mhx)
str(mhx)

mhx = within(mhx, {
	MarijEver			= isMhx(isEverUsed)
	Marij5Times			= isMhx(isUsed5)
	MarijAgeFirst			= howOld(howOldWhenFirstUsed)
	MarijTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	MarijTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	MarijEverUsedMore		= isMhx(isEverUsedMoreThanNow)
	MarijTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	MarijTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	MarijTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	MarijTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	MarijTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	MarijTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	MarijTimeSinceAbstainNum	= howLongNum(timeSinceAbstainNum)
	MarijTimeSinceAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	MarijTolerance			= isMhx(isTolerance)
	MarijTreated			= isMhx(isEverBeenTreated)
	MarijTreatedTimes		= howMany(howManyTimesTreated)
	MarijTimeSinceTreatedNum	= howLongNum(timeSinceLastTreatedNum)
	MarijTimeSinceTreatedUnit	= howLongUnit(timeSinceLastTreatedUnit)
	MarijNA				= isMhx(isNA)
	})

mhx = within(mhx, {
	label(MarijEver)			= 'MedHxDrugs: Ever used marijuana?'
	label(Marij5Times)			= 'MedHxDrugs: Used marijuana more than 5 times?'
	label(MarijAgeFirst)			= 'MedHxDrugs: Age first used marijuana?'
	label(MarijTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used marijuana?'
	label(MarijTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used marijuana?'
	label(MarijEverUsedMore)		= 'MedHxDrugs: Ever a time when you used more marijuana than you do now?'
	label(MarijTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(MarijTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(MarijTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(MarijTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(MarijTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without marijuana?'
	label(MarijTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without marijuana?'
	label(MarijTimeSinceAbstainNum)		= 'MedHxDrugs: How long ago (number) was longest period of abstinence from marijuana?'
	label(MarijTimeSinceAbstainUnit)	= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from marijuana?'
	label(MarijTolerance)			= 'MedHxDrugs: Ever developed a tolerance to marijuana?'
	label(MarijTreated)			= 'MedHxDrugs: Ever treated for marijuana problems?'
	label(MarijTreatedTimes)		= 'MedHxDrugs: How many times treated for marijuana problems?'
	label(MarijTimeSinceTreatedNum)		= 'MedHxDrugs: How long (number) since last treated for marijuana problems?'
	label(MarijTimeSinceTreatedUnit)	= 'MedHxDrugs: How long (unit) since last treated for marijuana problems?'
	label(MarijNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for marijuana?'
	})

w03MedHxMarij = mhx[,c(1,zNamesRange(mhx, 'MarijEver', 'MarijNA'))]

str(w03MedHxMarij)
describe(w03MedHxMarij)

w03MedHxMarij[w03MedHxMarij$HNDid==8033916701,]

save(w03MedHxMarij, file='/git/Wave03/MedHx/zrdata/w03MedHxMarij.rdata')
