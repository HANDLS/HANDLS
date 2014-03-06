whichUse = function(dat) {
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat == 'Powder Cocaine', 0, dat)
	dat = ifelse(dat == 'Crack Cocaine', 1, dat)
	dat = ifelse(dat == 'Both', 2, dat)

	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Powder Cocaine','Crack Cocaine','Both'))
	dat
	}

library(RMySQL)

w00Dem = HNDload()

source('/git/Wave03/MedHx/w03MedHx-functions.R')

HNDdb = dbConnect(MySQL(), group='HND')

dbDat = dbGetQuery(HNDdb, 'select * from Wave03.hist_tda_coke order by handls_id asc, WhnStamp asc')
dbIDs = dbGetQuery(HNDdb, 'select cast(handls_ID as char) as HNDid from Wave03.hist_tda_coke order by handls_id asc, WhnStamp asc')

head(dbDat)
head(dbIDs)

mhx = HNDidLegit(cbind(dbIDs, dbDat))
mhx = HNDdupsDelete(mhx)

head(mhx)
str(mhx)

mhx = within(mhx, {
	CokeEver			= isMhx(isEverUsed)
	Coke5Times			= isMhx(isUsed5)
	CokeType			= whichUse(whichDoYouUse)
	CokeShoot			= isMhx(isNeedle)
	CokeSmoke			= isMhx(isSmoke)
	CokeSnort			= isMhx(isSnort)
	CokeSpeedball			= isMhx(isSpeedball)
	CokeAgeFirst			= howOld(howOldWhenFirstUsed)
	CokeTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	CokeTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	CokeEverUsedMore		= isMhx(isEverUsedMoreThanNow)
	CokeTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	CokeTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	CokeTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	CokeTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	CokeTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	CokeTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	CokeTimeSinceAbstainNum		= howLongNum(timeSinceAbstainNum)
	CokeTimeSinceAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	CokeTolerance			= isMhx(isTolerance)
	CokeOD				= isMhx(isOD)
	CokeTreated			= isMhx(isEverBeenTreated)
	CokeTreatedTimes		= howMany(howManyTimesTreated)
	CokeTimeSinceTreatedNum		= howLongNum(timeSinceLastTreatedNum)
	CokeTimeSinceTreatedUnit	= howLongUnit(timeSinceLastTreatedUnit)
	CokeNA				= isMhx(isNA)
	})

mhx = within(mhx, {
	label(CokeEver)				= 'MedHxDrugs: Ever used cocaine/crack?'
	label(Coke5Times)			= 'MedHxDrugs: Used cocaine/crack more than 5 times?'
	label(CokeType)				= 'MedHxDrugs: Do you use powder cocaine or crack?'
	label(CokeShoot)			= 'MedHxDrugs: Shoot cocaine/crack?'
	label(CokeSmoke)			= 'MedHxDrugs: Smoke cocaine/crack?'
	label(CokeSnort)			= 'MedHxDrugs: Snort cocaine/crack?'
	label(CokeSpeedball)			= 'MedHxDrugs: Did speedballs (coke+opiates)?'
	label(CokeAgeFirst)			= 'MedHxDrugs: Age first used coke/crack?'
	label(CokeTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used coke/crack?'
	label(CokeTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used coke/crack?'
	label(CokeEverUsedMore)			= 'MedHxDrugs: Ever a time when you used more crack/cocaine than you do now?'
	label(CokeTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(CokeTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(CokeTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(CokeTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(CokeTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without coke/crack?'
	label(CokeTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without coke/crack?'
	label(CokeTimeSinceAbstainNum)		= 'MedHxDrugs: How long ago (number) was longest period of abstinence from coke/crack?'
	label(CokeTimeSinceAbstainUnit)		= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from coke/crack?'
	label(CokeTolerance)			= 'MedHxDrugs: Ever developed a tolerance to coke/crack?'
	label(CokeOD)				= 'MedHxDrugs: Ever OD on coke/crack?'
	label(CokeTreated)			= 'MedHxDrugs: Ever treated for coke/crack problems?'
	label(CokeTreatedTimes)			= 'MedHxDrugs: How many times treated for coke/crack problems?'
	label(CokeTimeSinceTreatedNum)		= 'MedHxDrugs: How long (number) since last treated for coke/crack problems?'
	label(CokeTimeSinceTreatedUnit)	= 	'MedHxDrugs: How long (unit) since last treated for coke/crack problems?'
	label(CokeNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for cocaine?'
	})

w03MedHxCoke = mhx[,c(1,zNamesRange(mhx, 'CokeEver', 'CokeNA'))]

str(w03MedHxCoke)
describe(w03MedHxCoke)

w03MedHxCoke[w03MedHxCoke$HNDid==8033916701,]

save(w03MedHxCoke, file='/git/Wave03/MedHx/zrdata/w03MedHxCoke.rdata')
