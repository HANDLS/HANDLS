library(RMySQL)

w00Dem = HNDload()

source('/git/Wave03/MedHx/w03MedHx-functions.R')

HNDdb = dbConnect(MySQL(), group='HND')

dbDat = dbGetQuery(HNDdb, 'select * from Wave03.hist_tda_heroin order by handls_id asc, WhnStamp asc')
dbIDs = dbGetQuery(HNDdb, 'select cast(handls_ID as char) as HNDid from Wave03.hist_tda_heroin order by handls_id asc, WhnStamp asc')

head(dbDat)
head(dbIDs)

mhx = HNDidLegit(cbind(dbIDs, dbDat))
mhx = HNDdupsDelete(mhx)

head(mhx)
str(mhx)

mhx = within(mhx, {
	OpiateEver			= isMhx(isEverUsed)
	Opiate5Times			= isMhx(isUsed5)
	OpiateHeroin			= isMhx(isHeroin)
	OpiateMethadone			= isMhx(isMeth)
	OpiateMorphine			= isMhx(isMorph)
	OpiateCodeine			= isMhx(isCode)
	OpiateSpeedball			= isMhx(isSpeedball)
	OpiateShoot			= isMhx(isNeedle)
	OpiateSmoke			= isMhx(isSmoke)
	OpiateSnort			= isMhx(isSnort)
	OpiatePill			= isMhx(isPills)
	OpiateLiquid			= isMhx(isLiquid)
	OpiateAgeFirst			= howOld(howOldWhenFirstUsed)
	OpiateTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	OpiateTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	OpiateEverUsedMore		= isMhx(isEverUsedMoreThanNow)
	OpiateTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	OpiateTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	OpiateTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	OpiateTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	OpiateTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	OpiateTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	OpiateTimeSinceAbstainNum	= howLongNum(timeSinceAbstainNum)
	OpiateTimeSinceAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	OpiateTolerance			= isMhx(isTolerance)
	OpiateOD			= isMhx(isOD)
	OpiateTreated			= isMhx(isEverBeenTreated)
	OpiateTreatedTimes		= howMany(howManyTimesTreated)
	OpiateTimeSinceTreatedNum	= howLongNum(timeSinceLastTreatedNum)
	OpiateTimeSinceTreatedUnit	= howLongUnit(timeSinceLastTreatedUnit)
	OpiateMethadoneMaint		= isMhx(isEverOnMethMaint)
	OpiateTimeSinceMethNum		= howLongNum(timeSinceLastMethMaintNum)
	OpiateTimeSinceMethUnit		= howLongUnit(timeSinceLastMethMaintUnit)
	OpiateTimeTotalMethNum		= howLongNum(totalTimeOnMethMaintNum)
	OpiateTimeTotalMethUnit		= howLongUnit(totalTimeOnMethMaintUnit)
	OpiateNA			= isMhx(isNA)
	})

mhx = within(mhx, {
	label(OpiateEver)			= 'MedHxDrugs: Ever used heroin/morphine/codeine?'
	label(Opiate5Times)			= 'MedHxDrugs: Used heroin/morphine/codeine more than 5 times?'
	label(OpiateHeroin)			= 'MedHxDrugs: Used heroin?'
	label(OpiateMethadone)			= 'MedHxDrugs: Used methadone (not methadone maint.)?'
	label(OpiateMorphine)			= 'MedHxDrugs: Used morphine?'
	label(OpiateCodeine)			= 'MedHxDrugs: Used codeine?'
	label(OpiateSpeedball)			= 'MedHxDrugs: Did speedballs (coke+opiates)?'
	label(OpiateShoot)			= 'MedHxDrugs: Shot opiates?'
	label(OpiateSmoke)			= 'MedHxDrugs: Smoked opiates?'
	label(OpiateSnort)			= 'MedHxDrugs: Snorted opiates?'
	label(OpiatePill)			= 'MedHxDrugs: Used opiates in pill form?'
	label(OpiateLiquid)			= 'MedHxDrugs: Used opiates in liquid form?'
	label(OpiateAgeFirst)			= 'MedHxDrugs: Age first used opiates?'
	label(OpiateTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used opiates?'
	label(OpiateTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used opiates?'
	label(OpiateEverUsedMore)		= 'MedHxDrugs: Ever a time when you used more heroin/morphine/codeine than you do now?'
	label(OpiateTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(OpiateTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(OpiateTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(OpiateTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(OpiateTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without opiates?'
	label(OpiateTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without opiates?'
	label(OpiateTimeSinceAbstainNum)	= 'MedHxDrugs: How long ago (number) was longest period of abstinence from opiates?'
	label(OpiateTimeSinceAbstainUnit)	= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from opiates?'
	label(OpiateTolerance)			= 'MedHxDrugs: Ever developed a tolerance to opiates?'
	label(OpiateOD)				= 'MedHxDrugs: Ever OD on opiates?'
	label(OpiateTreated)			= 'MedHxDrugs: Ever treated for opiate problems?'
	label(OpiateTreatedTimes)		= 'MedHxDrugs: How many times treated for opiate problems?'
	label(OpiateTimeSinceTreatedNum)	= 'MedHxDrugs: How long (number) since last treated for opiate problems?'
	label(OpiateTimeSinceTreatedUnit)	= 'MedHxDrugs: How long (unit) since last treated for opiate problems?'
	label(OpiateMethadoneMaint)		= 'MedHxDrugs: Ever on methadone maintenance?'
	label(OpiateTimeSinceMethNum)		= 'MedHxDrugs: How long (number) since last on methadone maintenance?'
	label(OpiateTimeSinceMethUnit)		= 'MedHxDrugs: How long (unit) since last on methadone maintenance?'
	label(OpiateTimeTotalMethNum)		= 'MedHxDrugs: How long (number) total on methadone maintenance?'
	label(OpiateTimeTotalMethUnit)		= 'MedHxDrugs: How long (unit) total on methadone maintenance'
	label(OpiateNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for heroin/morphine/codeine?'
	})

w03MedHxOpiates = mhx[,c(1,zNamesRange(mhx, 'OpiateEver', 'OpiateNA'))]

str(w03MedHxOpiates)
describe(w03MedHxOpiates)

w03MedHxOpiates[w03MedHxOpiates$HNDid==8033916701,]

save(w03MedHxOpiates, file='/git/Wave03/MedHx/zrdata/w03MedHxOpiates.rdata')
