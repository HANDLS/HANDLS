library(RMySQL)

w00Dem = HNDload()

source('/git/Wave03/MedHx/w03MedHx-functions.R')

HNDdb = dbConnect(MySQL(), group='HND')

dbDat = dbGetQuery(HNDdb, 'select * from Wave03.hist_tda_alc')
dbIDs = dbGetQuery(HNDdb, 'select cast(handls_ID as char) as HNDid from Wave03.hist_tda_alc')

head(dbDat)
head(dbIDs)

mhx = HNDidLegit(cbind(dbIDs, dbDat))
mhx = HNDdupsDelete(mhx)

head(mhx)
str(mhx)

mhx = within(mhx, {
	AlcEver			= isMhx(isEverDrank)
	Alc5Times		= isMhx(isDrank5)
	AlcAgeFirst		= howOld(howOldWhenFirstDrank)
	AlcTimeSinUsedNum	= howLongNum(howLongSinceLastDrinkNum)
	AlcTimeSinUsedUnit	= howLongUnit(howLongSinceLastDrinkUnit)
	AlcYouTooMuch		= isMhx(youThinkDrinkTooMuch)
	AlcFamTooMuch		= isMhx(famThinkDrinkTooMuch)
	AlcDocTooMuch		= isMhx(docThinkDrinkTooMuch)
	AlcYouCutDown		= isMhx(youThinkCutDown)
	AlcCauseProbs		= isMhx(isLifeFallingApart)
	AlcDrankMore		= isMhx(isEverDrankMoreThanNow)
	AlcTimeSinDrankMoreNum	= howLongNum(howLongSinceLastDrankThisMuchNum)
	AlcTimeSinDrankMoreUnit	= howLongUnit(howLongSinceLastDrankThisMuchUnit)
	AlcLongDrinkMoreNum	= howLongNum(howLongDidYouDrinkThisMuchNum)
	AlcLongDrinkMoreUnit	= howLongUnit(howLongDidYouDrinkThisMuchUnit)
	AlcTimeLongAbstainNum	= howLongNum(longestWithoutDrinkNum)
	AlcTimeLongAbstainUnit	= howLongUnit(longestWithoutDrinkUnit)
	AlcTimeSinAbstainNum	= howLongNum(timeSinceAbstainNum)
	AlcTimeSinAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	AlcTolerance		= isMhx(isTolerance)
	AlcDrinkAM		= isMhx(isDrinkToCalmDown)
	AlcForget		= isMhx(isForgotLastNight)
	AlcDT			= isMhx(isHadDts)
	AlcEverTreated		= isMhx(isEverBeenTreated)
	AlcTreatedTimes		= howMany(howManyTimesTreated)
	AlcTimeSinTreatNum	= howLongNum(timeSinceLastTreatedNum)
	AlcTimeSinTreatUnit	= howLongUnit(timeSinceLastTreatedUnit)
	AlcAA			= isMhx(isAA)
	})

mhx = within(mhx, {
	label(AlcEver)			= 'MedHx-Alc: Since your last visit, have you had a drink of alcohol?'
	label(Alc5Times)		= 'MedHx-Alc: Have you had more than 5 alcoholic drinks in your life?'
	label(AlcAgeFirst)		= 'MedHx-Alc: How old were you when you had your first drink?'
	label(AlcTimeSinUsedNum)	= 'MedHx-Alc: How long (number) has it been since you had a drink?'
	label(AlcTimeSinUsedUnit)	= 'MedHx-Alc: How long (unit) has it been since you had a drink?'
	label(AlcYouTooMuch)		= 'MedHx-Alc: Past 6 months, did you ever think that you were drinking too much?'
	label(AlcFamTooMuch)		= 'MedHx-Alc: Past 6 months, did anyone in your family think that you were drinking too much?'
	label(AlcDocTooMuch)		= 'MedHx-Alc: Past 6 months, did friends/doctor/other think that you were drinking too much?'
	label(AlcYouCutDown)		= 'MedHx-Alc: Past 6 months, did you think about cutting down or limiting your drinking?'
	label(AlcCauseProbs)		= 'MedHx-Alc: Past 6 months, problems in your life because of your drinking?'
	label(AlcDrankMore)		= 'MedHx-Alc: Was there ever a time when you drank more than you do right now?'
	label(AlcTimeSinDrankMoreNum)	= 'MedHx-Alc: How long (number) has it been since you drank this much?'
	label(AlcTimeSinDrankMoreUnit)	= 'MedHx-Alc: How long (unit) has it been since you drank this much?'
	label(AlcLongDrinkMoreNum)	= 'MedHx-Alc: How long (number) did you drink this much?'
	label(AlcLongDrinkMoreUnit)	= 'MedHx-Alc: How long (unit) did you drink this much?'
	label(AlcTimeLongAbstainNum)	= 'MedHx-Alc: What is the longest (number) you have ever gone without a drink?'
	label(AlcTimeLongAbstainUnit)	= 'MedHx-Alc: What is the longest (unit) you have ever gone without a drink?'
	label(AlcTimeSinAbstainNum)	= 'MedHx-Alc: How long ago (number) was this?'
	label(AlcTimeSinAbstainUnit)	= 'MedHx-Alc: How long ago (unit) was this?'
	label(AlcTolerance)		= 'MedHx-Alc: Have you ever built up a tolerance for alcohol?'
	label(AlcDrinkAM)		= 'MedHx-Alc: Have you ever had a drink first thing in the morning?'
	label(AlcForget)		= 'MedHx-Alc: Have you ever awoken after drinking and could not remember night before?'
	label(AlcDT)			= 'MedHx-Alc: Have you ever had DTs?'
	label(AlcEverTreated)		= 'MedHx-Alc: Since your last visit, have you been treated for an alcohol problem?'
	label(AlcTreatedTimes)		= 'MedHx-Alc: How many times have you been treated for alcohol problems?'
	label(AlcTimeSinTreatNum)	= 'MedHx-Alc: How long (number) has it been since you were last treated?'
	label(AlcTimeSinTreatUnit)	= 'MedHx-Alc: How long (unit) has it been since you were last treated?'
	label(AlcAA)			= 'MedHx-Alc: Do you currently go to AA or any other support group for alcohol?'
	})

w03MedHxAlc = mhx[,c(1,zNamesRange(mhx, 'AlcEver', 'AlcAA'))]

str(w03MedHxAlc)
describe(w03MedHxAlc)

w03MedHxAlc[w03MedHxAlc$HNDid==8033916701,]

save(w03MedHxAlc, file='/git/Wave03/MedHx/zrdata/w03MedHxAlc.rdata')
