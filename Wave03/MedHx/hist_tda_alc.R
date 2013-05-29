# ***folder name for saved files [include terminal slash]...
rsav = 'location for saved files'	#e.g., rsav = '/hnd/rsav/'
#rsav = '/prj/hnd/Wave03/znlz/MedHx/Rsave/'
rsav = 'C:/Users/Elias/Documents/work/NIA/2013/HANDLS/Wave03/MedHx/Rsave/'

library(Hmisc)

zNamesRange = function (dat, varBeg, varEnd)
{
    nBeg = which(names(dat) == varBeg)
    nEnd = which(names(dat) == varEnd)
    if (length(nBeg) == 0 | length(nEnd) == 0)
        stop("one or both name(s) not found")
    else return(nBeg:nEnd)
}

isMhx = function(dat) {

# Recode medical history "is" status item & create factor:
#	input: -1=No, 1 = Yes, 7=DK, 8=Refused
#	output: 0=No, 1=Yes, (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat==-1, 0, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:1, labels=c('No','Yes'))
	dat
	}

howOld = function(dat) {
	
	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongNum = function(dat) {
	
	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat=='Dont Know' | dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongUnit = function(dat) {
	
	dat = ifelse(is.na(dat) | dat==0, 9, dat)
	dat = ifelse(dat == 'seconds', 0, dat)
	dat = ifelse(dat == 'minutes', 1, dat)
	dat = ifelse(dat == 'hours', 2, dat)
	dat = ifelse(dat == 'days', 3, dat)
	dat = ifelse(dat == 'weeks', 4, dat)
	dat = ifelse(dat == 'months', 5, dat)
	dat = ifelse(dat == 'years', 6, dat)

	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:6, labels=c('seconds','minutes','hours','days','weeks','months','years'))
	dat
	}

howMany = function(dat) {
	
	dat = ifelse(is.na(dat), 0, dat)
	dat = ifelse(dat=='Dont Know' | dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_alc.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxAlcEver			= isMhx(isEverDrank)
	MedHxAlc5Times			= isMhx(isDrank5)
	MedHxAlcAgeFirst			= howOld(howOldWhenFirstDrank)
	MedHxAlcTimeSinUsedNum		= howLongNum(howLongSinceLastDrinkNum)
	MedHxAlcTimeSinUsedUnit		= howLongUnit(howLongSinceLastDrinkUnit)
	MedHxAlcYouTooMuch		= isMhx(youThinkDrinkTooMuch)
	MedHxAlcFamTooMuch		= isMhx(famThinkDrinkTooMuch)
	MedHxAlcDocTooMuch		= isMhx(docThinkDrinkTooMuch)
	MedHxAlcYouCutDown		= isMhx(youThinkCutDown)
	MedHxAlcCauseProbs		= isMhx(isLifeFallingApart)
	MedHxAlcDrankMore			= isMhx(isEverDrankMoreThanNow)
	MedHxAlcTimeSinDrankMoreNum	= howLongNum(howLongSinceLastDrankThisMuchNum)
	MedHxAlcTimeSinDrankMoreUnit	= howLongUnit(howLongSinceLastDrankThisMuchUnit)
	MedHxAlcLongDrinkMoreNum	= howLongNum(howLongDidYouDrinkThisMuchNum)
	MedHxAlcLongDrinkMoreUnit	= howLongUnit(howLongDidYouDrinkThisMuchUnit)
	MedHxAlcTimeLongAbstainNum	= howLongNum(longestWithoutDrinkNum)
	MedHxAlcTimeLongAbstainUnit	= howLongUnit(longestWithoutDrinkUnit)
	MedHxAlcTimeSinAbstainNum	= howLongNum(timeSinceAbstainNum)
	MedHxAlcTimeSinAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	MedHxAlcTolerance			= isMhx(isTolerance)
	MedHxAlcDrinkAM			= isMhx(isDrinkToCalmDown)
	MedHxAlcForget			= isMhx(isForgotLastNight)
	MedHxAlcDT				= isMhx(isHadDts)
	MedHxAlcEverTreated		= isMhx(isEverBeenTreated)
	MedHxAlcTreatedTimes		= howMany(howManyTimesTreated)
	MedHxAlcTimeSinTreatNum		= howLongNum(timeSinceLastTreatedNum)
	MedHxAlcTimeSinTreatUnit	= howLongUnit(timeSinceLastTreatedUnit)
	MedHxAlcAA				= isMhx(isAA)
	})

mhx = within(mhx, {
	label(MedHxAlcEver)			= 'MedHx-Alc: Since your last visit, have you had a drink of alcohol?'
	label(MedHxAlc5Times)			= 'MedHx-Alc: Have you had more than 5 alcoholic drinks in your life?'
	label(MedHxAlcAgeFirst)			= 'MedHx-Alc: How old were you when you had your first drink?'
	label(MedHxAlcTimeSinUsedNum)		= 'MedHx-Alc: How long (number) has it been since you had a drink?'
	label(MedHxAlcTimeSinUsedUnit)	= 'MedHx-Alc: How long (unit) has it been since you had a drink?'
	label(MedHxAlcYouTooMuch)		= 'MedHx-Alc: Past 6 months, did you ever think that you were drinking too much?'
	label(MedHxAlcFamTooMuch)		= 'MedHx-Alc: Past 6 months, did anyone in your family think that you were drinking too much?'
	label(MedHxAlcDocTooMuch)		= 'MedHx-Alc: Past 6 months, did friends/doctor/other think that you were drinking too much?'
	label(MedHxAlcYouCutDown)		= 'MedHx-Alc: Past 6 months, did you think about cutting down or limiting your drinking?'
	label(MedHxAlcCauseProbs)		= 'MedHx-Alc: Past 6 months, problems in your life because of your drinking?'
	label(MedHxAlcDrankMore)		= 'MedHx-Alc: Was there ever a time when you drank more than you do right now?'
	label(MedHxAlcTimeSinDrankMoreNum)	= 'MedHx-Alc: How long (number) has it been since you drank this much?'
	label(MedHxAlcTimeSinDrankMoreUnit)	= 'MedHx-Alc: How long (unit) has it been since you drank this much?'
	label(MedHxAlcLongDrinkMoreNum)	= 'MedHx-Alc: How long (number) did you drink this much?'
	label(MedHxAlcLongDrinkMoreUnit)	= 'MedHx-Alc: How long (unit) did you drink this much?'
	label(MedHxAlcTimeLongAbstainNum)	= 'MedHx-Alc: What is the longest (number) you have ever gone without a drink?'
	label(MedHxAlcTimeLongAbstainUnit)	= 'MedHx-Alc: What is the longest (unit) you have ever gone without a drink?'
	label(MedHxAlcTimeSinAbstainNum)	= 'MedHx-Alc: How long ago (number) was this?'
	label(MedHxAlcTimeSinAbstainUnit)	= 'MedHx-Alc: How long ago (unit) was this?'
	label(MedHxAlcTolerance)		= 'MedHx-Alc: Have you ever built up a tolerance for alcohol?'
	label(MedHxAlcDrinkAM)			= 'MedHx-Alc: Have you ever had a drink first thing in the morning?'
	label(MedHxAlcForget)			= 'MedHx-Alc: Have you ever awoken after drinking and could not remember night before?'
	label(MedHxAlcDT)				= 'MedHx-Alc: Have you ever had DTs?'
	label(MedHxAlcEverTreated)		= 'MedHx-Alc: Since your last visit, have you been treated for an alcohol problem?'
	label(MedHxAlcTreatedTimes)		= 'MedHx-Alc: How many times have you been treated for alcohol problems?'
	label(MedHxAlcTimeSinTreatNum)	= 'MedHx-Alc: How long (number) has it been since you were last treated?'
	label(MedHxAlcTimeSinTreatUnit)	= 'MedHx-Alc: How long (unit) has it been since you were last treated?'
	label(MedHxAlcAA)				= 'MedHx-Alc: Do you currently go to AA or any other support group for alcohol?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxAlcEver', 'MedHxAlcAA'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
