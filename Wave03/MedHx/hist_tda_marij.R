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

whichUse = function(dat) {
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat == 'Powder Cocaine', 0, dat)
	dat = ifelse(dat == 'Crack Cocaine', 1, dat)
	dat = ifelse(dat == 'Both', 2, dat)

	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Powder Cocaine','Crack Cocaine','Both'))
	dat
	}

howMany = function(dat) {
	
	dat = ifelse(is.na(dat), 0, dat)
	dat = ifelse(dat=='Dont Know' | dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_marij.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxMarijEver				= isMhx(isEverUsed)
	MedHxMarij5Times				= isMhx(isUsed5)
	MedHxMarijAgeFirst			= howOld(howOldWhenFirstUsed)
	MedHxMarijTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	MedHxMarijTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	MedHxMarijEverUsedMore			= isMhx(isEverUsedMoreThanNow)
	MedHxMarijTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	MedHxMarijTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	MedHxMarijTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	MedHxMarijTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	MedHxMarijTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	MedHxMarijTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	MedHxMarijTimeSinceAbstainNum		= howLongNum(timeSinceAbstainNum)
	MedHxMarijTimeSinceAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	MedHxMarijTolerance			= isMhx(isTolerance)
	MedHxMarijTreated				= isMhx(isEverBeenTreated)
	MedHxMarijTreatedTimes			= howMany(howManyTimesTreated)
	MedHxMarijTimeSinceTreatedNum		= howLongNum(timeSinceLastTreatedNum)
	MedHxMarijTimeSinceTreatedUnit	= howLongUnit(timeSinceLastTreatedUnit)
	MedHxMarijNA				= isMhx(isNA)
	})

mhx = within(mhx, {
	label(MedHxMarijEver)				= 'MedHxDrugs: Ever used marijuana?'
	label(MedHxMarij5Times)				= 'MedHxDrugs: Used marijuana more than 5 times?'
	label(MedHxMarijAgeFirst)			= 'MedHxDrugs: Age first used marijuana?'
	label(MedHxMarijTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used marijuana?'
	label(MedHxMarijTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used marijuana?'
	label(MedHxMarijEverUsedMore)			= 'MedHxDrugs: Ever a time when you used more marijuana than you do now?'
	label(MedHxMarijTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(MedHxMarijTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(MedHxMarijTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(MedHxMarijTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(MedHxMarijTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without marijuana?'
	label(MedHxMarijTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without marijuana?'
	label(MedHxMarijTimeSinceAbstainNum)	= 'MedHxDrugs: How long ago (number) was longest period of abstinence from marijuana?'
	label(MedHxMarijTimeSinceAbstainUnit)	= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from marijuana?'
	label(MedHxMarijTolerance)			= 'MedHxDrugs: Ever developed a tolerance to marijuana?'
	label(MedHxMarijTreated)			= 'MedHxDrugs: Ever treated for marijuana problems?'
	label(MedHxMarijTreatedTimes)			= 'MedHxDrugs: How many times treated for marijuana problems?'
	label(MedHxMarijTimeSinceTreatedNum)	= 'MedHxDrugs: How long (number) since last treated for marijuana problems?'
	label(MedHxMarijTimeSinceTreatedUnit)	= 'MedHxDrugs: How long (unit) since last treated for marijuana problems?'
	label(MedHxMarijNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for marijuana?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxMarijEver', 'MedHxMarijNA'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
