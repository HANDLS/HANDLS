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

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_coke.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxCokeEver				= isMhx(isEverUsed)
	MedHxCoke5Times				= isMhx(isUsed5)
	MedHxCokeType				= whichUse(whichDoYouUse)
	MedHxCokeShoot				= isMhx(isNeedle)
	MedHxCokeSmoke				= isMhx(isSmoke)
	MedHxCokeSnort				= isMhx(isSnort)
	MedHxCokeSpeedball			= isMhx(isSpeedball)
	MedHxCokeAgeFirst				= howOld(howOldWhenFirstUsed)
	MedHxCokeTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	MedHxCokeTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	MedHxCokeEverUsedMore			= isMhx(isEverUsedMoreThanNow)
	MedHxCokeTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	MedHxCokeTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	MedHxCokeTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	MedHxCokeTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	MedHxCokeTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	MedHxCokeTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	MedHxCokeTimeSinceAbstainNum		= howLongNum(timeSinceAbstainNum)
	MedHxCokeTimeSinceAbstainUnit		= howLongUnit(timeSinceAbstainUnit)
	MedHxCokeTolerance			= isMhx(isTolerance)
	MedHxCokeOD					= isMhx(isOD)
	MedHxCokeTreated				= isMhx(isEverBeenTreated)
	MedHxCokeTreatedTimes			= howMany(howManyTimesTreated)
	MedHxCokeTimeSinceTreatedNum		= howLongNum(timeSinceLastTreatedNum)
	MedHxCokeTimeSinceTreatedUnit		= howLongUnit(timeSinceLastTreatedUnit)
	MedHxCokeNA					= isMhx(isNA)
	})

mhx = within(mhx, {
	label(MedHxCokeEver)				= 'MedHxDrugs: Ever used cocaine/crack?'
	label(MedHxCoke5Times)				= 'MedHxDrugs: Used cocaine/crack more than 5 times?'
	label(MedHxCokeType)				= 'MedHxDrugs: Do you use powder cocaine or crack?'
	label(MedHxCokeShoot)				= 'MedHxDrugs: Shoot cocaine/crack?'
	label(MedHxCokeSmoke)				= 'MedHxDrugs: Smoke cocaine/crack?'
	label(MedHxCokeSnort)				= 'MedHxDrugs: Snort cocaine/crack?'
	label(MedHxCokeSpeedball)			= 'MedHxDrugs: Did speedballs (coke+opiates)?'
	label(MedHxCokeAgeFirst)			= 'MedHxDrugs: Age first used coke/crack?'
	label(MedHxCokeTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used coke/crack?'
	label(MedHxCokeTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used coke/crack?'
	label(MedHxCokeEverUsedMore)			= 'MedHxDrugs: Ever a time when you used more crack/cocaine than you do now?'
	label(MedHxCokeTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(MedHxCokeTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(MedHxCokeTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(MedHxCokeTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(MedHxCokeTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without coke/crack?'
	label(MedHxCokeTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without coke/crack?'
	label(MedHxCokeTimeSinceAbstainNum)		= 'MedHxDrugs: How long ago (number) was longest period of abstinence from coke/crack?'
	label(MedHxCokeTimeSinceAbstainUnit)	= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from coke/crack?'
	label(MedHxCokeTolerance)			= 'MedHxDrugs: Ever developed a tolerance to coke/crack?'
	label(MedHxCokeOD)				= 'MedHxDrugs: Ever OD on coke/crack?'
	label(MedHxCokeTreated)				= 'MedHxDrugs: Ever treated for coke/crack problems?'
	label(MedHxCokeTreatedTimes)			= 'MedHxDrugs: How many times treated for coke/crack problems?'
	label(MedHxCokeTimeSinceTreatedNum)		= 'MedHxDrugs: How long (number) since last treated for coke/crack problems?'
	label(MedHxCokeTimeSinceTreatedUnit)	= 'MedHxDrugs: How long (unit) since last treated for coke/crack problems?'
	label(MedHxCokeNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for cocaine?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxCokeEver', 'MedHxCokeNA'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
