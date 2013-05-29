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
	dat = ifelse(dat==0, 9, dat)
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

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_heroin.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxOpiateEver				= isMhx(isEverUsed)
	MedHxOpiate5Times				= isMhx(isUsed5)
	MedHxOpiateHeroin				= isMhx(isHeroin)
	MedHxOpiateMethadone			= isMhx(isMeth)
	MedHxOpiateMorphine			= isMhx(isMorph)
	MedHxOpiateCodeine			= isMhx(isCode)
	MedHxOpiateSpeedball			= isMhx(isSpeedball)
	MedHxOpiateShoot				= isMhx(isNeedle)
	MedHxOpiateSmoke				= isMhx(isSmoke)
	MedHxOpiateSnort				= isMhx(isSnort)
	MedHxOpiatePill				= isMhx(isPills)
	MedHxOpiateLiquid				= isMhx(isLiquid)
	MedHxOpiateAgeFirst			= howOld(howOldWhenFirstUsed)
	MedHxOpiateTimeSinceUsedNum		= howLongNum(howLongSinceLastUsedNum)
	MedHxOpiateTimeSinceUsedUnit		= howLongUnit(howLongSinceLastUsedUnit)
	MedHxOpiateEverUsedMore			= isMhx(isEverUsedMoreThanNow)
	MedHxOpiateTimeSinceUsedMoreNum 	= howLongNum(howLongSinceLastUsedThisMuchNum)
	MedHxOpiateTimeSinceUsedMoreUnit	= howLongUnit(howLongSinceLastUsedThisMuchUnit)
	MedHxOpiateTimeUsedMoreNum		= howLongNum(howLongDidYouUseThisMuchNum)
	MedHxOpiateTimeUsedMoreUnit		= howLongUnit(howLongDidYouUseThisMuchUnit)
	MedHxOpiateTimeLongestAbstainNum	= howLongNum(longestWithoutUseNum)
	MedHxOpiateTimeLongestAbstainUnit	= howLongUnit(longestWithoutUseUnit)
	MedHxOpiateTimeSinceAbstainNum	= howLongNum(timeSinceAbstainNum)
	MedHxOpiateTimeSinceAbstainUnit	= howLongUnit(timeSinceAbstainUnit)
	MedHxOpiateTolerance			= isMhx(isTolerance)
	MedHxOpiateOD				= isMhx(isOD)
	MedHxOpiateTreated			= isMhx(isEverBeenTreated)
	MedHxOpiateTreatedTimes			= howMany(howManyTimesTreated)
	MedHxOpiateTimeSinceTreatedNum	= howLongNum(timeSinceLastTreatedNum)
	MedHxOpiateTimeSinceTreatedUnit	= howLongUnit(timeSinceLastTreatedUnit)
	MedHxOpiateMethadoneMaint		= isMhx(isEverOnMethMaint)		
	MedHxOpiateTimeSinceMethNum		= howLongNum(timeSinceLastMethMaintNum)
	MedHxOpiateTimeSinceMethUnit		= howLongUnit(timeSinceLastMethMaintUnit)
	MedHxOpiateTimeTotalMethNum		= howLongNum(totalTimeOnMethMaintNum)
	MedHxOpiateTimeTotalMethUnit		= howLongUnit(totalTimeOnMethMaintUnit)
	MedHxOpiateNA				= isMhx(isNA)
	})

mhx = within(mhx, {
	label(MedHxOpiateEver)				= 'MedHxDrugs: Ever used heroin/morphine/codeine?'
	label(MedHxOpiate5Times)			= 'MedHxDrugs: Used heroin/morphine/codeine more than 5 times?'
	label(MedHxOpiateHeroin)			= 'MedHxDrugs: Used heroin?'
	label(MedHxOpiateMethadone)			= 'MedHxDrugs: Used methadone (not methadone maint.)?'
	label(MedHxOpiateMorphine)			= 'MedHxDrugs: Used morphine?'
	label(MedHxOpiateCodeine)			= 'MedHxDrugs: Used codeine?'
	label(MedHxOpiateSpeedball)			= 'MedHxDrugs: Did speedballs (coke+opiates)?'
	label(MedHxOpiateShoot)				= 'MedHxDrugs: Shot opiates?'
	label(MedHxOpiateSmoke)				= 'MedHxDrugs: Smoked opiates?'
	label(MedHxOpiateSnort)				= 'MedHxDrugs: Snorted opiates?'
	label(MedHxOpiatePill)				= 'MedHxDrugs: Used opiates in pill form?'
	label(MedHxOpiateLiquid)			= 'MedHxDrugs: Used opiates in liquid form?'
	label(MedHxOpiateAgeFirst)			= 'MedHxDrugs: Age first used opiates?'
	label(MedHxOpiateTimeSinceUsedNum)		= 'MedHxDrugs: How long (number) since last used opiates?'
	label(MedHxOpiateTimeSinceUsedUnit)		= 'MedHxDrugs: How long (unit) since last used opiates?'
	label(MedHxOpiateEverUsedMore)		= 'MedHxDrugs: Ever a time when you used more heroin/morphine/codeine than you do now?'
	label(MedHxOpiateTimeSinceUsedMoreNum) 	= 'MedHxDrugs: How long ago (number) were you using this much?'
	label(MedHxOpiateTimeSinceUsedMoreUnit)	= 'MedHxDrugs: How long ago (unit) were you using this much?'
	label(MedHxOpiateTimeUsedMoreNum)		= 'MedHxDrugs: How long (number) did you use this much?'
	label(MedHxOpiateTimeUsedMoreUnit)		= 'MedHxDrugs: How long (unit) did you use this much?'
	label(MedHxOpiateTimeLongestAbstainNum)	= 'MedHxDrugs: Longest time (number) ever went without opiates?'
	label(MedHxOpiateTimeLongestAbstainUnit)	= 'MedHxDrugs: Longest time (unit) ever went without opiates?'
	label(MedHxOpiateTimeSinceAbstainNum)	= 'MedHxDrugs: How long ago (number) was longest period of abstinence from opiates?'
	label(MedHxOpiateTimeSinceAbstainUnit)	= 'MedHxDrugs: How long ago (unit) was longest period of abstinence from opiates?'
	label(MedHxOpiateTolerance)			= 'MedHxDrugs: Ever developed a tolerance to opiates?'
	label(MedHxOpiateOD)				= 'MedHxDrugs: Ever OD on opiates?'
	label(MedHxOpiateTreated)			= 'MedHxDrugs: Ever treated for opiate problems?'
	label(MedHxOpiateTreatedTimes)		= 'MedHxDrugs: How many times treated for opiate problems?'
	label(MedHxOpiateTimeSinceTreatedNum)	= 'MedHxDrugs: How long (number) since last treated for opiate problems?'
	label(MedHxOpiateTimeSinceTreatedUnit)	= 'MedHxDrugs: How long (unit) since last treated for opiate problems?'
	label(MedHxOpiateMethadoneMaint)		= 'MedHxDrugs: Ever on methadone maintenance?'		
	label(MedHxOpiateTimeSinceMethNum)		= 'MedHxDrugs: How long (number) since last on methadone maintenance?'
	label(MedHxOpiateTimeSinceMethUnit)		= 'MedHxDrugs: How long (unit) since last on methadone maintenance?'
	label(MedHxOpiateTimeTotalMethNum)		= 'MedHxDrugs: How long (number) total on methadone maintenance?'
	label(MedHxOpiateTimeTotalMethUnit)		= 'MedHxDrugs: How long (unit) total on methadone maintenance'
	label(MedHxOpiateNA)				= 'MedHxDrugs: Do you currently go to NA or another support group for heroin/morphine/codeine?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxOpiateEver', 'MedHxOpiateNA'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
