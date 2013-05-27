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

howLongNum = function(dat) {

#	input: number=date, (0,NA)=NA
#	output: number=date, NA=NA
	
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongUnit = function(dat) {

#	input: 'seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years', ('0',NA)=NA
#	output: 0='seconds', 1='minutes', 2='hours', 3='days', 4='weeks', 5='months', 6='years', NA=NA
	
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='seconds', 0, dat)	
	dat = ifelse(dat=='minutes', 1, dat)
	dat = ifelse(dat=='hours', 2, dat)
	dat = ifelse(dat=='days', 3, dat)
	dat = ifelse(dat=='weeks', 4, dat)
	dat = ifelse(dat=='months', 5, dat)
	dat = ifelse(dat=='years', 6, dat)

	dat = ifelse(dat=='0',NA,dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:6, labels=c('seconds','minutes','hours','days','weeks','months','years'))
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_cardio_vascular.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxPassPainLegs		= isMhx(isPain)
	MedHxPassPainStopWalk	= isMhx(isPainStop)
	MedHxPassNumb		= isMhx(isNumbness)
	MedHxPassNumbDuration	= howLongNum(howLongNumbnessNum)
	MedHxPassNumbUnits	= howLongUnit(howLongNumbnessUnit)
	MedHxPassWeak		= isMhx(isWeakness)
	MedHxPassWeakDuration	= howLongNum(howLongWeaknessNum)
	MedHxPassWeakUnits	= howLongUnit(howLongWeaknessUnit)
	MedHxPassSlur		= isMhx(isSlurring)
	MedHxPassSlurDuration	= howLongNum(howLongSlurringNum)
	MedHxPassSlurUnits	= howLongUnit(howLongSlurringUnit)
	})

mhx = within(mhx, {
	label(MedHxPassPainLegs)	= 'MedHxCV: Ever pain/cramp in back of legs when walk'
	label(MedHxPassPainStopWalk)	= 'MedHxCV: Pain in back of legs goes away if stop walking'
	label(MedHxPassNumb)		= 'MedHxCV: Ever numb/tingling over one side of body'
	label(MedHxPassNumbDuration)	= 'MedHxCV: How long numbness/tingling lasts'
	label(MedHxPassNumbUnits)	= 'MedHxCV: Units for vascNumbDuration'
	label(MedHxPassWeak)		= 'MedHxCV: Ever weak over one side of body'
	label(MedHxPassWeakDuration)	= 'MedHxCV: How long weakness lasts'
	label(MedHxPassWeakUnits)	= 'MedHxCV: Units for vascWeakDuration'
	label(MedHxPassSlur)		= 'MedHxCV: Ever slurred speech'
	label(MedHxPassSlurDuration)	= 'MedHxCV: How long slurring lasts'
	label(MedHxPassSlurUnits)	= 'MedHxCV: Units for vascSlurDuration'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxPassPainLegs', 'MedHxPassSlurUnits'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
