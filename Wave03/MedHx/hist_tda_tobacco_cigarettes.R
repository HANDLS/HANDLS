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

#	input: number=date, (0,NA)=NA
#	output: number=date, NA=NA
	
	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howManyCigsNum = function(dat) {

	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat == 0, NA, dat)
	dat
	}

howManyCigsUnit = function(dat) {

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='cigs', 0, dat)
	dat = ifelse(dat=='packs', 1, dat)
	
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:1, labels=c('cigs','packs'))
	dat
	}

howManyYears = function(dat) {
	
	dat = ifelse(is.na(dat), 0, dat)
	dat = ifelse(dat=='Dont Know', 0, dat)
	dat = ifelse(dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_tobacco_cigarettes.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxCigarettesEver			= isMhx(isEverSmokedBefore)
	MedHxCigarettes100			= isMhx(isSmoked100)
	MedHxCigarettesAgeStart			= howOld(howOldWhenStartedNum)
	MedHxCigarettesNow			= isMhx(isSmokeNow)
	MedHxCigarettesPerDayNowNum		= howManyCigsNum(howManyCigsPerDayNum)
	MedHxCigarettesPerDayNowUnit		= howManyCigsUnit(howManyCigsPerDayUnit)
	MedHxCigarettesYrsThisMuch		= howManyYears(howManyYearsSmoked)
	MedHxCigarettesQuit			= isMhx(isEverQuit)
	MedHxCigarettesQuitHealth		= isMhx(isQuitBecauseHealth)
	MedHxCigarettesYrsOffTotal		= howManyYears(howManyYearsOff)
	MedHxCigarettesAgeLastSmoked		= howOld(howOldWhenLastRegNum)
	MedHxCigarettesPerDayLastRegNum	= howManyCigsNum(howManyCigsPerDayWhenLastRegNum)
	MedHxCigarettesPerDayLastRegUnit	= howManyCigsUnit(howManyCigsPerDayWhenLastRegUnit)
	MedHxCigarettesYrsSmokedLast		= howManyYears(howManyYearsSmokedWhenLastReg)
	MedHxCigarettesTimeSmokedMore		= isMhx(isThereATimeWhenSmokedMore)
	MedHxCigarettesPerDayMostNum		= howManyCigsNum(howManyCigsPerDayWhenSmokedMoreNum)
	MedHxCigarettesPerDayMostUnit		= howManyCigsUnit(howManyCigsPerDayWhenSmokedMoreUnit)
	MedHxCigarettesYrsSmokedMost		= howManyYears(howManyYearsSmokedWhenSmokedMore)
	})

mhx = within(mhx, {
	label(MedHxCigarettesEver)			= 'MedHx-Tobacco-Cigarette: Since your last visit, have you smoked a cigarette?'
	label(MedHxCigarettes100)			= 'MedHx-Tobacco-Cigarette: Have you smoked a total of at least 100 cigarettes (about 5 packs) in your life?'
	label(MedHxCigarettesAgeStart)		= 'MedHx-Tobacco-Cigarette: How old were you when you first started smoking cigarettes fairly regularly?'
	label(MedHxCigarettesNow)			= 'MedHx-Tobacco-Cigarette: Do you smoke cigarettes now?'
	label(MedHxCigarettesPerDayNowNum)		= 'MedHx-Tobacco-Cigarette: How many cigarettes/packs do you smoke per day?'
	label(MedHxCigarettesPerDayNowUnit)		= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(MedHxCigarettesYrsThisMuch)		= 'MedHx-Tobacco-Cigarette: How many years have you smoked this much?'
	label(MedHxCigarettesQuit)			= 'MedHx-Tobacco-Cigarette: Since your last visit, have you quit smoking for a year or longer?'
	label(MedHxCigarettesQuitHealth)		= 'MedHx-Tobacco-Cigarette: Did you quit smoking because you had a health problem that was caused by smoking or made worse by smoking?'
	label(MedHxCigarettesYrsOffTotal)		= 'MedHx-Tobacco-Cigarette: Since you first started smoking, how many years altogether have you stayed off cigarettes?'
	label(MedHxCigarettesAgeLastSmoked)		= 'MedHx-Tobacco-Cigarette: How old were you when you last smoked cigarettes regularly?'
	label(MedHxCigarettesPerDayLastRegNum)	= 'MedHx-Tobacco-Cigarette: How many cigarettes/packs did you smoke per day when you last smoked cigarettes regularly?'
	label(MedHxCigarettesPerDayLastRegUnit)	= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(MedHxCigarettesYrsSmokedLast)		= 'MedHx-Tobacco-Cigarette: How many years did you smoke this much?'
	label(MedHxCigarettesTimeSmokedMore)	= 'MedHx-Tobacco-Cigarette: Was there ever a time when you smoked more than you do now?'
	label(MedHxCigarettesPerDayMostNum)		= 'MedHx-Tobacco-Cigarette: During the time you were smoking the most, about how many cigarettes/packs per day did you usually smoke?'
	label(MedHxCigarettesPerDayMostUnit)	= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(MedHxCigarettesYrsSmokedMost)		= 'MedHx-Tobacco-Cigarette: About how many years did you smoke this much?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxCigarettesEver', 'MedHxCigarettesYrsSmokedMost'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
