library(RMySQL)

w00Dem = HNDload()

source('/git/Wave03/MedHx/w03MedHx-functions.R')

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

HNDdb = dbConnect(MySQL(), group='HND')

dbDat = dbGetQuery(HNDdb, 'select * from Wave03.hist_tda_tobacco_cigarettes')
dbIDs = dbGetQuery(HNDdb, 'select cast(handls_ID as char) as HNDid from Wave03.hist_tda_tobacco_cigarettes')

head(dbDat)
head(dbIDs)

mhx = HNDidLegit(cbind(dbIDs, dbDat))
mhx = HNDdupsDelete(mhx)

head(mhx)
str(mhx)

mhx = within(mhx, {
	CigarettesEver			= isMhx(isEverSmokedBefore)
	Cigarettes100			= isMhx(isSmoked100)
	CigarettesAgeStart		= howOld(howOldWhenStartedNum)
	CigarettesNow			= isMhx(isSmokeNow)
	CigarettesPerDayNowNum		= howManyCigsNum(howManyCigsPerDayNum)
	CigarettesPerDayNowUnit		= howManyCigsUnit(howManyCigsPerDayUnit)
	CigarettesYrsThisMuch		= howManyYears(howManyYearsSmoked)
	CigarettesQuit			= isMhx(isEverQuit)
	CigarettesQuitHealth		= isMhx(isQuitBecauseHealth)
	CigarettesYrsOffTotal		= howManyYears(howManyYearsOff)
	CigarettesAgeLastSmoked		= howOld(howOldWhenLastRegNum)
	CigarettesPerDayLastRegNum	= howManyCigsNum(howManyCigsPerDayWhenLastRegNum)
	CigarettesPerDayLastRegUnit	= howManyCigsUnit(howManyCigsPerDayWhenLastRegUnit)
	CigarettesYrsSmokedLast		= howManyYears(howManyYearsSmokedWhenLastReg)
	CigarettesTimeSmokedMore	= isMhx(isThereATimeWhenSmokedMore)
	CigarettesPerDayMostNum		= howManyCigsNum(howManyCigsPerDayWhenSmokedMoreNum)
	CigarettesPerDayMostUnit	= howManyCigsUnit(howManyCigsPerDayWhenSmokedMoreUnit)
	CigarettesYrsSmokedMost		= howManyYears(howManyYearsSmokedWhenSmokedMore)
	})

mhx = within(mhx, {
	label(CigarettesEver)			= 'MedHx-Tobacco-Cigarette: Since your last visit, have you smoked a cigarette?'
	label(Cigarettes100)			= 'MedHx-Tobacco-Cigarette: Have you smoked a total of at least 100 cigarettes (about 5 packs) in your life?'
	label(CigarettesAgeStart)		= 'MedHx-Tobacco-Cigarette: How old were you when you first started smoking cigarettes fairly regularly?'
	label(CigarettesNow)			= 'MedHx-Tobacco-Cigarette: Do you smoke cigarettes now?'
	label(CigarettesPerDayNowNum)		= 'MedHx-Tobacco-Cigarette: How many cigarettes/packs do you smoke per day?'
	label(CigarettesPerDayNowUnit)		= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(CigarettesYrsThisMuch)		= 'MedHx-Tobacco-Cigarette: How many years have you smoked this much?'
	label(CigarettesQuit)			= 'MedHx-Tobacco-Cigarette: Since your last visit, have you quit smoking for a year or longer?'
	label(CigarettesQuitHealth)		= 'MedHx-Tobacco-Cigarette: Did you quit smoking because you had a health problem that was caused by smoking or made worse by smoking?'
	label(CigarettesYrsOffTotal)		= 'MedHx-Tobacco-Cigarette: Since you first started smoking, how many years altogether have you stayed off cigarettes?'
	label(CigarettesAgeLastSmoked)		= 'MedHx-Tobacco-Cigarette: How old were you when you last smoked cigarettes regularly?'
	label(CigarettesPerDayLastRegNum)	= 'MedHx-Tobacco-Cigarette: How many cigarettes/packs did you smoke per day when you last smoked cigarettes regularly?'
	label(CigarettesPerDayLastRegUnit)	= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(CigarettesYrsSmokedLast)		= 'MedHx-Tobacco-Cigarette: How many years did you smoke this much?'
	label(CigarettesTimeSmokedMore)		= 'MedHx-Tobacco-Cigarette: Was there ever a time when you smoked more than you do now?'
	label(CigarettesPerDayMostNum)		= 'MedHx-Tobacco-Cigarette: During the time you were smoking the most, about how many cigarettes/packs per day did you usually smoke?'
	label(CigarettesPerDayMostUnit)		= 'MedHx-Tobacco-Cigarette: Cigarettes/Packs'
	label(CigarettesYrsSmokedMost)		= 'MedHx-Tobacco-Cigarette: About how many years did you smoke this much?'
	})

w03MedHxCig = mhx[,c(1,zNamesRange(mhx, 'CigarettesEver', 'CigarettesYrsSmokedMost'))]

w03MedHxCig[w03MedHxCig$HNDid==8033916701,]

str(w03MedHxCig)
describe(w03MedHxCig)

save(w03MedHxCig, file='/git/Wave03/MedHx/zrdata/w03MedHxCig.rdata')
