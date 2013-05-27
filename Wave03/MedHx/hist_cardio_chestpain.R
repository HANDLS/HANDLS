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

dateMhx = function(dat) {

# Recode 0 as NA for date variables
#	input: number=date, 0=NA, NA=NA
#	output: number=date, (0,NA)=NA

	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongMhx = function(dat) {

#	input: 'Seconds', 'Less than 1 minute', '1 - 5 minutes', '6 - 15 minutes', '16 - 30 minutes',
#			'31 minutes - 2 hours', 'More than 2 hours', 7=DK, 8=Refused
#	output: 0='Seconds', 1='Less than 1 minute', 2='1 - 5 minutes', 3='6 - 15 minutes', 4='16 - 30 minutes',
#			5='31 minutes - 2 hours', 6='More than 2 hours', (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='Seconds', 0, dat)
	dat = ifelse(dat=='Less than 1 minute', 1, dat)
	dat = ifelse(dat=='1 - 5 minutes', 2, dat)
	dat = ifelse(dat=='6 - 15 minutes', 3, dat)
	dat = ifelse(dat=='16 - 30 minutes', 4, dat)
	dat = ifelse(dat=='31 minutes - 2 hours', 5, dat)
	dat = ifelse(dat=='More than 2 hours', 6, dat)
	dat = ifelse(dat<7, dat, NA)
	
	dat = factor(dat, levels=0:6, labels=c('Seconds','Less than 1 minute','1 - 5 minutes','6 - 15 minutes',
								'16 - 30 minutes','31 minutes - 2 hours','More than 2 hours'))
	dat
	}

painsGoMhx = function(dat) {

#	input: 'No', 'Sometimes', 'Usually', 'Dont Know'=DK, 7=DK, 8=Refused
#	output: 0='No', 1='Sometimes', 2='Usually', ('Dont Know',7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='No', 0, dat)
	dat = ifelse(dat=='Sometimes', 1, dat)
	dat = ifelse(dat=='Usually', 2, dat)
	dat = ifelse(dat=='Dont Know', 7, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('No','Sometimes','Usually'))
	dat
	}

DoctorSayMhx = function(dat) {

#	input: 'Heart Attack', 'Angina', 'Pain was not from heart', 7=DK, 8=Refused
#	output: 0='Heart Attack', 1='Angina', 2='Pain was not from heart', (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='Heart Attack', 0, dat)
	dat = ifelse(dat=='Angina', 1, dat)
	dat = ifelse(dat=='Pain was not from heart', 2, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Heart Attack','Angina','Pain was not from heart'))
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_cardio_chestpain.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxChestPain		= isMhx(isChestPain)
	MedHxChestPainMo1		= dateMhx(firstEpisodeMonth)
	MedHxChestPainYr1		= dateMhx(firstEpisodeYear)
	MedHxChestPainMoRec	= dateMhx(lastEpisodeMonth)
	MedHxChestPainYrRec	= dateMhx(lastEpisodeYear)
	MedHxChestPainSit		= isMhx(isPainWhenSitting)
	MedHxChestPainWalk	= isMhx(isPainWhenSlowPace)
	MedHxChestPainHill	= isMhx(isPainWhenUphill)
	MedHxChestPainStopWalk	= isMhx(isPainWhenStopWalking)
	MedHxChestPainDuration	= howLongMhx(howLongPainLasts)
	MedHxChestPainPressure	= isMhx(isPressure)
	MedHxChestPainBurn	= isMhx(isBurning)
	MedHxChestPainSqueeze	= isMhx(isSqueezing)
	MedHxChestPainStab	= isMhx(isStabbing)
	MedHxChestPainShoot	= isMhx(isShooting)
	MedHxChestPainRadiate	= painsGoMhx(doPainsGoAnywhere)
	MedHxChestPainShoulderL	= isMhx(isPainGoToLeftShoulder)
	MedHxChestPainBelly	= isMhx(isPainGoToBelly)
	MedHxChestPainNeck	= isMhx(isPainGoToNeck)
	MedHxChestPainBack	= isMhx(isPainGoToBack)
	MedHxChestPainShoulderR	= isMhx(isPainGoToRightShoulder)
	MedHxChestPainMeals	= isMhx(isPainOccurAfterMeals)
	MedHxChestPainNight	= isMhx(isPainOccurAtNight)
	MedHxChestPainExercise	= isMhx(isPainOccurWhenExercising)
	MedHxChestPainCold	= isMhx(isPainOccurInCold)
	MedHxChestPainUpset	= isMhx(isPainOccurWhenUpset)
	MedHxChestPainByEat	= isMhx(isMakePainWithMeal)
	MedHxChestPainByLay	= isMhx(isMakePainLayDown)
	MedHxChestPainByExercise= isMhx(isMakePainExercise)
	MedHxChestPainByHill	= isMhx(isMakePainInCold)
	MedHxChestPainByUpset	= isMhx(isMakePainWhenUpset)
	MedHxChestPainRest	= isMhx(isPainGoAwayRest)
	MedHxChestPainPosition	= isMhx(isPainGoAwayChangePosition)
	MedHxChestPainActivity	= isMhx(isPainGoAwayPhysical)
	MedHxChestPainTums	= isMhx(isPainGoAwayTums)
	MedHxChestPainNitro	= isMhx(isPainGoAwayTNT)
	MedHxChestPainMeds	= isMhx(isPainGoAwayOtherMeds)
	MedHxChestPainHospital	= isMhx(isBeenToHospital)
	MedHxChestPainDocSaid	= DoctorSayMhx(whatDidDoctorSay)
	})

mhx = within(mhx, {
	label(MedHxChestPain)		= 'MedHxCV: Ever had chest pain/pressure (excl. heartburn, colds, breast pain)'
	label(MedHxChestPainMo1)	= 'MedHxCV: First episode chest pain - month'
	label(MedHxChestPainYr1)	= 'MedHxCV: First episode chest pain - year'
	label(MedHxChestPainMoRec)	= 'MedHxCV: Most recent episode chest pain - month'
	label(MedHxChestPainYrRec)	= 'MedHxCV: Most recent episode chest pain - year'
	label(MedHxChestPainSit)	= 'MedHxCV: Chest pain when sitting/standing still'
	label(MedHxChestPainWalk)	= 'MedHxCV: Chest pain when walking slowly on flat surface'
	label(MedHxChestPainHill)	= 'MedHxCV: Chest pain when walking quickly or uphill'
	label(MedHxChestPainStopWalk)	= 'MedHxCV: Do chest pains continue if you stop walking'
	label(MedHxChestPainDuration)	= 'MedHxCV: How long do chest pains last'
	label(MedHxChestPainPressure)	= 'MedHxCV: Chest pain = pressure'
	label(MedHxChestPainBurn)	= 'MedHxCV: Chest pain = burning'
	label(MedHxChestPainSqueeze)	= 'MedHxCV: Chest pain = squeezing'
	label(MedHxChestPainStab)	= 'MedHxCV: Chest pain = stabbing/piercing'
	label(MedHxChestPainShoot)	= 'MedHxCV: Chest pain = shooting'
	label(MedHxChestPainRadiate)	= 'MedHxCV: Does chest pain go anywhere'
	label(MedHxChestPainShoulderL)= 'MedHxCV: Chest pain goes to left shoulder'
	label(MedHxChestPainBelly)	= 'MedHxCV: Chest pain goes to belly'
	label(MedHxChestPainNeck)	= 'MedHxCV: Chest pain goes to neck'
	label(MedHxChestPainBack)	= 'MedHxCV: Chest pain goes to back'
	label(MedHxChestPainShoulderR)= 'MedHxCV: Chest pain goes to right shoulder'
	label(MedHxChestPainMeals)	= 'MedHxCV: Chest pain occurs after meals'
	label(MedHxChestPainNight)	= 'MedHxCV: Chest pain occurs at night'
	label(MedHxChestPainExercise)	= 'MedHxCV: Chest pain occurs when exercising'
	label(MedHxChestPainCold)	= 'MedHxCV: Chest pain occurs in cold, windy weather'
	label(MedHxChestPainUpset)	= 'MedHxCV: Chest pain occurs when upset/excited/nervous'
	label(MedHxChestPainByEat)	= 'MedHxCV: Reproduce chest pain eating a large meal'
	label(MedHxChestPainByLay)	= 'MedHxCV: Reproduce chest pain laying down'
	label(MedHxChestPainByExercise)= 'MedHxCV: Reproduce chest pain vigorous exercise'
	label(MedHxChestPainByHill)	= 'MedHxCV: Reproduce chest pain walking uphill in cold, windy weather'
	label(MedHxChestPainByUpset)	= 'MedHxCV: Reproduce chest pain getting upset/excited'
	label(MedHxChestPainRest)	= 'MedHxCV: Chest pain goes away by resting'
	label(MedHxChestPainPosition)	= 'MedHxCV: Chest pain goes away by changing posture/position'
	label(MedHxChestPainActivity)	= 'MedHxCV: Chest pain goes away with physical activity'
	label(MedHxChestPainTums)	= 'MedHxCV: Chest pain goes away with antacids/Tums/bicarbonate of soda'
	label(MedHxChestPainNitro)	= 'MedHxCV: Chest pain goes away with nitroglycerin'
	label(MedHxChestPainMeds)	= 'MedHxCV: Chest pain goes away with other meds'
	label(MedHxChestPainHospital)	= 'MedHxCV: Went to hospital for chest pain'
	label(MedHxChestPainDocSaid)	= 'MedHxCV: What doctor said about chest pain'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxChestPain', 'MedHxChestPainDocSaid'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
