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

bothLegsMhx = function(dat) {
#	input: 1=1, 2=2, 7=DK, 8=Refused
#	output: 1=One, 2=Both, (7,8,9)=NA
	
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=1:2, labels=c('One','Both'))
	dat
	}

howFarMhx = function(dat) {

#	input: "Far"=Far, "1 block"=1 block, "2 blocks"=2 blocks, 7=DK, 8=Refused
#	output: 0=Far, 1=1 block, 2=2 blocks, (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=="Far", 0, dat)
	dat = ifelse(dat=="1 block", 1, dat)
	dat = ifelse(dat=="2 blocks", 2, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Far','1 block','2 blocks'))
	dat
	}

DoctorSayMhx = function(dat) {

#	input: "Heart Attack"=Heart Attack, "Another problem"=Another problem, "Not related"=Not related, 7=DK, "Dont Know"=DK
#	output: 0=Heart Attack, 1=Another problem, 2=Not related, (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=="Heart Attack", 0, dat)
	dat = ifelse(dat=="Another problem", 1, dat)
	dat = ifelse(dat=="Not related", 2, dat)
	dat = ifelse(dat=="Dont Know", 7, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Heart Attack','Another problem','Not related'))
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_cardio_heartfailure.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxCHFWalkFar		= howFarMhx(howFarCanWalk)
	MedHxCHFWalkPressure	= isMhx(isPressure)
	MedHxCHFWalkBreath	= isMhx(isShortness)
	MedHxCHFWalkCalves	= isMhx(isPainInCalves)
	MedHxCHFWalkKnees		= isMhx(isPainInKnees)
	MedHxCHFWakeBreath	= isMhx(isWakeUpShortOfBreath)
	MedHxCHFPillows		= isMhx(isNeededPillows)
	MedHxCHFLegsSwell		= isMhx(isLegsSwollen)
	MedHxCHFLegsWhich		= bothLegsMhx(isSwellingInBothLegs)
	MedHxCHFPassOut		= isMhx(isPassOut)
	MedHxPassOutDoctor	= isMhx(isSeeDoctor)
	MedHxPassOutDocSaid	= DoctorSayMhx(whatDidDoctorSay)
	})

mhx = within(mhx, {
	label(MedHxCHFWalkFar)		= 'MedHx-CV: How far can you walk'
	label(MedHxCHFWalkPressure)	= 'MedHx-CV: Stop walking due to chest pain/pressure?'
	label(MedHxCHFWalkBreath)	= 'MedHx-CV: Stop walking due to shortness of breath'
	label(MedHxCHFWalkCalves)	= 'MedHx-CV: Ever stop walking due to pain/cramps in back of calves'
	label(MedHxCHFWalkKnees)	= 'MedHx-CV: Ever stop walking due to pain in knee/hip/back'
	label(MedHxCHFWakeBreath)	= 'MedHx-CV: Wake up in middle of night short of breath'
	label(MedHxCHFPillows)		= 'MedHx-CV: Ever needed 2 or more pillows to avoid breathing problems'
	label(MedHxCHFLegsSwell)	= 'MedHx-CV: Ever had swelling in legs/ankles (excl. pregnancy)'
	label(MedHxCHFLegsWhich)	= 'MedHx-CV: Was swelling in one (1) or both (2) legs'
	label(MedHxCHFPassOut)		= 'MedHx-CV: Ever passed out/blacked out'
	label(MedHxPassOutDoctor)	= 'MedHx-CV: Went to doctor because passed out'
	label(MedHxPassOutDocSaid)	= 'MedHx-CV: What did doctor say about passing out'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxCHFWalkFar', 'MedHxPassOutDocSaid'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
