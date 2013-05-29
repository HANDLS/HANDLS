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

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_depend.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxDrugsAnyOthers	= isMhx(isAnyOthers)
	MedHxDrugsYouThinkMuch	= isMhx(isYouThinkTooMuch)
	MedHxDrugsFamThinkMuch	= isMhx(isFamThinkTooMuch)
	MedHxDrugsOthThinkMuch	= isMhx(isDocThinkTooMuch)
	MedHxDrugsCutDown		= isMhx(isCutDown)
	MedHxDrugsRuinLife	= isMhx(isRuiningLife)
	})

mhx = within(mhx, {	
	label(MedHxDrugsAnyOthers)	= 'MedHxDrugs: Past 6 months, are there any other drugs that you have used?'
	label(MedHxDrugsYouThinkMuch) = 'MedHxDrugs: Past 6 months, did you ever think you were using drugs too much?'
	label(MedHxDrugsFamThinkMuch) = 'MedHxDrugs: Past 6 months, did your family ever think you were using drugs too much?'
	label(MedHxDrugsOthThinkMuch)	= 'MedHxDrugs: Past 6 months, did your friends/doctor/other ever think you were using drugs too much?'
	label(MedHxDrugsCutDown)	= 'MedHxDrugs: Past 6 months, did you think about cutting down on your drug use?'
	label(MedHxDrugsRuinLife)	= 'MedHxDrugs: Past 6 months, any problems with life because of drug use?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxDrugsAnyOthers', 'MedHxDrugsRuinLife'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
