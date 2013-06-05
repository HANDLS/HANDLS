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

num = function(dat) {
	
	dat = ifelse(is.na(dat), 0, dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}


dfn = load(url('http://handls.nih.gov/zrdata/hist_review_menstrual.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxPerAge1st  	= num(ageFirstPeriod)
	MedHxNumBirths  	= num(numberOfBirths)
	MedHxNumPreg    	= num(numberOfPreg)
	MedHxMenopAge   	= num(ageMenopause)
	MedHxPerLastYear	= num(lastPeriodYear)
	MedHxPerLastMonth	= num(lastPeriodMonth)
	MedHxPerLastDay	= num(lastPeriodDay)
	})

mhx = within(mhx, {
	label(MedHxPerAge1st)	= 'MedHx-RevSys: Age of first period'
	label(MedHxNumBirths)	= 'MedHx-RevSys: Number of births'
	label(MedHxNumPreg)	= 'MedHx-RevSys: Number of pregnancies'
	label(MedHxMenopAge)	= 'MedHx-RevSys: Age at menopause'
	label(MedHxPerLastYear)	= 'MedHx-RevSys: Year of last period'
	label(MedHxPerLastMonth)= 'MedHx-RevSys: Month of last period'
	label(MedHxPerLastDay)	= 'MedHx-RevSys: Day of last period'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxPerAge1st', 'MedHxPerLastDay'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
