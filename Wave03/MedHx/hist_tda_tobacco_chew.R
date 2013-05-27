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

howMany = function(dat) {

	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat == 0, NA, dat)
	dat
	}

whichUse = function(dat) {

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='Chewing Tobacco', 0, dat)
	dat = ifelse(dat=='Snuff', 1, dat)
	dat = ifelse(dat=='Both', 2, dat)
	
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Chewing Tobacco','Snuff','Both'))
	dat
	}

chewSide = function(dat) {
	
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='Left', 0, dat)
	dat = ifelse(dat=='Right', 1, dat)
	dat = ifelse(dat=='Front', 2, dat)
	dat = ifelse(dat=='No Special Place', 3, dat)
	
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:3, labels=c('Left','Right','Front','No Special Place'))
	dat
	}

chewTop = function(dat) {
	
	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat=='Top', 0, dat)
	dat = ifelse(dat=='Bottom', 1, dat)
	dat = ifelse(dat=='No Special Place', 2, dat)
	
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:2, labels=c('Top','Bottom','No Special Place'))
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_tobacco_chew.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxChewEver		= isMhx(isEverChewedBefore)
	MedHxChewAgeStart		= howOld(howOldWhenStartedNum)
	MedHxChewNow		= isMhx(isChewNow)
	MedHxChewSnuffNow		= whichUse(whichDoYouUse)
	MedHxChewPerWeekNow	= howMany(howManyChewPerWeek)
	MedHxChewSide		= chewSide(leftRightOrFront)
	MedHxChewTopBottom	= chewTop(topOrBottom)
	})

mhx = within(mhx, {
	label(MedHxChewEver)		= 'MedHx-Tobacco-Chew: Have you used chewing tobacco/snuff?'
	label(MedHxChewAgeStart)	= 'MedHx-Tobacco-Chew: How old were you when you first started using chewing tobacco/snuff fairly regularly?'
	label(MedHxChewNow)		= 'MedHx-Tobacco-Chew: Do you use chewing tobacco/snuff now?'
	label(MedHxChewSnuffNow)	= 'MedHx-Tobacco-Chew: Which do you use now - chewing tobacco or snuff?'
	label(MedHxChewPerWeekNow)	= 'MedHx-Tobacco-Chew: How many containers of chewing tobacco/snuff do you use per week now?'
	label(MedHxChewSide)		= 'MedHx-Tobacco-Chew: On which side of your mouth do you use chew/snuff?'
	label(MedHxChewTopBottom)	= 'MedHx-Tobacco-Chew: Do you use chew/snuff on the top or bottom of your mouth?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxChewEver', 'MedHxChewTopBottom'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
