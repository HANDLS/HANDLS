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

howMany = function(dat) {

	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat == 0, NA, dat)
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_tda_tobacco_cigar.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxCigar20	= isMhx(isSmoked20)
	MedHxCigarsNow	= isMhx(isSmokeNow)
	MedHxCigarsPerDay = howMany(howManySmokePerDay)
	MedHxPipe20		= isMhx(isSmoked20Pipes)
	MedHxPipesNow	= isMhx(isSmokePipesNow)
	MedHxPipesPerDay	= howMany(howManyPipesSmokePerDay)
	})

mhx = within(mhx, {
	label(MedHxCigar20)	= 'MedHx-Tobacco-Cigar: Have you smoked at least 20 cigars in your entire life?'
	label(MedHxCigarsNow)	= 'MedHx-Tobacco-Cigar: Do you smoke cigars now?'
	label(MedHxCigarsPerDay)= 'MedHx-Tobacco-Cigar: How many cigars do you smoke per day?'
	label(MedHxPipe20)	= 'MedHx-Tobacco-Pipe: Have you smoked at least 20 pipes of tobacco in your entire life?'
	label(MedHxPipesNow)	= 'MedHx-Tobacco-Pipe: Do you smoke pipes now?'
	label(MedHxPipesPerDay)	= 'MedHx-Tobacco-Pipe: How many pipes do you smoke per day?'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxCigar20', 'MedHxPipesPerDay'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
