# ***folder name for saved files [include terminal slash]...
rsav = 'location for saved files'	#e.g., rsav = '/hnd/rsav/'

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

dfn = load(url('http://handls.nih.gov/zrdata/hist_allergy.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxAllergyDrugs	= isMhx(isHadReaction)
	MedHxAllergySeasonal	= isMhx(isSeasonal)
	MedHxAllergyHayFever	= isMhx(isHayFever)
	MedHxAllergySkin	= isMhx(isUrticaria)
	})

mhx = within(mhx, {
	label(MedHxAllergyDrugs)	= 'MedHx-Allergy: Ever had a reaction to a drug or medication'
	label(MedHxAllergyHayFever)	= 'MedHx-Allergy: Do you have hay fever'
	label(MedHxAllergySeasonal)	= 'MedHx-Allergy: Do you have seasonal allergies (grass, pollen, ragweed, etc)'
	label(MedHxAllergySkin)		= 'MedHx-Allergy: Do you have allergic skin reactions (urticaria)'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxAllergyDrugs', 'MedHxAllergySkin'))]

describe(mhx)

#*********************** rename dataframe so all the saved files don't have same name

eval(parse(text=paste0("save(mhx, file='", rsav, "')")))