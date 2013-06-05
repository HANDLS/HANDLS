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

year = function(dat) {
	
	dat = format(dat, '%Y')
	dat
	}

month = function(dat) {
	
	dat = format(dat, '%m')
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/hist_cardio_disease.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {
	MedHxCVhtn     		= isMhx(isDisease01)
	MedHxCVchf      		= isMhx(isDisease02)
	MedHxCVheartLarge       = isMhx(isDisease03)
	MedHxCVhighChol 		= isMhx(isDisease04)
	MedHxCVangina   		= isMhx(isDisease05)
	MedHxCVmi       		= isMhx(isDisease06)
	MedHxCVcad      		= isMhx(isDisease07)
	MedHxCVmurmur   		= isMhx(isDisease08)
	MedHxCVvalveLeak        = isMhx(isDisease09)
	MedHxCVaFib     		= isMhx(isDisease10)
	MedHxCVheartInfect      = isMhx(isDisease11)
	MedHxCVrheumHeart       = isMhx(isDisease12)
	MedHxCVmitralValve      = isMhx(isDisease13)
	MedHxCVtia      		= isMhx(isDisease14)
	MedHxCVstroke   		= isMhx(isDisease15)
	MedHxCVdvt      		= isMhx(isDisease17)
	MedHxCVaneurysm 		= isMhx(isDisease18)
	MedHxCVhtnYear  		= year(dxDate01)
	MedHxCVchfYear  		= year(dxDate02)
	MedHxCVheartLargeYear   = year(dxDate03)
	MedHxCVhighCholYear     = year(dxDate04)
	MedHxCVanginaYear       = year(dxDate05)
	MedHxCVmiYear   		= year(dxDate06)
	MedHxCVcadYear  		= year(dxDate07)
	MedHxCVmurmurYear       = year(dxDate08)
	MedHxCVvalveLeakYear    = year(dxDate09)
	MedHxCVaFibYear 		= year(dxDate10)
	MedHxCVheartInfectYear  = year(dxDate11)
	MedHxCVrheumHeartYear   = year(dxDate12)
	MedHxCVmitralValveYear  = year(dxDate13)
	MedHxCVtiaYear  		= year(dxDate14)
	MedHxCVstrokeYear       = year(dxDate15)
	MedHxCVdvtYear		= year(dxDate17)
	MedHxCVaneurysmYear     = year(dxDate18)
	MedHxCVhtnMonth  		= month(dxDate01)
	MedHxCVchfMonth  		= month(dxDate02)
	MedHxCVheartLargeMonth  = month(dxDate03)
	MedHxCVhighCholMonth    = month(dxDate04)
	MedHxCVanginaMonth      = month(dxDate05)
	MedHxCVmiMonth   		= month(dxDate06)
	MedHxCVcadMonth  		= month(dxDate07)
	MedHxCVmurmurMonth      = month(dxDate08)
	MedHxCVvalveLeakMonth   = month(dxDate09)
	MedHxCVaFibMonth 		= month(dxDate10)
	MedHxCVheartInfectMonth = month(dxDate11)
	MedHxCVrheumHeartMonth  = month(dxDate12)
	MedHxCVmitralValveMonth = month(dxDate13)
	MedHxCVtiaMonth  		= month(dxDate14)
	MedHxCVstrokeMonth      = month(dxDate15)
	MedHxCVdvtMonth		= month(dxDate17)
	MedHxCVaneurysmMonth    = month(dxDate18)
	})

mhx = within(mhx, {
	label(MedHxCVhtn)               = 'MedHx-CVdis: Hypertension self-report'
	label(MedHxCVchf)               = 'MedHx-CVdis: CHF (congestive heart failure) self-report'
	label(MedHxCVheartLarge)        = 'MedHx-CVdis: Enlarged heart self-report'
	label(MedHxCVhighChol)          = 'MedHx-CVdis: High cholesterol self-report'
	label(MedHxCVangina)            = 'MedHx-CVdis: Angina/chest pain self-report'
	label(MedHxCVmi)                = 'MedHx-CVdis: Heart attack/MI self-report'
	label(MedHxCVcad)               = 'MedHx-CVdis: Coronary artery disease/blockage self-report'
	label(MedHxCVmurmur)            = 'MedHx-CVdis: Heart murmur self-report'
	label(MedHxCVvalveLeak)         = 'MedHx-CVdis: Leaky heart valve self-report'
	label(MedHxCVaFib)              = 'MedHx-CVdis: Atrial fibrillation/irregular heartbeat self-report'
	label(MedHxCVheartInfect)       = 'MedHx-CVdis: Heart infection self-report'
	label(MedHxCVrheumHeart)        = 'MedHx-CVdis: Rheumatic heart disease self-report'
	label(MedHxCVmitralValve)       = 'MedHx-CVdis: Mitral valve prolapse self-report'
	label(MedHxCVtia)               = 'MedHx-CVdis: TIA/mini-stroke self-report'
	label(MedHxCVstroke)            = 'MedHx-CVdis: Stroke/CVA self-report'
	label(MedHxCVdvt)               = 'MedHx-CVdis: DVT/blood clot self-report'
	label(MedHxCVaneurysm)          = 'MedHx-CVdis: Aneurysm self-report'
	label(MedHxCVhtnYear)           = 'MedHx-CVdis: Earliest year hypertension reported'
	label(MedHxCVchfYear)           = 'MedHx-CVdis: Earliest year CHF reported'
	label(MedHxCVheartLargeYear)    = 'MedHx-CVdis: Earliest year enlarged heart reported'
	label(MedHxCVhighCholYear)      = 'MedHx-CVdis: Earliest year high cholesterol reported'
	label(MedHxCVanginaYear)        = 'MedHx-CVdis: Earliest year angina/chest pain reported'
	label(MedHxCVmiYear)            = 'MedHx-CVdis: Earliest year heart attack/MI reported'
	label(MedHxCVcadYear)           = 'MedHx-CVdis: Earliest year coronary artery disease reported'
	label(MedHxCVmurmurYear)        = 'MedHx-CVdis: Earliest year heart murmur reported'
	label(MedHxCVvalveLeakYear)     = 'MedHx-CVdis: Earliest year leaky heart valve reported'
	label(MedHxCVaFibYear)          = 'MedHx-CVdis: Earliest year atrial fibrillation/irregular heartbeat reported'
	label(MedHxCVheartInfectYear)   = 'MedHx-CVdis: Earliest year heart infection reported'
	label(MedHxCVrheumHeartYear)    = 'MedHx-CVdis: Earliest year rheumatic heart disease reported'
	label(MedHxCVmitralValveYear)   = 'MedHx-CVdis: Earliest year mitral valve prolapse reported'
	label(MedHxCVtiaYear)           = 'MedHx-CVdis: Earliest year TIA/mini-stroke reported'
	label(MedHxCVstrokeYear)        = 'MedHx-CVdis: Earliest year stroke/CVA reported'
	label(MedHxCVdvtYear)           = 'MedHx-CVdis: Earliest year DVT/blood clot reported'
	label(MedHxCVaneurysmYear)      = 'MedHx-CVdis: Earliest year aneurysm reported'
	label(MedHxCVhtnMonth)          = 'MedHx-CVdis: Earliest month hypertension reported'
	label(MedHxCVchfMonth)          = 'MedHx-CVdis: Earliest month CHF reported'
	label(MedHxCVheartLargeMonth)   = 'MedHx-CVdis: Earliest month enlarged heart reported'
	label(MedHxCVhighCholMonth)     = 'MedHx-CVdis: Earliest month high cholesterol reported'
	label(MedHxCVanginaMonth)       = 'MedHx-CVdis: Earliest month angina/chest pain reported'
	label(MedHxCVmiMonth)           = 'MedHx-CVdis: Earliest month heart attack/MI reported'
	label(MedHxCVcadMonth)          = 'MedHx-CVdis: Earliest month coronary artery disease reported'
	label(MedHxCVmurmurMonth)       = 'MedHx-CVdis: Earliest month heart murmur reported'
	label(MedHxCVvalveLeakMonth)    = 'MedHx-CVdis: Earliest month leaky heart valve reported'
	label(MedHxCVaFibMonth)         = 'MedHx-CVdis: Earliest month atrial fibrillation/irregular heartbeat reported'
	label(MedHxCVheartInfectMonth)  = 'MedHx-CVdis: Earliest month heart infection reported'
	label(MedHxCVrheumHeartMonth)   = 'MedHx-CVdis: Earliest month rheumatic heart disease reported'
	label(MedHxCVmitralValveMonth)  = 'MedHx-CVdis: Earliest month mitral valve prolapse reported'
	label(MedHxCVtiaMonth)          = 'MedHx-CVdis: Earliest month TIA/mini-stroke reported'
	label(MedHxCVstrokeMonth)       = 'MedHx-CVdis: Earliest month stroke/CVA reported'
	label(MedHxCVdvtMonth)          = 'MedHx-CVdis: Earliest month DVT/blood clot reported'
	label(MedHxCVaneurysmMonth)     = 'MedHx-CVdis: Earliest month aneurysm reported'
	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxCVhtn', 'MedHxCVaneurysmMonth'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))
