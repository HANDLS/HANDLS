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

#	input: number=date, (0,NA)=NA
#	output: number=date, NA=NA
	
	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

dfn = load(url('http://handls.nih.gov/zrdata/MedHx.rsave'))

str(eval(parse(text=paste0(dfn))))
eval(parse(text=paste0('mhx = ', dfn)))

str(mhx)

mhx = within(mhx, {

MedHxMeningitis                 =  isMhx(MedHxMeningitis)
MedHxTuberculosis               =  isMhx(MedHxTuberculosis)
MedHxSepticemia                 =  isMhx(MedHxSepticemia)
MedHxHerpesZoster               =  isMhx(MedHxHerpesZoster)
MedHxHIVorAIDS                  =  isMhx(MedHxHIVorAIDS)
MedHxGenitalHerpes              =  isMhx(MedHxGenitalHerpes)
MedHxGenitalWarts               =  isMhx(MedHxGenitalWarts)
MedHxSyphilis                   =  isMhx(MedHxSyphilis)
MedHxGonorrhea                  =  isMhx(MedHxGonorrhea)
MedHxChlamydia                  =  isMhx(MedHxChlamydia)
MedHxTrichomoniasis             =  isMhx(MedHxTrichomoniasis)
MedHxAsthma                     =  isMhx(MedHxAsthma)
MedHxChronicBronchitis          =  isMhx(MedHxChronicBronchitis)
MedHxEmphysema                  =  isMhx(MedHxEmphysema)
MedHxRheumatoidArthritis        =  isMhx(MedHxRheumatoidArthritis)
MedHxOsteoarthritis             =  isMhx(MedHxOsteoarthritis)
MedHxGout                       =  isMhx(MedHxGout)
MedHxScoliosis                  =  isMhx(MedHxScoliosis)
MedHxOsteoporosis               =  isMhx(MedHxOsteoporosis)
MedHxTendonitisBursitis         =  isMhx(MedHxTendonitisBursitis)
MedHxBrain                      =  isMhx(MedHxBrain)
MedHxBreast                     =  isMhx(MedHxBreast)
MedHxEsophagus                  =  isMhx(MedHxEsophagus)
MedHxStomach                    =  isMhx(MedHxStomach)
MedHxColon                      =  isMhx(MedHxColon)
MedHxRectum                     =  isMhx(MedHxRectum)
MedHxLiver                      =  isMhx(MedHxLiver)
MedHxSkin                       =  isMhx(MedHxSkin)
MedHxBone                       =  isMhx(MedHxBone)
MedHxMultipleMyeloma            =  isMhx(MedHxMultipleMyeloma)
MedHxLeukemiaLymphoma           =  isMhx(MedHxLeukemiaLymphoma)
MedHxLung                       =  isMhx(MedHxLung)
MedHxLarynx                     =  isMhx(MedHxLarynx)
MedHxMouthOral                  =  isMhx(MedHxMouthOral)
MedHxProstate                   =  isMhx(MedHxProstate)
MedHxTesticular                 =  isMhx(MedHxTesticular)
MedHxCervix                     =  isMhx(MedHxCervix)
MedHxOvary                      =  isMhx(MedHxOvary)
MedHxStomachUlcers              =  isMhx(MedHxStomachUlcers)
MedHxGERD                       =  isMhx(MedHxGERD)
MedHxCrohns                     =  isMhx(MedHxCrohns)
MedHxUlcerativeColitis          =  isMhx(MedHxUlcerativeColitis)
MedHxLiverCirrhosis             =  isMhx(MedHxLiverCirrhosis)
MedHxAlcoholicHepatitis         =  isMhx(MedHxAlcoholicHepatitis)
MedHxPancreatitis               =  isMhx(MedHxPancreatitis)
MedHxKidneyDisease              =  isMhx(MedHxKidneyDisease)
MedHxBPH                        =  isMhx(MedHxBPH)
MedHxOvarianTumors              =  isMhx(MedHxOvarianTumors)
MedHxUterineTumors              =  isMhx(MedHxUterineTumors)
MedHxLupus                      =  isMhx(MedHxLupus)
MedHxSarcoidosis                =  isMhx(MedHxSarcoidosis)
MedHxPsoriasis                  =  isMhx(MedHxPsoriasis)
MedHxAnemia                     =  isMhx(MedHxAnemia)
MedHxThalassemia                =  isMhx(MedHxThalassemia)
MedHxSickleCell                 =  isMhx(MedHxSickleCell)
MedHxAlzheimersDisease          =  isMhx(MedHxAlzheimersDisease)
MedHxDementiaAny                =  isMhx(MedHxDementiaAny)
MedHxPeripheralNeurophathy      =  isMhx(MedHxPeripheralNeurophathy)
MedHxParkinsonDisease           =  isMhx(MedHxParkinsonDisease)
MedHxMultipleSclerosis          =  isMhx(MedHxMultipleSclerosis)
MedHxEpilepsy                   =  isMhx(MedHxEpilepsy)
MedHxMigraine                   =  isMhx(MedHxMigraine)
MedHxHeadaches                  =  isMhx(MedHxHeadaches)
MedHxAnxietyDisorder            =  isMhx(MedHxAnxietyDisorder)
MedHxADD                        =  isMhx(MedHxADD)
MedHxDepression                 =  isMhx(MedHxDepression)
MedHxSuicidality                =  isMhx(MedHxSuicidality)
MedHxBipolarDisorder            =  isMhx(MedHxBipolarDisorder)
MedHxSchizophrenia              =  isMhx(MedHxSchizophrenia)
MedHxFracture                   =  isMhx(MedHxFracture)
MedHxHeadInjury                 =  isMhx(MedHxHeadInjury)
MedHxMotorVehicleAccident       =  isMhx(MedHxMotorVehicleAccident)
MedHxGunShotWound               =  isMhx(MedHxGunShotWound)
MedHxStabWound                  =  isMhx(MedHxStabWound)
MedHxDiabetes                   =  isMhx(MedHxDiabetes)
MedHxDomesticAbuse              =  isMhx(MedHxDomesticAbuse)
MedHxUterus                     =  isMhx(MedHxUterus)
MedHxSpinalStenosis             =  isMhx(MedHxSpinalStenosis)
MedHxSleepApnea                 =  isMhx(MedHxSleepApnea)
MedHxHepatitisA                 =  isMhx(MedHxHepatitisA)
MedHxHepatitisB                 =  isMhx(MedHxHepatitisB)
MedHxHepatitisC                 =  isMhx(MedHxHepatitisC)
MedHxHepatitisD                 =  isMhx(MedHxHepatitisD)
MedHxThyroidHypo                =  isMhx(MedHxThyroidHypo)
MedHxThyroidHyper               =  isMhx(MedHxThyroidHyper)
MedHxBreastMassL                =  isMhx(MedHxBreastMassL)
MedHxBreastMassR                =  isMhx(MedHxBreastMassR)
MedHxMeningitisYear             =  year(MedHxMeningitisYear)
MedHxTuberculosisYear           =  year(MedHxTuberculosisYear)
MedHxSepticemiaYear             =  year(MedHxSepticemiaYear)
MedHxHerpesZosterYear           =  year(MedHxHerpesZosterYear)
MedHxHIVorAIDSYear              =  year(MedHxHIVorAIDSYear)
MedHxGenitalHerpesYear          =  year(MedHxGenitalHerpesYear)
MedHxGenitalWartsYear           =  year(MedHxGenitalWartsYear)
MedHxSyphilisYear               =  year(MedHxSyphilisYear)
MedHxGonorrheaYear              =  year(MedHxGonorrheaYear)
MedHxChlamydiaYear              =  year(MedHxChlamydiaYear)
MedHxTrichomoniasisYear         =  year(MedHxTrichomoniasisYear)
MedHxAsthmaYear                 =  year(MedHxAsthmaYear)
MedHxChronicBronchitisYear      =  year(MedHxChronicBronchitisYear)
MedHxEmphysemaYear              =  year(MedHxEmphysemaYear)
MedHxRheumatoidArthritisYear    =  year(MedHxRheumatoidArthritisYear)
MedHxOsteoarthritisYear         =  year(MedHxOsteoarthritisYear)
MedHxGoutYear                   =  year(MedHxGoutYear)
MedHxScoliosisYear              =  year(MedHxScoliosisYear)
MedHxOsteoporosisYear           =  year(MedHxOsteoporosisYear)
MedHxTendonitisBursitisYear     =  year(MedHxTendonitisBursitisYear)
MedHxBrainYear                  =  year(MedHxBrainYear)
MedHxBreastYear                 =  year(MedHxBreastYear)
MedHxEsophagusYear              =  year(MedHxEsophagusYear)
MedHxStomachYear                =  year(MedHxStomachYear)
MedHxColonYear                  =  year(MedHxColonYear)
MedHxRectumYear                 =  year(MedHxRectumYear)
MedHxLiverYear                  =  year(MedHxLiverYear)
MedHxSkinYear                   =  year(MedHxSkinYear)
MedHxBoneYear                   =  year(MedHxBoneYear)
MedHxMultipleMyelomaYear        =  year(MedHxMultipleMyelomaYear)
MedHxLeukemiaLymphomaYear       =  year(MedHxLeukemiaLymphomaYear)
MedHxLungYear                   =  year(MedHxLungYear)
MedHxLarynxYear                 =  year(MedHxLarynxYear)
MedHxMouthOralYear              =  year(MedHxMouthOralYear)
MedHxProstateYear               =  year(MedHxProstateYear)
MedHxTesticularYear             =  year(MedHxTesticularYear)
MedHxCervixYear                 =  year(MedHxCervixYear)
MedHxOvaryYear                  =  year(MedHxOvaryYear)
MedHxStomachUlcersYear          =  year(MedHxStomachUlcersYear)
MedHxGERDYear                   =  year(MedHxGERDYear)
MedHxCrohnsYear                 =  year(MedHxCrohnsYear)
MedHxUlcerativeColitisYear      =  year(MedHxUlcerativeColitisYear)
MedHxLiverCirrhosisYear         =  year(MedHxLiverCirrhosisYear)
MedHxAlcoholicHepatitisYear     =  year(MedHxAlcoholicHepatitisYear)
MedHxPancreatitisYear           =  year(MedHxPancreatitisYear)
MedHxKidneyDiseaseYear          =  year(MedHxKidneyDiseaseYear)
MedHxBPHYear                    =  year(MedHxBPHYear)
MedHxOvarianTumorsYear          =  year(MedHxOvarianTumorsYear)
MedHxUterineTumorsYear          =  year(MedHxUterineTumorsYear)
MedHxLupusYear                  =  year(MedHxLupusYear)
MedHxSarcoidosisYear            =  year(MedHxSarcoidosisYear)
MedHxPsoriasisYear              =  year(MedHxPsoriasisYear)
MedHxAnemiaYear                 =  year(MedHxAnemiaYear)
MedHxThalassemiaYear            =  year(MedHxThalassemiaYear)
MedHxSickleCellYear             =  year(MedHxSickleCellYear)
MedHxAlzheimersDiseaseYear      =  year(MedHxAlzheimersDiseaseYear)
MedHxDementiaAnyYear            =  year(MedHxDementiaAnyYear)
MedHxPeripheralNeurophathyYear  =  year(MedHxPeripheralNeurophathyYear)
MedHxParkinsonDiseaseYear       =  year(MedHxParkinsonDiseaseYear)
MedHxMultipleSclerosisYear      =  year(MedHxMultipleSclerosisYear)
MedHxEpilepsyYear               =  year(MedHxEpilepsyYear)
MedHxMigraineYear               =  year(MedHxMigraineYear)
MedHxHeadachesYear              =  year(MedHxHeadachesYear)
MedHxAnxietyDisorderYear        =  year(MedHxAnxietyDisorderYear)
MedHxADDYear                    =  year(MedHxADDYear)
MedHxDepressionYear             =  year(MedHxDepressionYear)
MedHxSuicidalityYear            =  year(MedHxSuicidalityYear)
MedHxBipolarDisorderYear        =  year(MedHxBipolarDisorderYear)
MedHxSchizophreniaYear          =  year(MedHxSchizophreniaYear)
MedHxFractureYear               =  year(MedHxFractureYear)
MedHxHeadInjuryYear             =  year(MedHxHeadInjuryYear)
MedHxMotorVehicleAccidentYear   =  year(MedHxMotorVehicleAccidentYear)
MedHxGunShotWoundYear           =  year(MedHxGunShotWoundYear)
MedHxStabWoundYear              =  year(MedHxStabWoundYear)
MedHxDiabetesYear               =  year(MedHxDiabetesYear)
MedHxDomesticAbuseYear          =  year(MedHxDomesticAbuseYear)
MedHxUterusYear                 =  year(MedHxUterusYear)
MedHxSpinalStenosisYear         =  year(MedHxSpinalStenosisYear)
MedHxSleepApneaYear             =  year(MedHxSleepApneaYear)
MedHxHepatitisAYear             =  year(MedHxHepatitisAYear)
MedHxHepatitisBYear             =  year(MedHxHepatitisBYear)
MedHxHepatitisCYear             =  year(MedHxHepatitisCYear)
MedHxHepatitisDYear             =  year(MedHxHepatitisDYear)
MedHxThyroidHypoYear            =  year(MedHxThyroidHypoYear)
MedHxThyroidHyperYear           =  year(MedHxThyroidHyperYear)
MedHxBreastMassLYear            =  year(MedHxBreastMassLYear)
MedHxBreastMassRYear            =  year(MedHxBreastMassRYear)

MedHxFibromyalgia               =  isMhx(MedHxFibromyalgia)
MedHxProstatectomy              =  isMhx(MedHxProstatectomy)
MedHxTubalLigation              =  isMhx(MedHxTubalLigation)
MedHxHysterectomy               =  isMhx(MedHxHysterectomy)
MedHxFibromyalgiaYear           =  year(MedHxFibromyalgiaYear)
MedHxProstatectomyYear          =  year(MedHxProstatectomyYear)
MedHxTubalLigationYear          =  year(MedHxTubalLigationYear)
MedHxHysterectomyYear           =  year(MedHxHysterectomyYear)

	})

mhx = within(mhx, {

label(MedHxMeningitis)                  = 'MedHx-Diseases: Meningitis'
label(MedHxTuberculosis)                = 'MedHx-Diseases: Tuberculosis / Positive PPD'
label(MedHxSepticemia)                  = 'MedHx-Diseases: Septicemia (Blood Infection)'
label(MedHxHerpesZoster)                = 'MedHx-Diseases: Herpes Zoster (Shingles)'
label(MedHxHIVorAIDS)                   = 'MedHx-Diseases: HIV / AIDS'
label(MedHxGenitalHerpes)               = 'MedHx-Diseases: Genital Herpes (Genital Blisters)'
label(MedHxGenitalWarts)                = 'MedHx-Diseases: Genital Warts / HPV Infection'
label(MedHxSyphilis)                    = 'MedHx-Diseases: Syphilis'
label(MedHxGonorrhea)                   = 'MedHx-Diseases: Gonorrhea'
label(MedHxChlamydia)                   = 'MedHx-Diseases: Chlamydia'
label(MedHxTrichomoniasis)              = 'MedHx-Diseases: Trichomoniasis'
label(MedHxAsthma)                      = 'MedHx-Diseases: Asthma'
label(MedHxChronicBronchitis)           = 'MedHx-Diseases: Chronic Bronchitis'
label(MedHxEmphysema)                   = 'MedHx-Diseases: Emphysema'
label(MedHxRheumatoidArthritis)         = 'MedHx-Diseases: Rheumatoid Arthritis'
label(MedHxOsteoarthritis)              = 'MedHx-Diseases: Osteoarthritis'
label(MedHxGout)                        = 'MedHx-Diseases: Gout'
label(MedHxScoliosis)                   = 'MedHx-Diseases: Scoliosis'
label(MedHxOsteoporosis)                = 'MedHx-Diseases: Osteoporosis'
label(MedHxTendonitisBursitis)          = 'MedHx-Diseases: Tendonitis / Bursitis'
label(MedHxBrain)                       = 'MedHx-Diseases: Brain cancer'
label(MedHxBreast)                      = 'MedHx-Diseases: Breast'
label(MedHxEsophagus)                   = 'MedHx-Diseases: Esophagus'
label(MedHxStomach)                     = 'MedHx-Diseases: Stomach'
label(MedHxColon)                       = 'MedHx-Diseases: Colon'
label(MedHxRectum)                      = 'MedHx-Diseases: Rectum'
label(MedHxLiver)                       = 'MedHx-Diseases: Liver'
label(MedHxSkin)                        = 'MedHx-Diseases: Skin'
label(MedHxBone)                        = 'MedHx-Diseases: Bone'
label(MedHxMultipleMyeloma)             = 'MedHx-Diseases: Multiple Myeloma'
label(MedHxLeukemiaLymphoma)            = 'MedHx-Diseases: Leukemia / Lymphoma'
label(MedHxLung)                        = 'MedHx-Diseases: Lung'
label(MedHxLarynx)                      = 'MedHx-Diseases: Larynx'
label(MedHxMouthOral)                   = 'MedHx-Diseases: Mouth / Oral'
label(MedHxProstate)                    = 'MedHx-Diseases: MALES - Prostate'
label(MedHxTesticular)                  = 'MedHx-Diseases: MALES - Testicular'
label(MedHxCervix)                      = 'MedHx-Diseases: FEMALES - Cervix'
label(MedHxOvary)                       = 'MedHx-Diseases: FEMALES - Ovary'
label(MedHxStomachUlcers)               = 'MedHx-Diseases: Stomach Ulcers'
label(MedHxGERD)                        = 'MedHx-Diseases: Reflux Disease (GERD)'
label(MedHxCrohns)                      = 'MedHx-Diseases: Crohn-s Disease'
label(MedHxUlcerativeColitis)           = 'MedHx-Diseases: Ulcerative Colitis'
label(MedHxLiverCirrhosis)              = 'MedHx-Diseases: Liver Cirrhosis'
label(MedHxAlcoholicHepatitis)          = 'MedHx-Diseases: Alcoholic Hepatitis'
label(MedHxPancreatitis)                = 'MedHx-Diseases: Pancreatitis'
label(MedHxKidneyDisease)               = 'MedHx-Diseases: Kidney Disease'
label(MedHxBPH)				    = 'MedHx-Diseases: MALES - Enlarged Prostate (BPH)'
label(MedHxOvarianTumors)               = 'MedHx-Diseases: FEMALES - Ovarian Tumors'
label(MedHxUterineTumors)               = 'MedHx-Diseases: FEMALES - Uterine Tumors'
label(MedHxLupus)                       = 'MedHx-Diseases: Lupus'
label(MedHxSarcoidosis)                 = 'MedHx-Diseases: Sarcoidosis'
label(MedHxPsoriasis)                   = 'MedHx-Diseases: Psoriasis'
label(MedHxAnemia)                      = 'MedHx-Diseases: Anemia'
label(MedHxThalassemia)                 = 'MedHx-Diseases: Thalassemia'
label(MedHxSickleCell)                  = 'MedHx-Diseases: Sickle Cell Disease'
label(MedHxAlzheimersDisease)           = 'MedHx-Diseases: Alzheimer-s Disease'
label(MedHxDementiaAny)                 = 'MedHx-Diseases: Dementia (e.g. AIDS senile etc.)'
label(MedHxPeripheralNeurophathy)       = 'MedHx-Diseases: Peripheral Neurophathy'
label(MedHxParkinsonDisease)            = 'MedHx-Diseases: Parkinson-s Disease'
label(MedHxMultipleSclerosis)           = 'MedHx-Diseases: Multiple Sclerosis'
label(MedHxEpilepsy)                    = 'MedHx-Diseases: Epilepsy (Seizure Disorder)'
label(MedHxMigraine)                    = 'MedHx-Diseases: Migraine'
label(MedHxHeadaches)                   = 'MedHx-Diseases: Frequent or Severe Headaches'
label(MedHxAnxietyDisorder)             = 'MedHx-Diseases: Anxiety Disorder / Panic Disorder'
label(MedHxADD)                 	    = 'MedHx-Diseases: Attention Deficit Disorder / ADHD'
label(MedHxDepression)                  = 'MedHx-Diseases: Depression'
label(MedHxSuicidality)                 = 'MedHx-Diseases: Suicidal Thoughts / Attempts'
label(MedHxBipolarDisorder)             = 'MedHx-Diseases: Bipolar Disorder'
label(MedHxSchizophrenia)               = 'MedHx-Diseases: Schizophrenia'
label(MedHxFracture)                    = 'MedHx-Diseases: Fracture'
label(MedHxHeadInjury)                  = 'MedHx-Diseases: Head Injury'
label(MedHxMotorVehicleAccident)        = 'MedHx-Diseases: Motor Vehicle Accident'
label(MedHxGunShotWound)                = 'MedHx-Diseases: Gun Shot Wound'
label(MedHxStabWound)                   = 'MedHx-Diseases: Stab Wound'
label(MedHxDiabetes)                    = 'MedHx-Diseases: Diabetes (sugar)'
label(MedHxDomesticAbuse)               = 'MedHx-Diseases: Domestic Abuse'
label(MedHxUterus)                      = 'MedHx-Diseases: FEMALES - Uterus'
label(MedHxSpinalStenosis)              = 'MedHx-Diseases: Spinal Stenosis'
label(MedHxSleepApnea)                  = 'MedHx-Diseases: Sleep Apnea'
label(MedHxHepatitisA)                  = 'MedHx-Diseases: Hepatitis A'
label(MedHxHepatitisB)                  = 'MedHx-Diseases: Hepatitis B'
label(MedHxHepatitisC)                  = 'MedHx-Diseases: Hepatitis C'
label(MedHxHepatitisD)                  = 'MedHx-Diseases: Hepatitis D'
label(MedHxThyroidHypo)                 = 'MedHx-Diseases: Thyroid disorder hypoactive'
label(MedHxThyroidHyper)                = 'MedHx-Diseases: Thyroid disorder hyperactive'
label(MedHxBreastMassL)                 = 'MedHx-Diseases: Breast mass left'
label(MedHxBreastMassR)                 = 'MedHx-Diseases: Breast mass right'
label(MedHxMeningitisYear)              = 'MedHx-Diseases: Meningitis (year 1st event)'
label(MedHxTuberculosisYear)            = 'MedHx-Diseases: Tuberculosis / Positive PPD (year 1st event)'
label(MedHxSepticemiaYear)              = 'MedHx-Diseases: Septicemia (Blood Infection) (year 1st event)'
label(MedHxHerpesZosterYear)            = 'MedHx-Diseases: Herpes Zoster (Shingles) (year 1st event)'
label(MedHxHIVorAIDSYear)               = 'MedHx-Diseases: HIV / AIDS (year 1st event)'
label(MedHxGenitalHerpesYear)           = 'MedHx-Diseases: Genital Herpes (Genital Blisters) (year 1st event)'
label(MedHxGenitalWartsYear)            = 'MedHx-Diseases: Genital Warts / HPV Infection (year 1st event)'
label(MedHxSyphilisYear)                = 'MedHx-Diseases: Syphilis (year 1st event)'
label(MedHxGonorrheaYear)               = 'MedHx-Diseases: Gonorrhea (year 1st event)'
label(MedHxChlamydiaYear)               = 'MedHx-Diseases: Chlamydia (year 1st event)'
label(MedHxTrichomoniasisYear)          = 'MedHx-Diseases: Trichomoniasis (year 1st event)'
label(MedHxAsthmaYear)                  = 'MedHx-Diseases: Asthma (year 1st event)'
label(MedHxChronicBronchitisYear)       = 'MedHx-Diseases: Chronic Bronchitis (year 1st event)'
label(MedHxEmphysemaYear)               = 'MedHx-Diseases: Emphysema (year 1st event)'
label(MedHxRheumatoidArthritisYear)     = 'MedHx-Diseases: Rheumatoid Arthritis (year 1st event)'
label(MedHxOsteoarthritisYear)          = 'MedHx-Diseases: Osteoarthritis (year 1st event)'
label(MedHxGoutYear)                    = 'MedHx-Diseases: Gout (year 1st event)'
label(MedHxScoliosisYear)               = 'MedHx-Diseases: Scoliosis (year 1st event)'
label(MedHxOsteoporosisYear)            = 'MedHx-Diseases: Osteoporosis (year 1st event)'
label(MedHxTendonitisBursitisYear)      = 'MedHx-Diseases: Tendonitis / Bursitis (year 1st event)'
label(MedHxBrainYear)                   = 'MedHx-Diseases: Brain cancer (year 1st event)'
label(MedHxBreastYear)                  = 'MedHx-Diseases: Breast (year 1st event)'
label(MedHxEsophagusYear)               = 'MedHx-Diseases: Esophagus (year 1st event)'
label(MedHxStomachYear)                 = 'MedHx-Diseases: Stomach (year 1st event)'
label(MedHxColonYear)                   = 'MedHx-Diseases: Colon (year 1st event)'
label(MedHxRectumYear)                  = 'MedHx-Diseases: Rectum (year 1st event)'
label(MedHxLiverYear)                   = 'MedHx-Diseases: Liver (year 1st event)'
label(MedHxSkinYear)                    = 'MedHx-Diseases: Skin (year 1st event)'
label(MedHxBoneYear)                    = 'MedHx-Diseases: Bone (year 1st event)'
label(MedHxMultipleMyelomaYear)         = 'MedHx-Diseases: Multiple Myeloma (year 1st event)'
label(MedHxLeukemiaLymphomaYear)        = 'MedHx-Diseases: Leukemia / Lymphoma (year 1st event)'
label(MedHxLungYear)                    = 'MedHx-Diseases: Lung (year 1st event)'
label(MedHxLarynxYear)                  = 'MedHx-Diseases: Larynx (year 1st event)'
label(MedHxMouthOralYear)               = 'MedHx-Diseases: Mouth / Oral (year 1st event)'
label(MedHxProstateYear)                = 'MedHx-Diseases: MALES - Prostate (year 1st event)'
label(MedHxTesticularYear)              = 'MedHx-Diseases: MALES - Testicular (year 1st event)'
label(MedHxCervixYear)                  = 'MedHx-Diseases: FEMALES - Cervix (year 1st event)'
label(MedHxOvaryYear)                   = 'MedHx-Diseases: FEMALES - Ovary (year 1st event)'
label(MedHxStomachUlcersYear)           = 'MedHx-Diseases: Stomach Ulcers (year 1st event)'
label(MedHxGERDYear)                    = 'MedHx-Diseases: Reflux Disease (GERD) (year 1st event)'
label(MedHxCrohnsYear)                  = 'MedHx-Diseases: Crohn-s Disease (year 1st event)'
label(MedHxUlcerativeColitisYear)       = 'MedHx-Diseases: Ulcerative Colitis (year 1st event)'
label(MedHxLiverCirrhosisYear)          = 'MedHx-Diseases: Liver Cirrhosis (year 1st event)'
label(MedHxAlcoholicHepatitisYear)      = 'MedHx-Diseases: Alcoholic Hepatitis (year 1st event)'
label(MedHxPancreatitisYear)            = 'MedHx-Diseases: Pancreatitis (year 1st event)'
label(MedHxKidneyDiseaseYear)           = 'MedHx-Diseases: Kidney Disease (year 1st event)'
label(MedHxBPHYear)                     = 'MedHx-Diseases: MALES - Enlarged Prostate (BPH) (year 1st event)'
label(MedHxOvarianTumorsYear)           = 'MedHx-Diseases: FEMALES - Ovarian Tumors (year 1st event)'
label(MedHxUterineTumorsYear)           = 'MedHx-Diseases: FEMALES - Uterine Tumors (year 1st event)'
label(MedHxLupusYear)                   = 'MedHx-Diseases: Lupus (year 1st event)'
label(MedHxSarcoidosisYear)             = 'MedHx-Diseases: Sarcoidosis (year 1st event)'
label(MedHxPsoriasisYear)               = 'MedHx-Diseases: Psoriasis (year 1st event)'
label(MedHxAnemiaYear)                  = 'MedHx-Diseases: Anemia (year 1st event)'
label(MedHxThalassemiaYear)             = 'MedHx-Diseases: Thalassemia (year 1st event)'
label(MedHxSickleCellYear)              = 'MedHx-Diseases: Sickle Cell Disease (year 1st event)'
label(MedHxAlzheimersDiseaseYear)       = 'MedHx-Diseases: Alzheimer-s Disease (year 1st event)'
label(MedHxDementiaAnyYear)             = 'MedHx-Diseases: Dementia (e.g. AIDS senile etc.) (year 1st event)'
label(MedHxPeripheralNeurophathyYear)   = 'MedHx-Diseases: Peripheral Neurophathy (year 1st event)'
label(MedHxParkinsonDiseaseYear)        = 'MedHx-Diseases: Parkinson-s Disease (year 1st event)'
label(MedHxMultipleSclerosisYear)       = 'MedHx-Diseases: Multiple Sclerosis (year 1st event)'
label(MedHxEpilepsyYear)                = 'MedHx-Diseases: Epilepsy (Seizure Disorder) (year 1st event)'
label(MedHxMigraineYear)                = 'MedHx-Diseases: Migraine (year 1st event)'
label(MedHxHeadachesYear)               = 'MedHx-Diseases: Frequent or Severe Headaches (year 1st event)'
label(MedHxAnxietyDisorderYear)         = 'MedHx-Diseases: Anxiety Disorder / Panic Disorder (year 1st event)'
label(MedHxADDYear)                     = 'MedHx-Diseases: Attention Deficit Disorder / ADHD (year 1st event)'
label(MedHxDepressionYear)              = 'MedHx-Diseases: Depression (year 1st event)'
label(MedHxSuicidalityYear)             = 'MedHx-Diseases: Suicidal Thoughts / Attempts (year 1st event)'
label(MedHxBipolarDisorderYear)         = 'MedHx-Diseases: Bipolar Disorder (year 1st event)'
label(MedHxSchizophreniaYear)           = 'MedHx-Diseases: Schizophrenia (year 1st event)'
label(MedHxFractureYear)                = 'MedHx-Diseases: Fracture (year 1st event)'
label(MedHxHeadInjuryYear)              = 'MedHx-Diseases: Head Injury (year 1st event)'
label(MedHxMotorVehicleAccidentYear)    = 'MedHx-Diseases: Motor Vehicle Accident (year 1st event)'
label(MedHxGunShotWoundYear)            = 'MedHx-Diseases: Gun Shot Wound (year 1st event)'
label(MedHxStabWoundYear)               = 'MedHx-Diseases: Stab Wound (year 1st event)'
label(MedHxDiabetesYear)                = 'MedHx-Diseases: Diabetes (sugar) (year 1st event)'
label(MedHxDomesticAbuseYear)           = 'MedHx-Diseases: Domestic Abuse (year 1st event)'
label(MedHxUterusYear)                  = 'MedHx-Diseases: FEMALES - Uterus (year 1st event)'
label(MedHxSpinalStenosisYear)          = 'MedHx-Diseases: Spinal Stenosis (year 1st event)'
label(MedHxSleepApneaYear)              = 'MedHx-Diseases: Sleep Apnea (year 1st event)'
label(MedHxHepatitisAYear)              = 'MedHx-Diseases: Hepatitis A (year 1st event)'
label(MedHxHepatitisBYear)              = 'MedHx-Diseases: Hepatitis B (year 1st event)'
label(MedHxHepatitisCYear)              = 'MedHx-Diseases: Hepatitis C (year 1st event)'
label(MedHxHepatitisDYear)              = 'MedHx-Diseases: Hepatitis D (year 1st event)'
label(MedHxThyroidHypoYear)             = 'MedHx-Diseases: Thyroid disorder hypoactive (year 1st event)'
label(MedHxThyroidHyperYear)            = 'MedHx-Diseases: Thyroid disorder hyperactive (year 1st event)'
label(MedHxBreastMassLYear)             = 'MedHx-Diseases: Breast mass left (year 1st event)'
label(MedHxBreastMassRYear)             = 'MedHx-Diseases: Breast mass right (year 1st event)'

label(MedHxFibromyalgia)                = 'MedHx-Diseases: Fibromyalgia'
label(MedHxProstatectomy)               = 'MedHx-Diseases: Prostatectomy'
label(MedHxTubalLigation)               = 'MedHx-Diseases: TubalLigation'
label(MedHxHysterectomy)                = 'MedHx-Diseases: Hysterectomy'
label(MedHxFibromyalgiaYear)            = 'MedHx-Diseases: Fibromyalgia (year 1st event)'
label(MedHxProstatectomyYear)           = 'MedHx-Diseases: Prostatectomy (year 1st event)'
label(MedHxTubalLigationYear)           = 'MedHx-Diseases: TubalLigation (year 1st event)'
label(MedHxHysterectomyYear)            = 'MedHx-Diseases: Hysterectomy (year 1st event)'

	})

mhx = mhx[,c(1,zNamesRange(mhx, 'MedHxMeningitis', 'MedHxHysterectomyYear'))]

describe(mhx)

eval(parse(text=paste0(dfn, '=mhx')))
eval(parse(text=paste0("save(", dfn, ", file='", rsav, dfn, ".rdata')")))