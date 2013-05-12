library(RMySQL)

HNDdb = dbConnect(MySQL(), group='HND')

# -1=No, 1 = Yes, 7=DK, 8=Refused

xferMhx = function(dat) {

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat==-1, 0, dat)
	}

# Cigarettes:

sqlSmoke = 'select handls_ID, isEverSmokedBefore, isSmoked100, isSmokeNow, isEverQuit from Wave03.hist_tda_tobacco_cigarettes'
smoke = dbGetQuery(HNDdb, sqlSmoke)

with(smoke, table(isEverSmokedBefore))
with(smoke, table(isSmoked100))
with(smoke, table(isSmokeNow))
with(smoke, table(isEverQuit))

smoke = within(smoke, {
	HNDid = as.numeric(handls_ID)
	HNDwave = 3
	cigSmokeBef  = factor(xferMhx(isEverSmokedBefore), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	cigEverSmoke = factor(xferMhx(isSmoked100),        levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	cigSmokeNow  = factor(xferMhx(isSmokeNow),         levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	cigQuitSmoke = factor(xferMhx(isEverQuit),         levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	label(cigSmokeBef) =  'Since your last visit, have you smoked a cigarette?'
	label(cigEverSmoke) = 'Have you smoked a total of at least 100 cigarettes (about 5 packs) in your life?'
	label(cigSmokeNow) =  'Do you smoke cigarettes now?'
	label(cigQuitSmoke) = 'Since your last visit, have you quit smoking for a year or longer?'
	})

smoke = smoke[,zQ(HNDid, cigSmokeBef, cigEverSmoke, cigSmokeNow, cigQuitSmoke)]

describe(smoke)

# Alcohol:

sqlEtOH = 'select handls_ID, isEverDrank, isDrank5 from Wave03.hist_tda_alc'
etoh = dbGetQuery(HNDdb, sqlEtOH)

etoh = within(etoh, {
	HNDid = as.numeric(handls_ID)
	etohHadDrink = factor(xferMhx(isEverDrank), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	etohDrankLif = factor(xferMhx(isDrank5),    levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	label(etohHadDrink) = 'Since your last visit, have had a drink of alcohol?'
	label(etohDrankLif) = 'Have you had more than 5 alcoholic drinks in your life?'
	})

etoh = etoh[,zQ(HNDid, etohHadDrink, etohDrankLif)]

describe(etoh)

# Marijuana:

sqlMarij = 'select handls_ID, isEverUsed, isUsed5 from Wave03.hist_tda_marij'
marij = dbGetQuery(HNDdb, sqlMarij)

marij = within(marij, {
	HNDid = as.numeric(handls_ID)
	marijEver = factor(xferMhx(isEverUsed), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	marijLife = factor(xferMhx(isUsed5), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	label(marijEver) = 'Since your last visit, have you used marijuana?'
	label(marijLife) = 'Have you used marijuana more than 5 times in your life?'
	})

marij = marij[,zQ(HNDid,marijEver,marijLife)]

describe(marij)

# Heroin:

sqlHeroin = 'select handls_ID, isEverUsed, isUsed5 from Wave03.hist_tda_heroin'
heroin = dbGetQuery(HNDdb, sqlHeroin)

heroin = within(heroin, {
	HNDid = as.numeric(handls_ID)
	heroinEver = factor(xferMhx(isEverUsed), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	heroinLife = factor(xferMhx(isUsed5), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	label(heroinEver) = 'Since your last visit, have you used heroin?'
	label(heroinLife) = 'Have you used heroin more than 5 times in your life?'
	})

heroin = heroin[,zQ(HNDid,heroinEver,heroinLife)]

describe(heroin)

# Cocaine:

sqlcoke = 'select handls_ID, isEverUsed, isUsed5 from Wave03.hist_tda_coke'
coke = dbGetQuery(HNDdb, sqlcoke)

coke = within(coke, {
	HNDid = as.numeric(handls_ID)
	cokeEver = factor(xferMhx(isEverUsed), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	cokeLife = factor(xferMhx(isUsed5), levels=c(0,1,7,8,9), labels=zQ(No,Yes,DK,Refused,NA))
	label(cokeEver) = 'Since your last visit, have you used cocaine?'
	label(cokeLife) = 'Have you used cocaine more than 5 times in your life?'
	})

coke = coke[,zQ(HNDid,cokeEver,cokeLife)]

describe(coke)

# Merge...

w03MedHx = merge(smoke,    etoh,   'HNDid')
w03MedHx = merge(w03MedHx, marij,  'HNDid')
w03MedHx = merge(w03MedHx, heroin, 'HNDid')
w03MedHx = merge(w03MedHx, coke,   'HNDid')

describe(w03MedHx)

w03MedHx = HNDselIDrange(w03MedHx)

dim(w03MedHx)

save(w03MedHx, file='/prj/hnd/zrdata/w03MedHx.rdata')
save(w03MedHx, file='~/Desktop/w03MedHx.rdata', compress=F)
