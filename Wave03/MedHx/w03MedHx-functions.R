isMhx = function(dat) {

#	Recode medical history "is" status item & create factor:
#		input: -1=No, 1 = Yes, 7=DK, 8=Refused
#		output: 0=No, 1=Yes, (7,8,9)=NA

	dat = ifelse(is.na(dat), 9, dat)
	dat = ifelse(dat==-1, 0, dat)
	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:1, labels=c('No','Yes'))
	dat
	}

howOld = function(dat) {

	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongNum = function(dat) {

	dat = ifelse(is.na(dat),0,dat)
	dat = ifelse(dat=='Dont Know' | dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	dat
	}

howLongUnit = function(dat) {

	dat = ifelse(is.na(dat) | dat==0, 9, dat)
	dat = ifelse(dat == 'seconds', 0, dat)
	dat = ifelse(dat == 'minutes', 1, dat)
	dat = ifelse(dat == 'hours', 2, dat)
	dat = ifelse(dat == 'days', 3, dat)
	dat = ifelse(dat == 'weeks', 4, dat)
	dat = ifelse(dat == 'months', 5, dat)
	dat = ifelse(dat == 'years', 6, dat)

	dat = ifelse(dat<7, dat, NA)

	dat = factor(dat, levels=0:6, labels=c('seconds','minutes','hours','days','weeks','months','years'))
	dat
	}

howMany = function(dat) {

	dat = ifelse(is.na(dat), 0, dat)
	dat = ifelse(dat=='Dont know' | dat=='Refused', 0, dat)
	dat = ifelse(dat==0, NA, dat)
	}
