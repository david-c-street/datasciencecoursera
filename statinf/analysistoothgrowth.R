#script to run some confidence intervals and tests on tooth growth data
library(ggplot2)
library(dplyr)

data(ToothGrowth)

tg <- tbl_df(ToothGrowth)
tg_dose <- mutate(tg, dosefact=as.factor(dose))

tg_bysuppdose <- group_by(tg, supp, dose)
tg_bysuppdose_stat <- summarise(tg_bysuppdose, 
	meanlen=mean(len), sdlen=sd(len))

vplottitle <- 'Tooth growth by supplement (OJ=Orange Juice, VC=Ascorbic Acid)'
vplot <- ggplot(tg_dose, aes(y=len, x=dosefact))
vplot <- vplot + geom_violin() + facet_wrap(~supp)
vplot <- vplot + labs(title=vplottitle, 
	x='vitamin C dose (mg)', y='tooth length')

#testing affect of dose	
tg_bydose <- group_by(tg, dose)
tg_bydosemean <- summarise(tg_bydose, meanlen=mean(len))
dose1 <- filter(ToothGrowth, dose==0.5)[,'len']
dose2 <- filter(ToothGrowth, dose==1.0)[,'len']
t1 <- t.test(dose1, dose2, alternative='greater')

dose1 <- filter(ToothGrowth, dose==1.0)[,'len']
dose2 <- filter(ToothGrowth, dose==2.0)[,'len']
t2 <- t.test(dose1, dose2, alternative='greater')

dose1 <- filter(ToothGrowth, dose==0.5)[,'len']
dose2 <- filter(ToothGrowth, dose==2.0)[,'len']
t3 <- t.test(dose1, dose2, alternative='greater')

#testing effect of supplementation method
tg_bysupp <- group_by(tg, supp)
tg_bysuppmean <- summarise(tg_bysupp, meanlen=mean(len))
supp1 <- filter(ToothGrowth, supp=='VC')[,'len']
supp2 <- filter(ToothGrowth, supp=='OJ')[,'len']
t4 <- t.test(supp1, supp2, alternative='greater')