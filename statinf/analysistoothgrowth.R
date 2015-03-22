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