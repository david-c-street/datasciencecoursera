library(shiny)
library(ggplot2)
library(dplyr)
library(stats)
data(ToothGrowth)

#create a tbl_df version of data for quick parsing and plotting
tg <- tbl_df(ToothGrowth)
tg <- mutate(tg, dosefact=as.factor(dose), ypos=runif(dim(tg)[1], -0.5, 0.5))

# set hard limits that keeps the scale the same for comparison
xhi <- 40

#create function to filter data based on checkbox inputs
cboxfilter <- function(tg, cboxin) {
	#filter data based on checkbox input
	#start with VC and OJ
	if (sum("1" == cboxin) > 0){
		tg1<-filter(tg, supp=="VC")
	} else {tg1 <- filter(tg, dose==0)}
	
	if (sum("2" == cboxin) > 0){
		tg2<-filter(tg, supp=="OJ")
	} else {tg2 <- filter(tg, dose==0)}
	
	tgtmp <- rbind(tg1, tg2)
	
	#Now apply dose inputs
	if (sum("3" == cboxin) > 0){
		tg3 <- filter(tgtmp, dose==0.5)
	} else {tg3 <- filter(tgtmp, dose==0)}
	
	if (sum("4" == cboxin) > 0){
		tg4 <- filter(tgtmp, dose==1.0)
	} else {tg4 <- filter(tgtmp, dose==0)}
	
	if (sum("5" == cboxin) > 0){
		tg5 <- filter(tgtmp, dose==2.0)
	} else {tg5 <- filter(tgtmp, dose==0)}
	
	tguse <- rbind(tg3, tg4, tg5)
	
	if (dim(tguse)[1]==0) {tguse <- tg[1,]}
		
	return(tguse)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
	#Create data table for the introduction
	output$tghead <- renderTable({head(ToothGrowth, n=5)})
	
	#Create histogram
	output$histplot <- renderPlot({
		#filter the data based on checkbox inputs
		tguse <- cboxfilter(tg, input$usedata)
		
		# draw the histogram with the specified number of bins
		# set hard limits that keeps the scale the same for comparison
		yhi <- 10
		hplot <- ggplot(data=tguse)
		hplot <- hplot + coord_cartesian(xlim=c(0,xhi), ylim=c(0,yhi))
		hplot <- hplot + geom_histogram(aes(x=len), binwidth=input$binw)
		hplot <- hplot + labs(title='Tooth Length Histogram', x='tooth length')
		if (dim(tguse)[1] == 1) {
			hplot <- hplot + geom_text(label="OH NO!?! NO DATA!", 
								x=20, y=6, size=10)
		}
		print(hplot)
	})
	
	#Create jitter plot with facets
	output$jitterplot <- renderPlot({
		#filter the data based on checkbox inputs
		tguse <- cboxfilter(tg, input$usedata)
		
		jplot <- ggplot(data=tguse)
		jplot <- jplot + coord_cartesian(xlim=c(0,xhi), ylim=c(-1,1))
		jplot <- jplot + geom_point(aes(x=len, y=ypos, color=dosefact), size=5)
		jplot <- jplot + facet_grid(supp~.)
		jplot <- jplot + labs(title='Tooth Length Jitter Plot (OJ=Orange Juice, VC=Ascorbic Acid)', x='tooth length', y='')
		if (dim(tguse)[1] == 1) {
			jplot <- jplot + geom_text(label="OH NO!?! NO DATA!", 
								x=20, y=0, size=10)
		}
		print(jplot)
	})
	
	#Create box plot
	output$boxplot <-renderPlot({
		#filter the data based on checkbox inputs
		tguse <- cboxfilter(tg, input$usedata)
		
		bplottitle <- 'Tooth growth by supplement (OJ=Orange Juice, VC=Ascorbic Acid)'
		bplot <- ggplot(tguse, aes(y=len, x=supp))
		bplot <- bplot + coord_cartesian(ylim=c(0,xhi))
		bplot <- bplot + geom_boxplot()
		bplot <- bplot + labs(title=bplottitle, 
			x='Supplement Method', y='tooth length')
		if (dim(tguse)[1] == 1) {
			bplot <- bplot + geom_text(label="OH NO!?! NO DATA!", 
								x=1, y=4.25, size=10)
		}
		print(bplot)
	})
	
	#Create violin plot
	output$violinplot <-renderPlot({
		#filter the data based on checkbox inputs
		tguse <- cboxfilter(tg, input$usedata)
		
		vplottitle <- 'Tooth growth by supplement (OJ=Orange Juice, VC=Ascorbic Acid)'
		vplot <- ggplot(tguse, aes(y=len, x=supp))
		vplot <- vplot + coord_cartesian(ylim=c(0,xhi))
		vplot <- vplot + geom_violin()
		vplot <- vplot + labs(title=vplottitle, 
			x='Supplement Method', y='tooth length')
		if (dim(tguse)[1] == 1) {
			vplot <- vplot + geom_text(label="OH NO!?! NO DATA!", 
								x=1, y=4.25, size=10)
		}
		print(vplot)
	})
})