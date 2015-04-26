library(shiny)

shinyUI(navbarPage("ToothGrowth Data Analysis",
	tabPanel("Introduction",
		titlePanel("Example of ToothGrowth Data Analysis"),
		sidebarLayout(position='right',
			sidebarPanel(
				h5('We\'ll be looking at the ToothGrowth data set from R.'),
				h5('In R it can be loaded with the following command:'),
				code('data(ToothGrowth)'),
				h5('It includes tooth length measurements after Guinea Pigs have been given three different doses of Vitamin C as a direct supplement or as Orange Juice.')
			),
			# explain!
			mainPanel(
				h4('This tool is an example of the visual exploration of a set of data. It contains a few interactive pages which allow for a basic exploration of a fairly small data set.'),
				em("The Data Exploration page presents a few ways to visualize the data set being considered. Just play around with adding or removing data categories! Start thinking about possible differences in tooth growth associated with different supplements and doses. The next step of this data analysis, not covered here, would be to check those hypthoses."),
				br(), br(),
				strong("Here is what the data we'll be looking at looks like:"),
				br(), br(),
				code("head(ToothGrowth, n=5)"),
				br(), br(),
				tableOutput("tghead")
			)
		)
	),
	tabPanel("Data Exploration",
		titlePanel("Explore!"),
		sidebarLayout(position='left',
			sidebarPanel(
				sliderInput("binw",
				          "Bin width for histogram",
				          min = 0.5,
				          max = 3,
				          value = 1),
				checkboxGroupInput("usedata", 
				      label = h3("Select Data to Display"), 
				      choices = list("Ascorbic Acid (VC)" = 1, 
							      "Orange Juice (OJ)" = 2, 
								  "0.5 mg dose" = 3,
								  "1.0 mg dose" = 4,
								  "2.0 mg dose" = 5),
							      selected = list(1,2,3,4,5))
			),
			# Show plot
			mainPanel(
				tabsetPanel(
					tabPanel("Histogram", plotOutput("histplot"),
						h5("A good first look graph, has its problems but is very easy to construct to get an early feel for the data. Watch out for bin width though! Certain values will obscure the data, others will make it almost useless to look at.")), 
					tabPanel("Jitter Plot", plotOutput("jitterplot"),
						h5("A simple one dimensional graph to get a sense the shape of the data, like a histogram but don't have to worry about bin width"),
						h5("Note: Y values are random and mean nothing, the values are spread over a y range to avoid overlapping points")),
					tabPanel("Box Plot", plotOutput("boxplot"),
						h5("Gives a better idea of the spread of the data")), 
					tabPanel("Violin Plot", plotOutput("violinplot"),
						h5("Gives an idea of the spread and shape of the data, providing elements of the histogram and boxplot"))
			    )
			)
		)
	)
))