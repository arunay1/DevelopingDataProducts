library(shiny)
houseDF <-read.csv("houseData.csv",stringsAsFactors = FALSE)

paramNames <-c(	
	"sqft_lot",                    # Plot size
	"sqft_living",                 # Living Area 
	"sqft_above",                  # square feet above ground
	"sqft_basement",               # square feet in basement
	"waterfront",                  # waterfront property
	"view",                        # type of view
	"condition",                   # condition of house
	"grade",                       # measure of quality of construction	
	"bedrooms",
	"bathrooms",
	"floors",                      # Number of Floors
	"sqft_living15",               # Average sq.ft. of 15 nearest neighbors
	"sqft_lot15",                  # Average lot size of 15 nearest neighbors
	"yr_built",                    # the year built
	"zipcode",
    "id",
    "price"	
)

houseDF$waterfront <- as.factor(houseDF$waterfront)
houseDF$view       <- as.factor(houseDF$view)
houseDF$condition  <- as.factor(houseDF$condition)
houseDF$grade      <- as.factor(houseDF$grade)

houseDF <- houseDF[,paramNames]


trainIndex <- createDataPartition(houseDF$price,p=0.7,list=FALSE)
train      <- houseDF[trainIndex,]
test       <- houseDF[-trainIndex,]

houseLinearModel <- lm(formula=price ~ bedrooms  + bathrooms + floors + waterfront + view + condition + grade + sqft_lot + sqft_living +
                                       sqft_above + sqft_basement + sqft_living15 + sqft_lot15+ yr_built+ zipcode, data=train)

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
shinyServer(function(input, output, session) {

	getParams <- function() {
		input[["recalc"]]
		params <- lapply(paramNames, function(p){
		input[[p]]
		})
		names(params) <- paramNames
		params
	 }

  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  output$pdictprice <-renderText(predict_price(getParams()))

  output$bedrooms <-renderText(getParams()[[9]])
  output$bathrooms<-renderText(getParams()[[10]])
  output$floors   <-renderText(getParams()[[11]])
  
  
  output$lot   <-renderText(getParams()[[1]])
  output$live  <-renderText(getParams()[[2]])  
  output$above <-renderText(getParams()[[3]])
  output$base  <-renderText(getParams()[[4]])
})

predict_price<-function(params)
{
	sqft_lot     <- params[[1]]
	sqft_living  <- params[[2]]
	sqft_above   <- params[[3]]
	sqft_basement<- params[[4]]
	

	if(params[[5]] == TRUE)
		waterfront   <- 1
	else
		waterfront   <- 0
	waterfront<-as.factor(waterfront)
	
	view    	 <- params[[6]]
	condition	 <- params[[7]]
	grade        <- params[[8]] 
	         
	bedrooms     <- params[[9]]
	bathrooms    <- params[[10]]
	floors       <- params[[11]]
	
	sqft_living15<- params[[12]]
	sqft_lot15   <- params[[13]]
	yr_built     <- params[[14]]
	zipcode      <- params[[15]]

	
	house<-list("sqft_lot"=sqft_lot,
	"sqft_living"=sqft_living,
	"sqft_above"=sqft_above,   
	"sqft_basement"=sqft_basement,	              
	"waterfront"=waterfront,   
	"view"=view,    	 
	"condition"=condition,
	"grade"=grade,	                        
	"bedrooms"=bedrooms,
	"bathrooms"=bathrooms,    
	"floors"=floors,  	              
	"sqft_living15"=sqft_living15,
	"sqft_lot15"=sqft_lot15,
	"yr_built"=yr_built,
    "zipcode"=zipcode)
	
	PredictedPrice<-predict(houseLinearModel,house)
	paste(PredictedPrice)
}