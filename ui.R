library(shiny)
library(caret)

houseDF <-read.csv("houseData.csv",stringsAsFactors = FALSE)

subset<-c(
    "id",
    "price",
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
	"zipcode"
)


houseDF$waterfront <- as.factor(houseDF$waterfront)
houseDF$view       <- as.factor(houseDF$view)
houseDF$condition  <- as.factor(houseDF$condition)
houseDF$grade      <- as.factor(houseDF$grade)

houseDF <- houseDF[,subset]

bedsSummary <-summary(houseDF$bedrooms)
bathSummary <-summary(houseDF$bathrooms)
flrsSummary <-summary(houseDF$floors)
yrbuiltSumm <-summary(houseDF$yr_built)
zipSummary  <-summary(houseDF$zipcode)

lotSummary <-summary(houseDF$sqft_lot)
livSummary <-summary(houseDF$sqft_living)
abvSummary <-summary(houseDF$sqft_above)              
basSummary <-summary(houseDF$sqft_basement)
liv15Summ  <-summary(houseDF$sqft_living15)
lot15Summ  <-summary(houseDF$sqft_lot15)

renderInputs1 <- function() {
  wellPanel(
    fluidRow(
      column(12,
        sliderInput("sqft_lot",     "Area of Plot   (sq ft)   :", min = lotSummary[1], max = lotSummary[6]+lotSummary[4], value = lotSummary[3],step = 1),
        sliderInput("sqft_living",  "Area of Living (sq ft)   :", min = livSummary[1], max = livSummary[6]+livSummary[4], value = livSummary[3],step = 1),
        sliderInput("sqft_above",   "Area above ground (sq ft):", min = abvSummary[1], max = abvSummary[6]+abvSummary[4], value = abvSummary[3],step = 1),
        sliderInput("sqft_basement","Area of Basement (sq ft) :", min = basSummary[1], max = basSummary[6]+basSummary[4], value = basSummary[3],step = 1)
      )
    )
  )
}

renderInputs2 <- function() {
  wellPanel(
    fluidRow(
      column(12,
        sliderInput("sqft_lot15"   ,"Average Plot Area of 15 nearest neighbors (sq ft):", min = lot15Summ[1], max = lot15Summ[6]+lot15Summ[4], value = lot15Summ[3],step = 1),  	  
        sliderInput("sqft_living15","Average Living Area of 15 nearest neighbors (sq ft):", min = liv15Summ[1], max = liv15Summ[6]+liv15Summ[4], value = liv15Summ[3],step = 1),
        sliderInput("yr_built"     ,"Year Of Built          :", min = yrbuiltSumm[1], max = yrbuiltSumm[6], value = yrbuiltSumm[3], step = 5,sep=""),
        sliderInput("zipcode"      ,"ZIP Code of House      :", min = zipSummary[1] , max = zipSummary[6],  value = zipSummary[3] , step = 5,sep="")
      )
    )
  )
}

renderInputs3 <- function() {
  wellPanel(
    fluidRow(
      column(12,
        sliderInput("bedrooms" , "Number of Bedrooms  :", min = bedsSummary[1], max = bedsSummary[6]+5, value = bedsSummary[3],step = 1),
        sliderInput("bathrooms", "Number of Bathrooms :", min = bathSummary[1], max = bathSummary[6]+5, value = bathSummary[3],step = 1),
        sliderInput("floors"   , "Number of Floors    :", min = flrsSummary[1], max = flrsSummary[6]+5, value = flrsSummary[3],step = 1)
      )
    )
  )
}

renderInputs4 <- function() {
  wellPanel(
    fluidRow(
      column(12,
	  	checkboxInput('waterfront', 'Is Water Body Facing'),
	    selectInput('condition', 'Condition of House', levels(houseDF$condition)),
		selectInput('grade'    , 'Grade of House',     levels(houseDF$grade)),
		selectInput('view'     , 'View from the House',levels(houseDF$view))
      )
    )
  )
}

renderInputs5 <- function() {
  wellPanel(
    fluidRow(
	  column(1,tags$p("# Beds :")),
	  column(1,verbatimTextOutput("bedrooms")),

	  column(1,tags$p("Bathrooms :")),
	  column(1,verbatimTextOutput("bathrooms")),

	  column(1,tags$p("Floors :")),
	  column(1,verbatimTextOutput("floors")),

	  column(1,tags$p("Lot Area :")),
	  column(1,verbatimTextOutput("lot")),

	  column(1,tags$p("Area Above:")),
	  column(1,verbatimTextOutput("above")),

	  column(1,tags$p("Area Basement:")),
	  column(1,verbatimTextOutput("base"))	  
	 )
  )
}

renderInputs6 <- function() {
  wellPanel(
    fluidRow(
	  column(6,submitButton("Predict"),align="right"), 
	  column(6,verbatimTextOutput("pdictprice"),align="left")
	 )
  )
}

# Define UI for application that plots random distributions
shinyUI(fluidPage
(
    theme="simplex.min.css",
    tags$style(type="text/css","color {font-color :#069;}",
    "label {font-size: 14px;}",
    ".recalculating {opacity: 1.0;}"
  ),

  # Application title
  tags$h2("A Simulation to Predict Price of a House in King County,Seattle,US"),    
  hr(),

  fluidRow(
		column(6, renderInputs1()),
		column(6, renderInputs2())
		),
  fluidRow(
		column(6, renderInputs3()),
		column(6, renderInputs4())
		),
  fluidRow(column(12, renderInputs5(), align="center")),
  fluidRow(column(12, renderInputs6(), align="center"))
 )
)