
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Load required libraries

library(shiny)


# Run any data manpluations or function creations here
# N-load backbone formula


# Define each part of the loading formula
#a
AtmNatVeg <- function(){
	return(input$AtmDepRate * input$NatVegArea * input$NtransNatVeg)
}
#b
AtmTurf <- function(){
	return(input$AtmDepRate * input$TurfArea * input$TransTurf)
}
#c
AtmAg <- function(){
	return(input$AtmDepRate * input$AgArea * input$TransAg)
}
#d
AtmImperv <- function(){
	return((input$AtmDepRate * (input$RoofArea + input$DrivewayArea) * input$NtransTurf) * (input$AtmDepRate * input$ImpervArea))) #Need help with this formula
}
#e
AtmWetlands <- function(){
	return(input$AtmDepRate *
}
#f
AtmPonds <- function(){
	return(input$AtmDepRate *
}
## Total N load to estuary sourced from Atmospheric Deposition
TotalLoadAtmospheric <- function(){
	return(AtmNatVeg() + AtmTurf() + AtmAg() + AtmImperv() + AtmWetlands() + AtmPonds())
}
##Via fertilizer application

#g
FertTurf <- function(){
	return(input$FertLawns * input$LawnArea * input$PercentHomes * input$DeNitTurf)
}
#h 	
FertAg <- function(){
	return(input$FertAg * input$AgArea * input$DeNit)
}
#i
FertGolf <- function(){
	return(input$Fert)
}




N.load(1,2,3,4)


shinyServer( # this will be run each time a user changes something.
	function(input, output) {

	output$text1 <- renderText({
		paste("Your total load is ", input$AtmDepRate + input$NtransNatVeg)
	})
  }
 )

