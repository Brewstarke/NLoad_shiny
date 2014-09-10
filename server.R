
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



shinyServer( # this will be run each time a user changes something.
	function(input, output) {
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
			return(input$AtmDepRate * input$AgArea * input$NTransAg)
		}
		#d
		AtmImperv <- function(){
			return((input$AtmDepRate * (input$RoofArea + input$DrivewayArea) * input$NtransTurf) * (input$AtmDepRate * input$ImpervArea)) #Need help with this formula
		}
		#e
		AtmWetlands <- function(){
			return(input$AtmDepRate * input$WetlandsArea)
		}
		#f
		AtmPonds <- function(){
			return(input$AtmDepRate * input$PondsArea * input$ThroughAquiferPonds)
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
		

	output$Load1 <- renderText({
		paste("Your total load is ", AtmNatVeg() + AtmTurf() + AtmImperv() + AtmWetlands() + AtmPonds(), "KG N/ha/year")
	})
	output$TotalLoad <- renderText({
		paste("The total load under this scenario is ", TotalLoadAtmospheric(), "KG N/ha/year")
	})
	ouput$AtmAg <- renderText({
		paste("Ag load is ", AtmAg())
	})
	
}
)

