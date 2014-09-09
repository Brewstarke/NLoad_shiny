
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Load required libraries

library(shiny)


# Run any data manpluations or function creations here
# N-load backbone formula


# Atm.Dep.load <- function(a,b,c,d,e,f){
# 	NatVeg <- function(AtmDepRate, )
# 		
# }
# Fert.load <- function(g,h,i){
# 	
# }	
# 	
# N.load <- function(j,k,l,m){
# 	Total.Load <- j+k+l+m
# 	return(Total.Load)
# }
# 
# N.load(1,2,3,4)


shinyServer(
	function(input, output) {

	output$text1 <- renderText({
		paste("Your value is ", input$AtmDepRate + input$NtransNatVeg)
	})
  }
 )

