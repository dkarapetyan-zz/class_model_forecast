# TODO: Createe RShiny version of graph_forecast server, and deploy
# 
# Author: David Karapetyan
###############################################################################

source("/Users/davidkarapetyan/Documents/code/r/ppnr.quant.repo/class_model/src/graphs/graph_forecast.R")
library("shiny")

shinyServer(function(input, output) {
			
			
			
			output$ui <- renderUI({
						if (is.null(input$book))
							return()
						
# Depending on input$book, we'll generate a different
# UI component and send it to the client.
						switch(input$book,
								"Capital" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Allowed DTA",
												"Capital",
												"Dividends",
												"Leverage Ratio",
												"Net Income",
												"Net Income Before Tax",
												"Taxes",
												"Tier 1 Common Capital"),
										selected = "Net Income"),
								"LLL" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												#"Total Reserves 000 ",
												"Provision"),
										selected = "Provision"),
								"AFS" =  selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"Return on AFS Securities",
												"Total AFS Securities",
												"Gain AFS Securities"),
										selected = "Total AFS Securities"),
								"NCO" = selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												"FirstLien Residential Real Estate",
												"Junior Lien Residential Real Estate",
												"HELOC Residential Real Estate",
												"Construction Commercial Real Estate",
												"Multifamily Commercial Real Estate",
												"NonFarm NonResidential CRE",
												"Credit Card",
												"Other Consumer",
												"CI", 
												"Leases",
												"Other Real Estate",
												"Loans to Foreign Governments",
												"Agriculture",
												"Loans to Depository Institutions",
												"Other"),
										selected = "CI"),
#								"PPNR" = selectInput("dynamic_var", "Choose a variable:",
#										choices = c(
#												"Compensation Noninterest Expense Ratio", 
#												"Fixed Asset Noninterest Expense Ratio",
#												"Net Interest Margin",
#												"Noninterest Nontrading Income Ratio",
#												"Other Noninterest Expense Ratio",
#												"Return on Trading Assets"),
#								selected = "Return on Trading Assets"),
#								"Asset Coefficients" = selectInput("dynamic_var", "Choose a variable:",
#										choices = c(
#												"Net Interest Margin",
#												"Noninterest Nontrading Income Ratio", 
#												"Compensation Noninterest Expense Ratio",
#												"Fixed Asset Noninterest Expense Ratio",
#												"Other Noninterest Expense Ratio",
#												"Return on Trading Assets"),
#								selected = "Net Interest Margin"),
								"Loss" =  selectInput("dynamic_var", "Choose a variable:",
										choices = c(
												#"4-Qrt Net Charge-offs",
												#"Agriculture",
												"CI", 
												"Construction Commercial Real Estate",
												"Credit Card",
												"FirstLien Residential Real Estate",
												"HELOC Residential Real Estate",
												"Junior Lien Residential Real Estate",
												"Leases",
												"Loans to Depository Institutions",
												"Loans to Foreign Governments",
												"Multifamily Commercial Real Estate",
												"NonFarm NonResidential CRE",
												"Other",
												"Other Consumer",
												"Other Real Estate",
										#"Total Net Charge-offs"
										),
										selected = "CI"))
					})
			
			
			
			
			
			output$plot <- renderPlot(
					{
						GraphForecast(
								book = input$book,
								variable = input$dynamic_var,
								bank = input$bank,
								quarter = "2014Q3",
								nco_data = nco_data,
								ppnr_data = ppnr_data,
								total_assets = total_assets,
								capital_data = capital_data,
								model_coefficients = model_coefficients_ey,
								macro_forecasts = macro_forecasts)
					}
			)				
		})

