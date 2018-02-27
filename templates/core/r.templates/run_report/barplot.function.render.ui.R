
output$.TAB.PLOT.VAR.UI <- renderUI ({
	if (input$.TAB.PLOT.VAR.ShowPanel) {
		sidebarLayout(
			sidebarPanel(

				radioButtons(
					".TAB.PLOT.VAR.OrientationRadio",
					"Plot Orientation",
					c("horizontal" , "vertical")
				),

				radioButtons(
					".TAB.PLOT.VAR.PlotTypeRadio",
					"Plot Type",
					c("point" , "bar")
				),

				radioButtons(
					".TAB.PLOT.VAR.PlotSize",
					"Plot Size",
					c("full size" , "fit to screen")
				),

				sliderInput(".TAB.PLOT.VAR.FailThreshold",
					"Fail Threshold:",
					min = 0,
					max = .FAIL.SLIDER.MAX.STRING.,
					value = 0
				),

				selectInput(".TAB.PLOT.VAR.Group", "Group by:",
					c("None" = "none",
					"Run" = "Run",
					"Lane" = "Lane",
					"Run and Lane" = "Run_Lane",
					"Barcode" = "Barcode",
					"Study" = "Study",
					"Subject" = "Subject",
					"Sample" = "Sample",
					"Tissue Type and Origin" = "Tissue_type_origin",
					"Date" = "Date",
					"Instrument" = "Instrument",
					"Increment" = "Increment",
					"Flowcell" = "Flowcell"),
				),

				selectInput(".TAB.PLOT.VAR.SelectColumn1", "Select for data where:",
					c("None" = "none",
					"Run" = "Run",
					"Lane" = "Lane",
					"Run and Lane" = "Run_Lane",
					"Barcode" = "Barcode",
					"Study" = "Study",
					"Subject" = "Subject",
					"Sample" = "Sample",
					"Tissue Type and Origin" = "Tissue_type_origin",
					"Date" = "Date",
					"Instrument" = "Instrument",
					"Increment" = "Increment",
					"Flowcell" = "Flowcell")
				),
				selectInput(".TAB.PLOT.VAR.SelectType1", "",
					c("equals" = "equals",
					"contains" = "contains",
					"does not equal" = "does_not_equal",
					"does not contain" = "does_not_contain")
				),
				textInput(".TAB.PLOT.VAR.SelectValue1", "", "")

			),
			mainPanel(
				plotlyOutput(".TAB.PLOT.VAR.Plot" , height="120vh")
			)
		)
	} else {
		plotlyOutput(".TAB.PLOT.VAR.Plot" , height="120vh")
	}
})
