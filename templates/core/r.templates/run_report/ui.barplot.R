                    tabPanel(".Y.LABEL.",
                        fluidPage(
                            bsButton(".TAB.PLOT.VAR.ShowPanel","show/hide sidebar",type="toggle",value=TRUE),
                            uiOutput('.TAB.PLOT.VAR.UI')
                        )
                    ),
