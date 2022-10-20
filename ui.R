library(shiny)
library(dplyr)
library(reactablefmtr)
shinyUI(
    fluidPage(
        titlePanel("Productivity module seconds to pay-off table"),
        sidebarLayout(
            sidebarPanel(

                numericInput(
                    inputId = "sciencetime", value = 60, min = 0, max = 60,
                    label = img(src = "lab.png", width = 24, height = 24, "Research time" |> strong())
                ),

                checkboxGroupInput(
                    inputId = "sciencecost", label = "Science packs",
                    selected = c(3, 7, 27.7, 98.5, 106, 225),
                    choiceNames = list(
                        img(src = "automation.png", width = 24, height = 24, "Automation science" |> strong()),
                        img(src = "logistic.png", width = 24, height = 24, "Logistic science" |> strong()),
                        img(src = "military.png", width = 24, height = 24, "Military science" |> strong()),
                        img(src = "chemical.png", width = 24, height = 24, "Chemical science" |> strong()),
                        img(src = "production.png", width = 24, height = 24, "Production science" |> strong()),
                        img(src = "utility.png", width = 24, height = 24, "Utility science" |> strong()),
                        img(src = "space.png", width = 24, height = 24, "Space science" |> strong())
                    ),
                    choiceValues = list(3, 7, 24.5, 27.7, 98.5, 106, 225)
                ),

                checkboxInput(
                    inputId = "assembler", value = TRUE,
                    label = img(src = "assembler.png", width = 24, height = 24, "MK III Assembler" |> strong())
                )

            ),
            mainPanel(
                reactableOutput("table")
            )
        )
    )
)
# rsconnect::deployApp(appDir = "~/Documents/R/prod", appName = "Factorio")
