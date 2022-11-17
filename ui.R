library(shiny)
library(shinythemes)
library(reactablefmtr)

shinyUI(
    fluidPage(
        theme = shinytheme("cosmo"),
        titlePanel(h2(id = "head", "Productivity module pay-off table"), windowTitle = "Productivity"),
        tags$style(HTML("#head {font-weight: normal;}")),
        sidebarLayout(
            sidebarPanel(

checkboxInput(
    inputId = "assembler", value = TRUE,
    img(src = "Assembler.png", width = 24, height = 24, "MK III Assembler" |> strong())
),

sliderInput(
    inputId = "beacon", value = 8, min = 0, max = 12,
    img(src = "Beacon.png", width = 24, height = 24, "Number of beacons" |> strong())
),

numericInput(
    inputId = "sciencetime", value = 60, min = 0, max = 60,
    img(src = "Research.png", width = 24, height = 24, "Research time" |> strong())
),

checkboxGroupInput(
    inputId = "sciencecost", label = "Science packs",
    choiceNames = list(
        img(src = "Automation science.png", width = 24, height = 24, "Automation science" |> strong()),
        img(src = "Logistic science.png",   width = 24, height = 24, "Logistic science"   |> strong()),
        img(src = "Military science.png",   width = 24, height = 24, "Military science"   |> strong()),
        img(src = "Chemical science.png",   width = 24, height = 24, "Chemical science"   |> strong()),
        img(src = "Production science.png", width = 24, height = 24, "Production science" |> strong()),
        img(src = "Utility science.png",    width = 24, height = 24, "Utility science"    |> strong()),
        img(src = "Space science.png",      width = 24, height = 24, "Space science"      |> strong())
    ),
    choiceValues = list(3, 7, 24.5, 27.7, 98.5, 106, 225),
    selected = c(3, 7, 27.7, 98.5, 106, 225)
)

            ),
            mainPanel(
                reactableOutput("table")
            )
        )
    )
)
# rsconnect::deployApp(appDir = "~/Documents/R/shiny/prod", appName = "Factorio")
