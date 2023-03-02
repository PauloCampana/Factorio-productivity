library(shiny)
library(shinythemes)
library(reactablefmtr)

fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel(h2(id = "head", "Productivity module pay-off table"), windowTitle = "Productivity"),
    tags$style(HTML("#head {font-weight: normal;}")),
    sidebarLayout(
        sidebarPanel(
            checkboxInput(
                inputId = "assembler", value = TRUE,
                img(src = "Assembler.png", width = 24, height = 24, strong("MK III Assembler"))
            ),
            sliderInput(
                inputId = "beacons_on_machine", value = 8, min = 0, max = 20,
                post = img(src = "Beacon.png", width = 16, height = 16),
                strong("Beacons affecting each machine")
            ),
            sliderInput(
                inputId = "machines_on_beacon", value = 8, min = 0, max = 12,
                post = img(src = "Beacon.png", width = 16, height = 16),
                strong("Machines affected by each beacon")
            ),
            hr(),
            checkboxInput(
                inputId = "lab", value = FALSE,
                strong("Lab settings")
            ),
            conditionalPanel(
                condition = "input.lab",
                numericInput(
                    inputId = "labspeed", value = 3.5, min = 0, max = 3.5, step = 0.1,
                    img(src = "Research.png", width = 32, height = 32, strong("Lab speed"))
                ),
                numericInput(
                    inputId = "researchspeed", value = 60, min = 0, max = 60,
                    img(src = "Researchspeed.png", width = 20, height = 20, strong("Research time"))
                ),
                checkboxGroupInput(
                    inputId = "researchcost", label = "Science packs",
                    choiceNames = list(
                        img(src = "Automation science.png", width = 32, height = 32, strong("Automation science")),
                        img(src =   "Logistic science.png", width = 32, height = 32, strong(  "Logistic science")),
                        img(src =   "Military science.png", width = 32, height = 32, strong(  "Military science")),
                        img(src =   "Chemical science.png", width = 32, height = 32, strong(  "Chemical science")),
                        img(src = "Production science.png", width = 32, height = 32, strong("Production science")),
                        img(src =    "Utility science.png", width = 32, height = 32, strong(   "Utility science")),
                        img(src =      "Space science.png", width = 32, height = 32, strong(     "Space science"))
                    ),
                    choiceValues = list("aut", "log", "mil", "che", "pro", "uti", "spa"),
                    selected = list("aut", "log", "che", "pro", "uti", "spa")
                )
            ),
            hr(),
            checkboxInput(
                inputId = "raw", value = FALSE,
                strong("Raw materials settings")
            ),
            conditionalPanel(
                condition = "input.raw",
                numericInput(
                    inputId = "iron", value = 1, min = 0, step = 0.1,
                    img(src = "Iron ore.png", width = 32, height = 32, strong("Iron ore"))
                ),
                numericInput(
                    inputId = "copper", value = 1, min = 0, step = 0.1,
                    img(src = "Copper ore.png", width = 32, height = 32, strong("Copper ore"))
                ),
                numericInput(
                    inputId = "coal", value = 1, min = 0, step = 0.1,
                    img(src = "Coal.png", width = 32, height = 32, strong("Coal"))
                ),
                numericInput(
                    inputId = "stone", value = 1, min = 0, step = 0.1,
                    img(src = "Stone.png", width = 32, height = 32, strong("Stone"))
                ),
                numericInput(
                    inputId = "uranium", value = 2.3, min = 0, step = 0.1,
                    img(src = "Uranium ore.png", width = 32, height = 32, strong("Uranium ore"))
                ),
                numericInput(
                    inputId = "petroleum", value = 0.186, min = 0, step = 0.1,
                    img(src = "Petroleum gas.png", width = 32, height = 32, strong("Petroleum gas"))
                ),
                numericInput(
                    inputId = "light", value = 0.151, min = 0, step = 0.1,
                    img(src = "Light oil.png", width = 32, height = 32, strong("Light oil"))
                ),
                numericInput(
                    inputId = "heavy", value = 0.16, min = 0, step = 0.1,
                    img(src = "Heavy oil.png", width = 32, height = 32, strong("Heavy oil"))
                ),
                numericInput(
                    inputId = "crude", value = 0.181, min = 0, step = 0.1,
                    img(src = "Crude oil.png", width = 32, height = 32, strong("Crude oil"))
                ),
                numericInput(
                    inputId = "water", value = 0, min = 0, step = 0.1,
                    img(src = "Water.png", width = 32, height = 32, strong("Water"))
                )
            )
        ),
        mainPanel(
            reactableOutput("table")
        )
    )
) |> shinyUI()
# rsconnect::deployApp(appDir = "~/Documents/R/shiny/prod2", appName = "Factorio")
