library(shiny)
library(dplyr)
library(lubridate)
library(reactablefmtr)

shinyServer(function(input, output){

    speed <- input$assembler |> ifelse(1.25, 0.75) |> reactive()
    slots <- input$assembler |> ifelse(4, 2) |> reactive()

    module <- data.frame(
        cost1 = c(70.4, 140.8, 211.2, 281.6),
        cost2 = c(712.6, 1425.2, 2137.8, 2850.4),
        cost3 = c(3993.8, 7987.6, 11981.4, 15975.2),
        prod1 = c(1.04, 1.08, 1.12, 1.16),
        prod2 = c(1.06, 1.12, 1.18, 1.24),
        prod3 = c(1.1, 1.2, 1.3, 1.4),
        penalty1 = c(-0.05, -0.1, -0.15, -0.2),
        penalty2 = c(-0.1, -0.2, -0.3, -0.4),
        penalty3 = c(-0.15, -0.3, -0.45, -0.6),
        speed = c(0.2, 0.3, 0.5, 999)
    )

    data <- reactive({
        data.frame(
            item = c(
                "Lubricant", "Heavy oil", "Light oil", "Petroleum gas", "Sulfuric acid",
                "Iron plate", "Copper plate", "Stone brick", "Steel plate", "Solid fuel",
                "Plastic bar", "Sulfur", "Battery", "Explosives",
                "Copper cable", "Iron stick", "Iron gear wheel", "Electronic circuit", "Advanced circuit",
                "Processing unit", "Engine unit", "Electric engine unit", "Flying robot frame",
                "Rocket control unit", "Low density structure", "Rocket fuel",
                "Automation science", "Logistic science", "Military science",
                "Chemical science", "Production science", "Utility science", "Space science",
                "Research"
            ),
            cost = c(
                0.16, 0.16, 0.15, 0.18, 0.29,
                1, 1, 2, 5, 1.5, 2.3, 2.7, 7.8, 1.9,
                0.5, 0.5, 2, 2.5, 12, 74.6, 9, 17, 44.4,
                145, 41.5, 16,
                3, 7, 24.5, 27.7, 98.5, 106, 225,
                input$sciencecost |> as.numeric() |> sum()
            ),
            recipetime = c(
                0.1, 0.2, 0.066, 0.1, 0.02,
                3.2, 3.2, 3.2, 16, 2, 0.5, 0.5, 4, 2,
                0.25, 0.25, 0.5, 0.5, 6, 10, 10, 10, 20,
                30, 20, 30,
                5, 6, 5, 12, 7, 7, 0.3,
                input$sciencetime
            ),
            speed = c(
                1, 1, 1, 1, 1,
                2, 2, 2, 2, 1, 1, 1, 1, 1,
                speed(), speed(), speed(), speed(), speed(), speed(), speed(), speed(), speed(),
                speed(), speed(), speed(),
                speed(), speed(), speed(), speed(), speed(), speed(), 1,
                3.5
            ),
            slots = c(
                3, 3, 3, 3, 3,
                2, 2, 2, 2, 3, 3, 3, 3, 3,
                slots(), slots(), slots(), slots(), slots(), slots(), slots(), slots(), slots(),
                slots(), slots(), slots(),
                slots(), slots(), slots(), slots(), slots(), slots(), 4,
                2
            )
        )
    })

    tabledata <- reactive({
        data.frame(
            Item = data()[ ,1],
            Tier3 = (
                (module$cost3[data()[ ,5]] + input$beacon * module$cost3[2]) *
                data()[ ,3] / data()[ ,2] /
                (data()[ ,4] * (1 + module$penalty3[data()[ ,5]] + input$beacon * module$speed[3])) /
                (1 - 1 / module$prod3[data()[ ,5]])
            ) |> round(digits = 0),
            Tier2 = (
                (module$cost2[data()[ ,5]] + input$beacon * module$cost2[2]) *
                data()[ ,3] / data()[ ,2] /
                (data()[ ,4] * (1 + module$penalty2[data()[ ,5]] + input$beacon * module$speed[2])) /
                (1 - 1 / module$prod2[data()[ ,5]])
            ) |> round(digits = 0),
            Tier1 = (
                (module$cost1[data()[ ,5]] + input$beacon * module$cost1[2]) *
                data()[ ,3] / data()[ ,2] /
                (data()[ ,4] * (1 + module$penalty1[data()[ ,5]] + input$beacon * module$speed[1])) /
                (1 - 1 / module$prod1[data()[ ,5]])
            ) |> round(digits = 0)
        ) |> mutate(
                Tier3formatted = Tier3 |> as.duration() |> as.period() |> tolower(),
                Tier2formatted = Tier2 |> as.duration() |> as.period() |> tolower(),
                Tier1formatted = Tier1 |> as.duration() |> as.period() |> tolower()
        )
    })

    output$table <- renderReactable({
        reactable(
            tabledata(),
            theme = cosmo(header_font_size = 20),
            pagination = FALSE, highlight = TRUE, sortable = FALSE,
            showSortable = TRUE, defaultSorted = "Tier3",
            columns = list(
                Item = colDef(
                    align = "left", filterable = TRUE,
                    header = tagList(
                        span(style = "display: inline-block; width: 32px;",
                             img(src = "Item.png", width = 32, height = 32)
                        ),
                        span(style = "font-weight: bold; margin-left: 8px;", "Item")
                    ),
                    cell = function(item){
                        image <- img(src = sprintf("%s.png", item), width = 32, height = 32)
                        tagList(
                            span(style = "display: inline-block; width: 32px;", image),
                            span(style = "font-weight: bold; margin-left: 8px;", item)
                        )
                    }
                ),
                Tier3formatted = colDef(
                    align = "center", vAlign = "center",
                    cell = data_bars(
                        tabledata(), fill_by = "Tier3",
                        text_position = "above", bold_text = TRUE,
                        fill_color = "#ffa020", bar_height = 10, round_edges = TRUE,
                        border_style = "solid", border_width = "1px", border_color = "#00000040"
                    ),
                    header = tagList(
                        if(input$beacon > 0){
                            span(style = "display: inline-block; width: 32px;",
                                img(src = "Speed3.png", width = 32, height = 32)
                            )
                        },
                        span(style = "display: inline-block; width: 32px;",
                             img(src = "Prod3.png", width = 32, height = 32)
                        ),
                        span(style = "font-weight: bold; margin-left: 8px;", "Tier 3")
                    )
                ),
                Tier2formatted = colDef(
                    align = "center", vAlign = "center",
                    cell = data_bars(
                        tabledata(), fill_by = "Tier2",
                        text_position = "above", bold_text = TRUE,
                        fill_color = "#ff9010", bar_height = 10, round_edges = TRUE,
                        border_style = "solid", border_width = "1px", border_color = "#00000040"
                    ),
                    header = tagList(
                        if(input$beacon > 0){
                            span(style = "display: inline-block; width: 32px;",
                                img(src = "Speed2.png", width = 32, height = 32)
                            )
                        },
                        span(style = "display: inline-block; width: 32px;",
                             img(src = "Prod2.png", width = 32, height = 32)
                        ),
                        span(style = "font-weight: bold; margin-left: 8px;", "Tier 2")
                    )
                ),
                Tier1formatted = colDef(
                    align = "center", vAlign = "center",
                    cell = data_bars(
                        tabledata(), fill_by = "Tier1",
                        text_position = "above", bold_text = TRUE,
                        fill_color = "#ff8000", bar_height = 10, round_edges = TRUE,
                        border_style = "solid", border_width = "1px", border_color = "#00000040"
                    ),
                    header = tagList(
                        if(input$beacon > 0){
                            span(style = "display: inline-block; width: 32px;",
                                img(src = "Speed1.png", width = 32, height = 32)
                            )
                        },
                        span(style = "display: inline-block; width: 32px;",
                             img(src = "Prod1.png", width = 32, height = 32)
                        ),
                        span(style = "font-weight: bold; margin-left: 8px;", "Tier 1")
                    )
                ),
                Tier3 = colDef(show = FALSE),
                Tier2 = colDef(show = FALSE),
                Tier1 = colDef(show = FALSE)
            )
        )
    })
})