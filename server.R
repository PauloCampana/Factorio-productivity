library(shiny)
library(dplyr)
library(lubridate)
library(reactablefmtr)

function(input, output, session) {

    raw <- c(
        input$iron, input$copper, input$coal, input$stone, input$uranium,
        input$petroleum, input$light, input$heavy, input$crude, input$water
    ) |> reactive()

    speed <- input$assembler |> ifelse(1.25, 0.75) |> reactive()
    slots <- input$assembler |> ifelse(4, 2) |> reactive()

        tier1 <- sum(c(  15, 32.5,   5, 0, 0,   100, 0, 0, 0,    0) * raw()) |> reactive()
        tier2 <- sum(c( 190,  355,  35, 0, 0, 737.5, 0, 0, 0, 87.5) * raw()) |> reactive()
        tier3 <- sum(c(1083, 2000, 190, 0, 0,  4025, 0, 0, 0,  525) * raw()) |> reactive()
       beacon <- sum(c( 110,  135,  20, 0, 0,   400, 0, 0, 0,    0) * raw()) |> reactive()
    assembler <- sum(c(  60,  130,  20, 0, 0,   400, 0, 0, 0,    0) * raw()) |> reactive()

    module <- tibble(
        cost =    c(tier1(), tier2(), tier3()),
        prod =    c(   0.04,    0.06,     0.1),
        penalty = c(  -0.05,    -0.1,   -0.15),
        speed =   c(    0.2,     0.3,     0.5)
    ) |> reactive()

    researchcost <-
        input$researchcost |>
        sapply(
            function(x) {
                case_when(
                    x == "aut" ~ c(    2,      1,     0,     0,     0,      0,     0,  0,   0,     0),
                    x == "log" ~ c(  5.5,    1.5,     0,     0,     0,      0,     0,  0,   0,     0),
                    x == "mil" ~ c(    7,    2.5,     5,    10,     0,      0,     0,  0,   0,     0),
                    x == "che" ~ c(   12,    7.5,   1.5,     0,     0,   37.5,     0,  0,   0,   7.5),
                    x == "pro" ~ c( 52.5,  19.17,  3.33, 11.67,     0,  66.67,     0,  0,   0,     0),
                    x == "uti" ~ c(33.33,  49.83,  3.83,     0,     0, 101.67,     0,  5,   0, 58.33),
                    x == "spa" ~ c(57.54, 101.79,  9.95,     0,     0, 222.25, 115.5,  0,   0, 54.25)
                )
            }
        ) |>
        apply(MARGIN = 1, sum) |>
        reactive()

    cost <- tibble(
                     "Iron plate" = c(    1,      0,     0,     0,     0,      0,     0,  0,   0,     0),
                   "Copper plate" = c(    0,      1,     0,     0,     0,      0,     0,  0,   0,     0),
                    "Stone brick" = c(    0,      0,     0,     2,     0,      0,     0,  0,   0,     0),
                    "Steel plate" = c(    5,      0,     0,     0,     0,      0,     0,  0,   0,     0),
           "Solid fuel petroleum" = c(    0,      0,     0,     0,     0,     20,     0,  0,   0,     0),
               "Solid fuel light" = c(    0,      0,     0,     0,     0,      0,    10,  0,   0,     0),
               "Solid fuel heavy" = c(    0,      0,     0,     0,     0,      0,     0, 20,   0,     0),
                    "Plastic bar" = c(    0,      0,   0.5,     0,     0,     10,     0,  0,   0,     0),
                         "Sulfur" = c(    0,      0,     0,     0,     0,     15,     0,  0,   0,    15),
                        "Battery" = c(  1.4,      1,     0,     0,     0,     30,     0,  0,   0,    70),
                     "Explosives" = c(    0,      0,   0.5,     0,     0,    7.5,     0,  0,   0,  12.5),
                   "Copper cable" = c(    0,    0.5,     0,     0,     0,      0,     0,  0,   0,     0),
                     "Iron stick" = c(  0.5,      0,     0,     0,     0,      0,     0,  0,   0,     0),
                "Iron gear wheel" = c(    2,      0,     0,     0,     0,      0,     0,  0,   0,     0),
                   "Empty barrel" = c(    5,      0,     0,     0,     0,      0,     0,  0,   0,     0),
             "Electronic circuit" = c(    1,    1.5,     0,     0,     0,      0,     0,  0,   0,     0),
               "Advanced circuit" = c(    2,      5,     1,     0,     0,     20,     0,  0,   0,     0),
                "Processing unit" = c( 24.1,     40,     2,     0,     0,   47.5,     0,  0,   0,  17.5),
                    "Engine unit" = c(    9,      0,     0,     0,     0,      0,     0,  0,   0,     0),
           "Electric engine unit" = c(   11,      3,     0,     0,     0,      0,     0, 15,   0,     0),
             "Flying robot frame" = c( 21.8,    9.5,     0,     0,     0,     60,     0, 15,   0,   140),
            "Rocket control unit" = c( 39.1,   72.5,     7,     0,     0,  147.5,     0,  0,   0,  17.5),
          "Low density structure" = c(   10,     20,   2.5,     0,     0,     50,     0,  0,   0,     0),
                    "Rocket fuel" = c(    0,      0,     0,     0,     0,      0,   110,  0,   0,     0),
             "Automation science" = c(    2,      1,     0,     0,     0,      0,     0,  0,   0,     0),
               "Logistic science" = c(  5.5,    1.5,     0,     0,     0,      0,     0,  0,   0,     0),
               "Military science" = c(    7,    2.5,     5,    10,     0,      0,     0,  0,   0,     0),
               "Chemical science" = c(   12,    7.5,   1.5,     0,     0,   37.5,     0,  0,   0,   7.5),
             "Production science" = c( 52.5,  19.17,  3.33, 11.67,     0,  66.67,     0,  0,   0,     0),
                "Utility science" = c(33.33,  49.83,  3.83,     0,     0, 101.67,     0,  5,   0, 58.33),
                  "Space science" = c(57.54, 101.79,  9.95,     0,     0, 222.25, 115.5,  5,   0, 54.25),
             "Uranium processing" = c(    0,      0,     0,     0,    10,      0,     0,  0,   0,     0),
             "Kovarex enrichment" = c(    0,      0,     0,     0, 29.59,      0,     0,  0,   0,     0),
              "Uranium fuel cell" = c(    1,      0,     0,     0,  21.7,      0,     0,  0,   0,     0),
                   "Nuclear fuel" = c(    0,      0,     0,     0, 29.59,      0,   110,  0,   0,     0),
           "Basic oil processing" = c(    0,      0,     0,     0,     0,      0,     0,  0, 100,     0),
        "Advanced oil processing" = c(    0,      0,     0,     0,     0,      0,     0,  0, 100,    50),
              "Coal liquefaction" = c(    0,      0, 10.38,     0,     0,      0,     0,  0,   0,    50),
             "Heavy oil cracking" = c(    0,      0,     0,     0,     0,      0,     0, 40,   0,    30),
             "Light oil cracking" = c(    0,      0,     0,     0,     0,      0,    30,  0,   0,    30),
                      "Lubricant" = c(    0,      0,     0,     0,     0,      0,     0,  1,   0,     0),
                  "Sulfuric acid" = c( 0.02,      0,     0,     0,     0,    1.5,     0, 10,   0,   3.5),
                       "Research" = researchcost()
    ) |> reactive()

    recipe <- tibble(
        recipe = cost() |> colnames(),
        cost = c(
            sum(cost()["Iron plate"             ] * raw()),
            sum(cost()["Copper plate"           ] * raw()),
            sum(cost()["Stone brick"            ] * raw()),
            sum(cost()["Steel plate"            ] * raw()),
            sum(cost()["Solid fuel petroleum"   ] * raw()),
            sum(cost()["Solid fuel light"       ] * raw()),
            sum(cost()["Solid fuel heavy"       ] * raw()),
            sum(cost()["Plastic bar"            ] * raw()),
            sum(cost()["Sulfur"                 ] * raw()),
            sum(cost()["Battery"                ] * raw()),
            sum(cost()["Explosives"             ] * raw()),
            sum(cost()["Copper cable"           ] * raw()),
            sum(cost()["Iron stick"             ] * raw()),
            sum(cost()["Iron gear wheel"        ] * raw()),
            sum(cost()["Empty barrel"           ] * raw()),
            sum(cost()["Electronic circuit"     ] * raw()),
            sum(cost()["Advanced circuit"       ] * raw()),
            sum(cost()["Processing unit"        ] * raw()),
            sum(cost()["Engine unit"            ] * raw()),
            sum(cost()["Electric engine unit"   ] * raw()),
            sum(cost()["Flying robot frame"     ] * raw()),
            sum(cost()["Rocket control unit"    ] * raw()),
            sum(cost()["Low density structure"  ] * raw()),
            sum(cost()["Rocket fuel"            ] * raw()),
            sum(cost()["Automation science"     ] * raw()),
            sum(cost()["Logistic science"       ] * raw()),
            sum(cost()["Military science"       ] * raw()),
            sum(cost()["Chemical science"       ] * raw()),
            sum(cost()["Production science"     ] * raw()),
            sum(cost()["Utility science"        ] * raw()),
            sum(cost()["Space science"          ] * raw()),
            sum(cost()["Uranium processing"     ] * raw()),
            sum(cost()["Kovarex enrichment"     ] * raw()),
            sum(cost()["Uranium fuel cell"      ] * raw()),
            sum(cost()["Nuclear fuel"           ] * raw()),
            sum(cost()["Basic oil processing"   ] * raw()),
            sum(cost()["Advanced oil processing"] * raw()),
            sum(cost()["Coal liquefaction"      ] * raw()),
            sum(cost()["Heavy oil cracking"     ] * raw()),
            sum(cost()["Light oil cracking"     ] * raw()),
            sum(cost()["Lubricant"              ] * raw()),
            sum(cost()["Sulfuric acid"          ] * raw()),
            sum(cost()["Research"               ] * raw())
        ),
        time = c(
            3.2, 3.2, 3.2, 16,
            2, 2, 2, 0.5, 0.5, 4, 2,
            0.25, 0.25, 0.5, 1,
            0.5, 6, 10, 10, 10, 20,
            30, 20, 30,
            5, 6, 5, 12, 7, 7, 0.3,
            12, 60, 10, 90,
            5, 5, 5, 2, 2, 0.1, 0.02,
            input$researchspeed
        ),
        speed = c(
            2, 2, 2, 2,
            1, 1, 1, 1, 1, 1, 1,
            speed(), speed(), speed(), speed(),
            speed(), speed(), speed(), speed(), speed(), speed(),
            speed(), speed(), speed(),
            speed(), speed(), speed(), speed(), speed(), speed(), 1,
            1, 1, speed(), 1,
            1, 1, 1, 1, 1, 1, 1,
            input$labspeed
        ),
        slots = c(
            2, 2, 2, 2,
            3, 3, 3, 3, 3, 3, 3,
            slots(), slots(), slots(), slots(),
            slots(), slots(), slots(), slots(), slots(), slots(),
            slots(), slots(), slots(),
            slots(), slots(), slots(), slots(), slots(), slots(), 4,
            2, 2, slots(), 2,
            3, 3, 3, 3, 3, 3, 3,
            2
        )
    ) |> reactive()

    payoff <- tibble(
        totalcost3 =
            recipe()["slots"] * module()[["cost"]][3] +
            input$beacons_on_machine / input$machines_on_beacon * (2 * module()[["cost"]][3] + beacon()) +
            (recipe()["speed"] == 1.25) * assembler(),
        totalspeed3 =
            recipe()["speed"] * (
                1 + recipe()["slots"] * module()[["penalty"]][3] +
                input$beacons_on_machine * module()[["speed"]][3]
            ),
        totalprod3 = 1 + recipe()["slots"] * module()[["prod"]][3],
        seconds3 =
            totalcost3 *
            recipe()["time"] /
            recipe()["cost"] /
            totalspeed3 /
            (1 - 1 / totalprod3),

        totalcost2 =
            recipe()["slots"] * module()[["cost"]][2] +
            input$beacons_on_machine / input$machines_on_beacon * (2 * module()[["cost"]][2] + beacon()) +
            (recipe()["speed"] == 1.25) * assembler(),
        totalspeed2 =
            recipe()["speed"] * (
                1 + recipe()["slots"] * module()[["penalty"]][2] +
                    input$beacons_on_machine * module()[["speed"]][2]
            ),
        totalprod2 = 1 + recipe()["slots"] * module()[["prod"]][2],
        seconds2 =
            totalcost2 *
            recipe()["time"] /
            recipe()["cost"] /
            totalspeed2 /
            (1 - 1 / totalprod2),

        totalcost1 =
            recipe()["slots"] * module()[["cost"]][1] +
            input$beacons_on_machine / input$machines_on_beacon * (2 * module()[["cost"]][1] + beacon()) +
            (recipe()["speed"] == 1.25) * assembler(),
        totalspeed1 =
            recipe()["speed"] * (
                1 + recipe()["slots"] * module()[["penalty"]][1] +
                    input$beacons_on_machine * module()[["speed"]][1]
            ),
        totalprod1 = 1 + recipe()["slots"] * module()[["prod"]][1],
        seconds1 =
            totalcost1 *
            recipe()["time"] /
            recipe()["cost"] /
            totalspeed1 /
            (1 - 1 / totalprod1)
    ) |> reactive()

    tabledata <- data.frame(
        recipe = recipe()["recipe"],
        seconds3 = payoff()["seconds3"] |> unlist() |> as.integer(),
        seconds2 = payoff()["seconds2"] |> unlist() |> as.integer(),
        seconds1 = payoff()["seconds1"] |> unlist() |> as.integer(),
        tier3 = payoff()["seconds3"] |> unlist() |> as.integer() |> as.duration() |> as.period() |> tolower(),
        tier2 = payoff()["seconds2"] |> unlist() |> as.integer() |> as.duration() |> as.period() |> tolower(),
        tier1 = payoff()["seconds1"] |> unlist() |> as.integer() |> as.duration() |> as.period() |> tolower()
    ) |> reactive()

    output$table <- reactable(
        tabledata(),
        theme = cosmo(header_font_size = 20),
        pagination = FALSE, highlight = TRUE, sortable = FALSE,
        showSortable = TRUE, defaultSorted = "seconds3",
        columns = list(
            recipe = colDef(
                align = "left", filterable = TRUE,
                header = tagList(
                    span(style = "display: inline-block; width: 32px;",
                         img(src = "Recipe.png", width = 32, height = 32)
                    ),
                    span(style = "font-weight: bold; margin-left: 8px;", "Recipe")
                ),
                cell = function(item){
                    image <- img(src = sprintf("%s.png", item), width = 32, height = 32)
                    tagList(
                        span(style = "display: inline-block; width: 32px;", image),
                        span(style = "font-weight: bold; margin-left: 8px;", item)
                    )
                }
            ),
            tier3 = colDef(
                align = "center", vAlign = "center",
                cell = data_bars(
                    tabledata(), fill_by = "seconds3",
                    text_position = "above", bold_text = TRUE,
                    fill_color = "#ffa020", bar_height = 10, round_edges = TRUE,
                    border_style = "solid", border_width = "1px", border_color = "#00000040"
                ),
                header = tagList(
                    if(input$beacons_on_machine > 0){
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
            tier2 = colDef(
                align = "center", vAlign = "center",
                cell = data_bars(
                    tabledata(), fill_by = "seconds2",
                    text_position = "above", bold_text = TRUE,
                    fill_color = "#ff9010", bar_height = 10, round_edges = TRUE,
                    border_style = "solid", border_width = "1px", border_color = "#00000040"
                ),
                header = tagList(
                    if(input$beacons_on_machine > 0){
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
            tier1 = colDef(
                align = "center", vAlign = "center",
                cell = data_bars(
                    tabledata(), fill_by = "seconds1",
                    text_position = "above", bold_text = TRUE,
                    fill_color = "#ff8000", bar_height = 10, round_edges = TRUE,
                    border_style = "solid", border_width = "1px", border_color = "#00000040"
                ),
                header = tagList(
                    if(input$beacons_on_machine > 0){
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
            seconds3 = colDef(show = FALSE),
            seconds2 = colDef(show = FALSE),
            seconds1 = colDef(show = FALSE)
        )
    ) |> renderReactable()

} |> shinyServer()
