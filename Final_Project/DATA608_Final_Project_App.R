#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
#library(ggimage)
library(plotly)
#library(magick)
library(rsconnect)
#library(sjmisc)
library(markdown)


# Data Source
# https://www.moneypuck.com/teams.htm
# https://www.moneypuck.com/data.htm

all.data.select <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/all.data.select.csv")
all.data.playoff.select <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/all.data.playoff.select.csv")



selected.vars.reg <- c("team", "season", "name", "situation", "games_played", "playoffFlag", "division", "xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "savePercentage")
selected.vars.playoff <- c("team", "season", "name", "situation", "games_played", "distance", "division", "xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "savePercentage")


var.names1 <- c("xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "allDangerShotsFor", "savePercentage")
var.names <- names(all.data.select)

t <- list(size = 12)


# function to draw horizontal line
hline <- function(y = 0, color = "black") {
    list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color)
    )
}




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(theme = shinytheme("darkly"),
               "NHL Stats Analysis: 2017 - 2022",
               # tab containing project information
               tabPanel("Information",
                        fluidPage(
                            includeMarkdown("Information.rmd")
                        )), 
               # tab containing main Regular season data
               tabPanel("Regular Season", 
                        fluidPage(
                            
                            fluidRow(
                                # Set up Season Selection Button
                                column(12, align =  "center",
                                       h2("Input for Both Plots"),
                                       radioButtons(inputId = "Season", 
                                                    label = "Select Season",
                                                    choices = unique(all.data.select$season),
                                                    selected = "2017",
                                                    inline = TRUE),
                                       #column(6, align = "center",
                                       # Set up Situation Selection Button
                                       radioButtons(inputId = "Situation", 
                                                    label = "Select Situation",
                                                    choices = unique(all.data.select$situation),
                                                    selected = "all", 
                                                    inline = TRUE)
                                )),
                            
                            fluidRow(
                                column(2,
                                       h2("Left Plot Input"),
                                       # Set up Season Selection Button
                                       # selectInput(inputId = "Season", 
                                       #             label = "Select Season",
                                       #             choices = all.data$season,
                                       #             selected = "2017"),
                                       # # Set up Situation Selection Button
                                       # selectInput(inputId = "Situation", 
                                       #             label = "Select Situation",
                                       #             choices = all.data$situation,
                                       #             selected = "all"),
                                       # Set up Variable Selection Button
                                       selectInput(inputId = "y_var1", 
                                                   label = "Select Variable",
                                                   choices = var.names1,
                                                   selected = "xGoalsPercentage")
                                       
                                ), column(4,
                                          plotlyOutput("regPlot1", height = 800)
                                ), column(4,
                                          plotlyOutput("regPlot2", height = 800)
                                ), column(2,
                                          h2("Right Plot Input"),
                                          # Set up Plot 2 Variable x Selection Button
                                          selectInput(inputId = "x_var2", 
                                                      label = "Select X Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage"),
                                          # Set up Plot 2 Variable y Selection Button
                                          selectInput(inputId = "y_var2", 
                                                      label = "Select Y Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage")) 
                                
                            )
                        )
                        
               ),
               # tab containing Regular season Team data
               tabPanel("Regular Season: Timeseries", 
                        fluidPage(
                            column(12, align =  "center",
                                   h2("Input"),
                                   
                                   selectInput(inputId = "team", 
                                               label = "Select Team",
                                               choices = unique(all.data.select$team),
                                               selected = "NJD"),
                                   # Set up Situation Selection Button
                                   radioButtons(inputId = "Situation2", 
                                                label = "Select Situation",
                                                choices = unique(all.data.select$situation),
                                                selected = "all", 
                                                inline = TRUE),
                                   selectInput(inputId = "y_var3", 
                                               label = "Select Variable",
                                               choices = var.names1,
                                               selected = "xGoalsPercentage")
                            ),
                            fluidRow(column(12, align =  "center",
                                            plotlyOutput("regPlot3", height = 700)
                            )))),
               # tab containing Playoff data
               tabPanel("Playoffs", 
                        fluidPage(
                            
                            fluidRow(
                                # Set up Season Selection Button
                                column(12, align =  "center",
                                       h2("Input for Both Plots"),
                                       radioButtons(inputId = "Season3", 
                                                    label = "Select Season",
                                                    choices = unique(all.data.select$season),
                                                    selected = "2017",
                                                    inline = TRUE),
                                       #column(6, align = "center",
                                       # Set up Situation Selection Button
                                       radioButtons(inputId = "Situation3", 
                                                    label = "Select Situation",
                                                    choices = unique(all.data.select$situation),
                                                    selected = "all", 
                                                    inline = TRUE)
                                )),
                            
                            fluidRow(
                                column(2,
                                       h2("Left Plot Input"),
                                       # Set up Season Selection Button
                                       # selectInput(inputId = "Season", 
                                       #             label = "Select Season",
                                       #             choices = all.data$season,
                                       #             selected = "2017"),
                                       # # Set up Situation Selection Button
                                       # selectInput(inputId = "Situation", 
                                       #             label = "Select Situation",
                                       #             choices = all.data$situation,
                                       #             selected = "all"),
                                       # Set up Variable Selection Button
                                       selectInput(inputId = "y_var4", 
                                                   label = "Select Variable",
                                                   choices = var.names1,
                                                   selected = "xGoalsPercentage")
                                       
                                ), column(4,
                                          plotlyOutput("playoffPlot1", height = 800)
                                ), column(4,
                                          plotlyOutput("playoffPlot2", height = 800)
                                ), column(2,
                                          h2("Right Plot Input"),
                                          # Set up Plot 2 Variable x Selection Button
                                          selectInput(inputId = "x_var5", 
                                                      label = "Select X Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage"),
                                          # Set up Plot 2 Variable y Selection Button
                                          selectInput(inputId = "y_var5", 
                                                      label = "Select Y Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage")) 
                                
                            )
                        )
                        
               ),
               # tab containing takeaways
               tabPanel("Conclusion", 
                        fluidPage(
                            includeMarkdown("Conclusion.rmd")
                        )
               ),
               
               
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reg season plot 1
    
    # Make data reactive
    all_data_reactive <- reactive(all.data.select %>%
                                      filter(season == input$Season) %>%
                                      filter(situation == input$Situation))
    
    historic.avg_reactive <- reactive(all.data.select %>%
                                          filter(situation == input$Situation)) 
    
    historic.avg.playoff_reactive <- reactive(all.data.playoff.select %>%
                                                  filter(situation == input$Situation3)) 
    
    line.data_reactive <- reactive(all.data.select %>%
                                       filter(team == input$team) %>%
                                       filter(situation == input$Situation2))
    
    all.data.playoff_reactive <- reactive(all.data.playoff.select %>%
                                              filter(season == input$Season3) %>%
                                              filter(situation == input$Situation3))
    
    # Create plotly graph
    output$regPlot1 <- renderPlotly({
        plot_ly(
            data = all_data_reactive(),
            x = ~team,
            y = as.formula(paste0('~', input$y_var1)),
            type = "bar",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A")
        ) %>%
            layout(title = paste0('Seasonal ', input$y_var1, ' Compared to NHL Average from 2017 to 2022'), font = t,
                   shapes = list(hline(mean(historic.avg_reactive()[,var.names == input$y_var1]), "red")),
                   xaxis = list(categoryorder = "total ascending"))
    })
    
    
    # reg season  plot 2
    output$regPlot2 <- renderPlotly({
        plot_ly(
            data = all_data_reactive(),
            x = as.formula(paste0('~', input$x_var2)),
            y = as.formula(paste0('~', input$y_var2)),
            type = "scatter",
            mode = "markers",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A"),
            size = 10
        )%>%
            layout(title = paste0('NHL Stats: ', input$x_var2, " vs. ", input$y_var2), font = t)
    })
    
    
    # reg season plot 3
    output$regPlot3 <- renderPlotly({
        plot_ly(
            data = line.data_reactive(),
            x = ~season,
            y = as.formula(paste0('~', input$y_var3)),
            type = "scatter",
            mode = "markers",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A"),
            size = 10
        ) %>%
            add_trace(data = line.data_reactive(),
                      x = ~season,
                      y = as.formula(paste0('~', input$y_var3)),
                      type = "scatter",
                      mode = 'lines', color=I('black'),
                      line = list(width = 2),
                      opacity = 0.35,
                      showlegend = F) %>%
            layout(title = paste0('', input$team, ' ', input$y_var3, ' Over the Last 5 Seasons'), font = t)
    })
    
    
    
    
    # Create playoff graph 1
    output$playoffPlot1 <- renderPlotly({
        plot_ly(
            data = all.data.playoff_reactive(),
            x = ~team,
            y = as.formula(paste0('~', input$y_var4)),
            type = "bar",
            color = ~distance,
            colors = c('#FF0000', "#FFA600", "#0A720F", "#000000", "#0045D8", "#CA0071"),
            size = 3
        ) %>%
            layout(title = paste0('Seasonal Playoff ', input$y_var4, ' Compared to NHL Average from 2017 to 2022'), font = t,
                   shapes = list(hline(mean(historic.avg.playoff_reactive()[,var.names == input$y_var4]), "red")),
                   xaxis = list(categoryorder = "total ascending"))
    })
    
    
    # playoff plot 2
    output$playoffPlot2 <- renderPlotly({
        plot_ly(
            data = all.data.playoff_reactive(),
            x = as.formula(paste0('~', input$x_var5)),
            y = as.formula(paste0('~', input$y_var5)),
            type = "scatter",
            mode = "markers",
            color = ~distance,
            colors = c('#FF0000', "#FFA600", "#0A720F", "#000000", "#0045D8", "#CA0071"),
            symbol = ~division,
            size = 3
        )%>%
            layout(title = paste0('NHL Playoff Stats: ', input$x_var5, " vs. ", input$y_var5), font = t)
    })
    
}





# Run the application 
shinyApp(ui = ui, server = server)








# https://www.moneypuck.com/teams.htm
# https://www.moneypuck.com/data.htm