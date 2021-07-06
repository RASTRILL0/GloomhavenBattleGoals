library(shinythemes)
library(shiny)
library(shinyjs)
library(tibble)
library(dplyr)

ui <- function(request) {
    fluidPage(theme = shinytheme("flatly"),
        h1("Gloomhaven Battle Goals", align = "center"),
        sidebarLayout(
            sidebarPanel(
                numericInput("Seed", "Seed", 1, step=1),
                helpText("Each player in your party must enter the same seed number. Enter a new seed number for each scenario."),
                radioButtons("Game", "Which game are you playing:", choices = c("Gloomhaven", "Jaws of the Lion","Gloomhaven with Jaws of the Lions goals"), selected = "Gloomhaven"),
                checkboxInput("Extended", "Include Satire's Extended Battle Goals", value = FALSE),
                sliderInput("Player","What player are you (default 1-4):", min = 1, max = 6, value = 1, ticks = FALSE ),
                sliderInput("NumCards", "Number of Cards (default 2):", min = 1, max = 4, value = 2, ticks = FALSE),
                bookmarkButton(label = "Invite Party", align = "center"),
                actionButton("button", "Draw!", align = "center")
            ),
            mainPanel(
                textOutput("Error"),
                hidden(
                    div(id='ShowDraw',
                        uiOutput("cards")
                    )
                ),
                br(),
                span("New:", style="color:red"), "Updated with Jaws of the Lion battle goals.",br(),
                "Gloomhaven and all related properties and images are owned by", tags$a(href="http://www.cephalofair.com", "Cephalofair Games."), align = "center", br(),
                "Card scans from", tags$a(href="https://github.com/any2cards/gloomhaven", "any2cards/gloomhaven"), "and", tags$a(href="https://boardgamegeek.com/thread/2184131/satires-extended-battle-goals", "Satire's Extended Battle Goals."), br(),
                "Use Discord to play?", tags$a(href="https://old.reddit.com/r/Gloomhaven/comments/fmpavk/if_anybody_else_is_struggling_with_secretly/fnhftv4/", "Try the Discord Bot created by nicholaskillin."), br(),
                "Feedback can be submitted", tags$a(href="https://boardgamegeek.com/thread/2393488/battle-goal-card-application-intended-remote-play", "here."), br()
        
        )
    )
}
