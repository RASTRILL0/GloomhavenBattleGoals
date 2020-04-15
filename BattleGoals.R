library(shiny)
library(shinyjs)
library(shinythemes)
library(tibble)
library(dplyr)
`%then%` <- shiny:::`%OR%`

Draws <- function(Player, NumCards){
    Spots <- seq(Player,78,by=6)
    FinalDraws <- Spots[1:NumCards]
    return(FinalDraws)
}

#Deck of cards
Deck <- as.tibble(c("Streamliner", "Scrambler", "FastHealer", "Indigent", "Purist", "Neutralizer", "Sadist", "Straggler", "Hunter", "Pacifist", "Dynamo", "Professional", "Explorer", "Executioner", "Protector", "Hoarder", "Zealot", "Masochist", "Diehard", "Layabout", "Plunderer", "Aggressor", "Opener", "Workhorse", 
        #Satire's Extended Battle Goals:
        "Acrobatic", "Ambusher", "Assassin", "Assistant", "Bastion", "Bully", "Contagious", "Covetous", "Cuddler", "Discerning", "Distracted", "Drowsy", "Elitist", "Exterminator", "Fearful", "Feeble", "Feral", "Finisher", "Hesitant", "Hothead", "Instigator", "Insulting", "Limping", "Lucky", "Marksman", "Miser", "Muggar", "Multitasker", "Paranoid", "Peacemonger", "Perforated", "Pickpocket", "Pincushion", "Prosperous", "Ravager", "Recluse", "Reserved", "Restless", "Retaliator", "Ritualistic", "Scavenger", "Shadow", "Sharpshooter", "Slayer", "Sleepless", "Sober", "Sociable", "Specialized", "Stalwart", "Stubborn", "Thorough", "Untouchable", "Wasteful", "Winded")
        )

ui <- function(request) {
    fluidPage(theme = shinytheme("flatly"),
        tags$head(includeScript("google-analytics.js")),
        h1("Gloomhaven Battle Goals", align = "center"),
        sidebarLayout(
            sidebarPanel(
                numericInput("Seed", "Seed", 1, step=1),
                helpText("Each player in your party must enter the same seed number. Enter a new seed number for each scenario."),
                sliderInput("Player","What player are you (default 1-4):", min = 1, max = 6, value = 1, ticks = FALSE ),
                sliderInput("NumCards", "Number of Cards (default 2):", min = 1, max = 4, value = 2, ticks = FALSE),
                checkboxInput("Extended", "Include Satire's Extended Battle Goals", value = FALSE),
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
                "Gloomhaven and all related properties and images are owned by", tags$a(href="http://www.cephalofair.com", "Cephalofair Games."), align = "center", br(),
                "Card scans from", tags$a(href="https://github.com/any2cards/gloomhaven", "any2cards/gloomhaven"), "and", tags$a(href="https://boardgamegeek.com/thread/2184131/satires-extended-battle-goals", "Satire's Extended Battle Goals."), br(),
                "Feedback can be submitted", tags$a(href="https://boardgamegeek.com/thread/2393488/battle-goal-card-application-intended-remote-play", "here."), br()
            )
        )
    )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Modified from here: https://gist.github.com/wch/5436415/
    # Insert the right number of card output objects into the web page
    output$cards <- renderUI({
        card_output_list <- lapply(1:input$NumCards, function(i) {
            cardnum <- paste("card", i, sep="")
            imageOutput(cardnum,inline = TRUE, click = paste("card", "click", i, sep=""))
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, card_output_list)
    })
    #Validate seed input and generate appropriate error message
    ErrorMsg <- reactive({
        validate(
            need(is.numeric(input$Seed), "Please input a number.") %then%
                need(input$Seed <= 2147483647 && input$Seed >= -2147483647, "Seed value must be between -2147483647 and 2147483647.")
            )
    })
    output$Error <- renderPrint(ErrorMsg()) 
    
    
    # Call renderImage for each one. Cards are only actually generated when they
    # are visible on the web page.
    observeEvent(input$button, {
        toggle('ShowDraw')
        for (i in 1:10) {
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderImage() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({validate(
                    need(is.numeric(input$Seed), "") %then%
                        need(input$Seed <= 2147483647 && input$Seed >= -2147483647, "")
                )
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                        paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
                    
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    observeEvent(input$cardclick1,{
        my_i <- 1
        cardnum <- paste("card", my_i, sep="")
        output[[cardnum]] <- renderImage({
            filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(2:5)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    observeEvent(input$cardclick2,{
        my_i <- 2
        cardnum <- paste("card", my_i, sep="")
        output[[cardnum]] <- renderImage({
            filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1, 3:5)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })  
    observeEvent(input$cardclick3,{
        my_i <- 3
        cardnum <- paste("card", my_i, sep="")
        output[[cardnum]] <- renderImage({
            filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1:2,4:5)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    observeEvent(input$cardclick4,{
        my_i <- 4
        cardnum <- paste("card", my_i, sep="")
        output[[cardnum]] <- renderImage({
            filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1:3, 5)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    observeEvent(input$cardclick5,{
        my_i <- 5
        cardnum <- paste("card", my_i, sep="")
        output[[cardnum]] <- renderImage({
            filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1:4)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/apps/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    
    Shuffle <- reactive({
        set.seed(input$Seed)
        if(input$Extended == TRUE){
            Random <- sample(1:78)
        }
        if(input$Extended == FALSE){
            Random <- sample(1:24)
        }
        Random
    })
    
    DrawnCards <- reactive({
        CardDraw <- Draws(Player = input$Player, NumCards = input$NumCards)
        slice(Deck, Shuffle()[CardDraw])
    })

    setBookmarkExclude(c("button", "cardclick1", "cardclick2", "cardclick3","cardclick4","cardclick5"))
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = 'url')
