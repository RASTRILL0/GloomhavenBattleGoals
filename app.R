library(shiny)
library(shinyjs)
library(shinythemes)
library(tibble)
library(dplyr)
#`%then%` <- shiny:::`%OR%`

Draws <- function(Player, NumCards){
    Spots <- seq(Player,78,by=6)
    FinalDraws <- Spots[1:NumCards]
    return(FinalDraws)
}

#Deck of cards
Deck <- as_tibble(c("Streamliner", "Scrambler", "FastHealer", "Indigent", "Purist", "Neutralizer", "Sadist", "Straggler", "Hunter", "Pacifist", "Dynamo", "Professional", "Explorer", "Executioner", "Protector", "Hoarder", "Zealot", "Masochist", "Diehard", "Layabout", "Plunderer", "Aggressor", "Opener", "Workhorse", 
        #Satire's Extended Battle Goals:
        "Acrobatic", "Ambusher", "Assassin", "Assistant", "Bastion", "Bully", "Contagious", "Covetous", "Cuddler", "Discerning", "Distracted", "Drowsy", "Elitist", "Exterminator", "Fearful", "Feeble", "Feral", "Finisher", "Hesitant", "Hothead", "Instigator", "Insulting", "Limping", "Lucky", "Marksman", "Miser", "Mugger", "Multitasker", "Paranoid", "Peacemonger", "Perforated", "Pickpocket", "Pincushion", "Prosperous", "Ravager", "Recluse", "Reserved", "Restless", "Retaliator", "Ritualistic", "Scavenger", "Shadow", "Sharpshooter", "Slayer", "Sleepless", "Sober", "Sociable", "Specialized", "Stalwart", "Stubborn", "Thorough", "Untouchable", "Wasteful", "Winded",
        #Jaws of the Lion Battle Goals:
        "acrobat_JotL","agoraphobe_JotL", "altruist_JotL","assistant_JotL","challenger_JotL","closer_JotL","conservator_JotL","diehard_JotL","egoist_JotL","fast-healer_JotL","gambler_JotL","hothead_JotL","insomniac_JotL","masochist_JotL","mugger_JotL","opener_JotL","pacifist_JotL","pickpocket_JotL","pincushion_JotL","plebeian_JotL","prohibitionist_JotL","ravager_JotL","recluse_JotL","sadist_JotL","scrambler_JotL","shadow_JotL","shirker_JotL","specialist_JotL","straggler_JotL","trailblazer_JotL","transmitter_JotL","weakling_JotL",
        #Gloomhaven Battle Goals that can be added to JotL:
        "Streamliner","Indigent","Purist","Neutralizer","Hunter","Dynamo","Professional","Explorer","Executioner","Protector","Hoarder","Zealot","Layabout","Plunderer","Aggressor","Workhorse"
        )
        )

ui <- function(request) {
    fluidPage(theme = shinytheme("flatly"),
#        tags$head(includeScript("google-analytics.js")),
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
            need(is.numeric(input$Seed), "Seed value must be a number."),
            #%then%
                need(input$Seed <= 2147483647 && input$Seed >= -2147483647, "Seed value must be between -2147483647 and 2147483647.")
            )
    })
    output$Error <- renderPrint(ErrorMsg()) 
    
    
    # Call renderImage for each one. Cards are only actually generated when they
    # are visible on the web page.
    observeEvent(input$button, {
        toggle('ShowDraw')
        for (i in 1:6) {
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderImage() will be the same across all instances, because
            # of when the expression is evaluated.
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({validate(
                    need(is.numeric(input$Seed), "") ,
                    #%then%
                        need(input$Seed <= 2147483647 && input$Seed >= -2147483647, "")
                )
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
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
            filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(2:4)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals/battlegoal-back.png'))
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
            filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1, 3, 4)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals/battlegoal-back.png'))
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
            filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1,2,4)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals/battlegoal-back.png'))
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
            filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1:3)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals/battlegoal-back.png'))
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
            filename <- normalizePath(file.path('/srv/shiny-server/battle-goals',
                                                paste(tolower(DrawnCards()[my_i,1]), '.png', sep='')))
            list(src = filename)
        }, deleteFile = FALSE)
        for (i in c(1:4)) {
            local({
                my_i <- i
                cardnum <- paste("card", my_i, sep="")
                output[[cardnum]] <- renderImage({
                    filename <- normalizePath(file.path('/srv/shiny-server/battle-goals/battlegoal-back.png'))
                    # Return a list containing the filename and alt text
                    list(src = filename)
                }, deleteFile = FALSE)
            })
        }
    })
    
    Shuffle <- reactive({
        set.seed(input$Seed)
        if(input$Game == "Gloomhaven" & input$Extended == TRUE){
            Random <- sample(1:78)
        }
        if(input$Game == "Gloomhaven" & input$Extended == FALSE){
            Random <- sample(1:24)
        }
        if(input$Game == "Jaws of the Lion" & input$Extended == TRUE){
          #This skips the "Specialized" card from Extended because it's duplicated in JotL
            Random <- sample(c(25:71,73:110))
        }
        if(input$Game == "Jaws of the Lion" & input$Extended == FALSE){
            Random <- sample(79:110)
        }
        if(input$Game == "Gloomhaven with Jaws of the Lions goals" & input$Extended == FALSE){
            Random <- sample(79:126)
        }
        if(input$Game == "Gloomhaven with Jaws of the Lions goals" & input$Extended == TRUE){
          #This skips the "Specialized" card from Extended because it's duplicated in JotL
            Random <- sample(c(25:71,73:126))
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
