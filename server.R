library(shiny)
library(shinyjs)
library(shinythemes)
library(tibble)
library(dplyr)

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
        for (i in c(2:4)) {
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
        for (i in c(1, 3, 4)) {
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
        for (i in c(1,2,4)) {
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
        for (i in c(1:3)) {
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

