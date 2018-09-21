library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
#library(cowplot)
library(dplyr)
library(tidyr)
library(openxlsx)
library(reshape2)

shinyServer(function(input, output, session) {
  
  
  load("base/BaseRes.Rata")

  
  rv <- reactiveValues(env.scenario = NULL, 
                       finalFormulaSB = NULL, 
                       finalFormulaTB = NULL,
                       savedScenario = list(),
                       message = "Choose a Variable to Examine.", 
                       currSB = "NULL", 
                       tableResult = list(),
                       messageList = "")
  
  #############################################################################################
  #Sorting out the Variables

  allVar <- c(names(baseRes), names(timeInvRes))[-c(23,24)]
  
  
  catvars <- allVar[!grepl("_con", allVar)]
  
  
  contvars <- allVar[grepl("_con", allVar)]
  
  
  subGrpVar <- c("Age Group", "Gender",  "Maori", "Pacific", "Asian",
                 "NZ European/Other")
  
  varnames <- read.csv("Varnames.csv", stringsAsFactors = FALSE)
 
  varname.vec <- varnames$FullName
  names(varname.vec) <- varnames$allVar
  
  
  
  #############################################################################################
  
  textareaInput <- function(inputId, label, value="", placeholder="", rows=2){
    tagList(
      div(strong(label), style="margin-top: 5px;"),
      tags$style(type="text/css", "textarea {width:100%; margin-top: 5px;}"),
      tags$textarea(id = inputId, placeholder = placeholder, rows = rows, value))
  }
  
  output$uiTB <- renderUI({
    
    temp <- HTML("<b> <font size=\"4\">STEP 2: </font></b> Choose variable:")

    switch(req(input$input_type_TB),
           
           "Percentage" = selectInput("dynamicTB", temp, 
                                      choices = as.character(varname.vec[catvars]), 
                                      selected = as.character(varname.vec[catvars])[1]),
           "Count" = selectInput("dynamicTB", temp, 
                                      choices = as.character(varname.vec[catvars]), 
                                      selected = as.character(varname.vec[catvars])[1]),
           "Mean" = selectInput("dynamicTB", temp, 
                                choices =  as.character(varname.vec[contvars]),
                                selected = as.character(varname.vec[contvars])[1])
           )
  })
  
  output$uiSubGrpTB <- renderUI({
    

    if(any(tolower(varnames$FullName[varnames$allVar %in%
                                     names(timeInvRes)]) %in% 
           tolower(input$dynamicTB)))
      selectInput("subGrp_TB",
                  HTML("<b> <font size=\"4\">STEP 3 (optional): </font></b> Select ByGroup:"), 
                  choices ='None')
    else 
      selectInput("subGrp_TB",
                HTML("<b> <font size=\"4\">STEP 3 (optional): </font></b> Select ByGroup:"), 
                choices = c(None='None',  subGrpVar))
  })
  

  output$uiExprTB1 <- renderUI({
    
    #print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
    if(req(input$subGrp_TB) == "None"){
      return()
    } else {

      choice <- unique(timeInvRes[[
        varnames$allVar[tolower(varnames$FullName) ==
                          tolower(input$subGrp_TB)]]][["Var"]])
    }
   
    if(input$subGrp_TB == "Age Group" & 
       input$dynamicTB %in% varnames$FullName[c(12, 15, 21, 22)]) 
      choice <- 
        c(
          "Childhood (0-14)",
          "Teenage Years (15-19)",
          "Young Adulthood (20-24)",
          "Middle Adulthood (25-34)" ,
          "Later Adulthood (35-54)" ,
          "Older Life Working (55-64)",
          "Older Life Retired (65-74)",
          "Later Life (75+)"
        )
    else if (input$subGrp_TB == "Age Group")
      choice <- 
      c(
        "Teenage Years (15-19)",
        "Young Adulthood (20-24)",
        "Middle Adulthood (25-34)" ,
        "Later Adulthood (35-54)" ,
        "Older Life Working (55-64)",
        "Older Life Retired (65-74)",
        "Later Life (75+)"
      )
    
    isolate(
        selectInput("subGrp_TB2", input$subGrp_TB,
                    choices = c("None", choice), 
                    selected = "None", 
                    selectize=FALSE)
    )
  })
  # Tables starts here ####
  
  baseTB <<- NULL 
  
  
  summaryOutputTB <- reactive( { 
    
    inputType = c("frequencies", "count", "means")
    
    names(inputType) = c("Percentage","Count", "Mean")
    
    
    selectVar <- names(varname.vec) [varname.vec %in% req(input$dynamicTB)]

    results <- 
    if(input$subGrp_TB == "None"){
      
      if(input$dynamicTB == "Age group"){
        temp <- timeInvRes$age_cat
        
        temp$Var[temp$Var ==
                   "Early Childhood (0-14)"] <-
          "Childhood (0-14)"
        
        temp$Var <- factor(temp$Var, 
                           levels =
                             c(
                               "Childhood (0-14)",
                               "Teenage Years (15-19)",
                               "Young Adulthood (20-24)",
                               "Middle Adulthood (25-34)" ,
                               "Later Adulthood (35-54)" ,
                               "Older Life Working (55-64)",
                               "Older Life Retired (65-74)",
                               "Later Life (75+)"
                             ))
        
        temp
      } else if(selectVar == "birthreg"){
        
        temp <- timeInvRes$birthreg
        
        temp$Var <- factor(temp$Var)
        
        levels(temp$Var) <- c("NZ", "Pacific", "Asia", "Europe", 
                              "Americas", "Middle East & Africa")
        
        temp
      } else {
        
        c(baseRes, timeInvRes)[[selectVar]]
      }
      
    } else if(input$subGrp_TB == "Age Group"){

      temp <- byAgeRes[[selectVar]]
      
      temp$groupByData[temp$groupByData ==
                         "Early Childhood (0-14)"] <-
        "Childhood (0-14)"
      
      temp$groupByData <-
        factor(
          temp$groupByData,
          levels =
            c(
              "Childhood (0-14)",
              "Teenage Years (15-19)",
              "Young Adulthood (20-24)",
              "Middle Adulthood (25-34)" ,
              "Later Adulthood (35-54)" ,
              "Older Life Working (55-64)",
              "Older Life Retired (65-74)",
              "Later Life (75+)"
            )
        ) 
      
      
      temp
    }  else if(input$subGrp_TB == "Gender"){
    
      temp <- bySexRes[[selectVar]]
      
      temp
    }  else if(input$subGrp_TB == "Maori"){
      
      byMaoriRes[[selectVar]]
    }   else if(input$subGrp_TB == "Pacific"){
      
      byPacificRes[[selectVar]]
    } else if(input$subGrp_TB == "Asian"){
      
      byAsianRes[[selectVar]]
    }  else if(input$subGrp_TB == "NZ European/Other"){
      
      byEuroRes[[selectVar]]
    }  

    if(selectVar == "dep_curr"){
      
      results$Var <- gsub("quantile", "quintile", results$Var)
      
      
      results$Var <- factor(results$Var ,
                            levels =  c( "1st quintile",
                                         "2nd quintile","Median", "3rd quintile",
                                         "4th quintile"))
      
      
      levels(results$Var)<- c("1st quintile (least)",
                                  "2nd quintile", 
                                  "3rd quintile", 
                                  "4th quintile", 
                                  "5th quintile")
      
    } else if(selectVar == "emp_curr"){
      
      results$Var <- factor(results$Var, levels = 
                              c( "Full-/Part-time employed", 
                                 "Unemployed/Unpaid", 
                                 "Not in labour force"))
      
    } else if(selectVar == "religion_curr"){
      
      results$Var <- factor(results$Var, levels =
                              c("No region",  "Christian", "Other"))
      
      levels(results$Var)[1] <- "No religion"
      
      
    } else if(selectVar == "education_curr"){
      
      results$Var <- factor(results$Var, levels =
                              c("No Qualification", 
                                "Secondary School Qualification",
                                "Post-school non-university", 
                                "University Qualification" ))
    } else if(selectVar == "h_income_curr_cat" | 
              selectVar == "p_income_curr_cat"){
      
      
      results$Var <- factor(results$Var, levels =
                              c(
                                "Zero or loss" ,
                                "$1-$10,000",
                                "$10,001-$20,000",
                                "$20,001-$30,000",
                                "$30,001-$40,000",
                                "$40,001-$50,000",
                                "$50,001-$60,000",
                                "$60,001-$70,000",
                                "$70,001-$100,000",
                                "$100,001 or More"
                              ) )
      
    } else if(selectVar == "partner_curr"){
      results$Var <- factor(results$Var, levels =
                              c(
                                "not living with a partner" ,
                                "living with a partner"
                              ) )
    } else if (input$input_type_TB != "Mean") {

      results$Var <- factor(results$Var)
    }
    

    
    if(input$subGrp_TB != "None" &
       !is.null(input$subGrp_TB2)) {
      if (input$subGrp_TB2 != "None") {
        results <- results %>%
          filter(groupByData == req(input$subGrp_TB2))
      } else {
        results <-  results
      }
    }

    if(input$input_type_TB == "Count")
      results <- results %>% 
            select(-Mean, -Lower, -Upper) %>%
            rename(Mean = MeanCount, 
                   Lower = LowerCount, 
                   Upper = UpperCount)
    
    
    if(input$input_type_TB == "Percentage")
      results <- results %>% 
      select(-MeanCount, -LowerCount, -UpperCount)
    
    
    
    baseTB <<- results 
    
    results
  })
  
  output$uiVar <- renderUI({
    # if(length(unique(summaryOutputTB()$Year))!=1)
   
    
    varlist <- levels(req(summaryOutputTB())$Var)
    
    # If it is a binary outcome, pick the second group as default
    if(length(varlist) == 2){
      selectInput("Var_TB", "Select a level to compare in plot:",  
                  selected =varlist[2], 
                  choices = varlist)
    } else {
      selectInput("Var_TB", "Select a level to compare in plot:",  
                  selected =varlist[1], 
                  choices = varlist)
    }
  })
  
  output$resultTB  <- DT::renderDataTable({
    
    results <- req(summaryOutputTB())
    
    
   if((input$input_type_TB == "Percentage" | input$input_type_TB == "Count") & 
      "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
                       Year~groupByData + Var + variable)
      
    }else if((input$input_type_TB == "Percentage" | input$input_type_TB == "Count")){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "Year")), 
                       Year~Var + variable)
      
    } else if(input$input_type_TB ==  "Mean" & 
              "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
                       Year ~ groupByData + variable)
      
    } else if(input$input_type_TB %in% c("Mean", "Quantile") &
              "Var" %in% names(results)){
      return(NULL)
    }    
    
    
    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
    
    notToRound <- c("<span style=\"font-size:20px\">Year</span>", 
                    "<span style=\"font-size:20px\">groupByData</span>", 
                    "<span style=\"font-size:20px\">Var</span>")
    
    if(!input$ci)
      results <- results[,-index]
    
    if(input$input_type_TB == "Percentage")
      colnames(results) <- gsub("Mean", "Percent", colnames(results))
    
    rv$tableResult$Base <- results
    
    colnames(results) <- 
      paste0('<span style="font-size:20px">',colnames(results),'</span>')
    
    tables<-
      DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
                  options = list(pageLength = 9999999, dom = 't',
                                 scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                                 scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') 
    
    
    if(input$input_type_TB == "Percentage")
      tables %>% formatRound(which(!colnames(results) %in% notToRound), digits = 1)
    else 
      tables %>% formatRound(which(!colnames(results) %in% notToRound), digits = 0)
    
  })
  
  
  output$selectSB <- renderUI({
    
    
    selectInput("selSB", "Select Scenario for comparison:",
                choices =c("allvars_91-06_86", 
                            # "allvars_M-like-NM",
                          "emp_86-91_81",
                           "emp_96-01_91",
                           "birthreg_86-06_81",
                           "hinc_wchild_01-06_96",
                           "emp_fem-wchild_01-06_96",
                           "emp_fem-35-54_01_81",
                           "emp_fem-15-34_81_01",
                           "hous_35-54_01_81",
                           "hous_15-34_81_01",
                           "educ_81-96_01"),
                selectize=TRUE)
  })
  
  
  scenDes <- read.csv("scenDes.csv")
  
  
  output$scenDes <- renderDataTable({ 

    res <- data.frame(t(scenDes[scenDes$Name == input$selSB, -1]))

    res[,1] <- as.character(res[,1])
    
    res[1,1] <- paste0("<b> Question: </b>", res[1,1])
    res[2,1] <- paste0("<b>Description: </b>", res[2,1])

    colnames(res) <- input$selSB
    
    datatable(res, options = list(dom = 't'), rownames = NULL, escape = FALSE)
    
    })
  
  
  # Scenario tables #####
  SBTB <<- NULL


  summaryOutputSBTB <- reactive({
    
    
    load(paste0("base/", input$selSB, ".Rata"))
 
    
    inputType = c("frequencies", "means", "count")
    
    names(inputType) = c("Percentage", "Mean","Count" )
    
    selectVar <- names(varname.vec) [varname.vec %in% req(input$dynamicTB)]
  
    results <- 
      if(input$subGrp_TB == "None"){
          
          if(input$dynamicTB == "Age group"){
            temp <- scenTimeInvRes$age_cat
            
            temp$Var[temp$Var ==
                       "Early Childhood (0-14)"] <-
              "Childhood (0-14)"
            
            temp$Var <- factor(temp$Var, 
                               levels =
                                 c("Childhood (0-14)",
                                   "Teenage Years (15-19)",
                                   "Young Adulthood (20-24)",
                                   "Middle Adulthood (25-34)" ,
                                   "Later Adulthood (35-54)" ,
                                   "Older Life Working (55-64)",
                                   "Older Life Retired (65-74)",
                                   "Later Life (75+)" ))
            
            temp
          } else if(selectVar == "birthreg"){
            
            temp <- scenTimeInvRes$birthreg
            
            temp$Var <- factor(temp$Var)
            
            levels(temp$Var) <- c("NZ", "Pacific", "Asia", "Europe", 
                                  "Americas", "Middle East & Africa")
            
            temp
          } else {
            
            c(scenRes, scenTimeInvRes)[[selectVar]]
          }
        
      } else if(input$subGrp_TB == "Age Group"){
     
        
        # if(selectVar == "birthreg"){
        #   
        #   temp <- scenTimeInvRes$birthregByAge
        #   
        #   temp$Var <- factor(temp$Var)
        #   
        #   levels(temp$Var) <- c("NZ", "Pacific", "Asia", "Europe", 
        #                         "Americas", "Middle East & Africa")
        # } else {
        #   
        #   temp <- scenByAgeRes[[selectVar]]
        # }
        
        
        temp <- scenByAgeRes[[selectVar]]
        
        temp$groupByData[temp$groupByData ==
                           "Early Childhood (0-14)"] <-
          "Childhood (0-14)"
        
        temp$groupByData <-
          factor(
            temp$groupByData,
            levels =
              c(
                "Childhood (0-14)",
                "Teenage Years (15-19)",
                "Young Adulthood (20-24)",
                "Middle Adulthood (25-34)" ,
                "Later Adulthood (35-54)" ,
                "Older Life Working (55-64)",
                "Older Life Retired (65-74)",
                "Later Life (75+)"
              )
          )
        
        temp
      }  else if(input$subGrp_TB == "Gender"){
        
        # if(selectVar == "birthreg"){
        #   
        #   temp <- scenTimeInvRes$birthregBySex
        #   
        #   temp$Var <- factor(temp$Var)
        #   
        #   levels(temp$Var) <- c("NZ", "Pacific", "Asia", "Europe", 
        #                         "Americas", "Middle East & Africa")
        # } else {
        #   
        #   temp <- scenBySexRes[[selectVar]]
        # }
        
        temp <- scenBySexRes[[selectVar]]
        
        temp
      }  else if(input$subGrp_TB == "Maori"){
        
        scenByMaoriRes[[selectVar]]
      }   else if(input$subGrp_TB == "Pacific"){
        
        scenByPacificRes[[selectVar]]
      } else if(input$subGrp_TB == "Asian"){
        
        scenByAsianRes[[selectVar]]
      }  else if(input$subGrp_TB == "NZ European/Other"){
        
        scenByEuroRes[[selectVar]]
      }  
    
    
    if(selectVar == "dep_curr"){
      
      results$Var <- gsub("quantile", "quintile", results$Var)
      
      
      results$Var <- factor(results$Var ,
                            levels =  c( "1st quintile",
                                        "2nd quintile","Median", "3rd quintile",
                  "4th quintile"))
      
      
      levels(results$Var) <- c("1st quintile (least)",
                                  "2nd quintile", 
                                  "3rd quintile", 
                                  "4th quintile", 
                                  "5th quintile")
      
      
      
      
    } else if(selectVar == "emp_curr"){
      
      results$Var <- factor(results$Var, levels = 
                              c( "Full-/Part-time employed", 
                                 "Unemployed/Unpaid", 
                                 "Not in labour force"))
      
    } else if(selectVar == "religion_curr"){
      
      results$Var <- factor(results$Var, levels =
                              c("No region",  "Christian", "Other"))
      
      levels(results$Var)[1] <- "No religion"
      
    } else if(selectVar == "education_curr"){
      
      results$Var <- factor(results$Var, levels =
                              c("No Qualification", 
                                "Secondary School Qualification",
                                "Post-school non-university", 
                                "University Qualification" ))
    } else if(selectVar == "h_income_curr_cat" | 
              selectVar == "p_income_curr_cat"){
      
      
      results$Var <- factor(results$Var, levels =
                              c(
                                "Zero or loss" ,
                                "$1-$10,000",
                                "$10,001-$20,000",
                                "$20,001-$30,000",
                                "$30,001-$40,000",
                                "$40,001-$50,000",
                                "$50,001-$60,000",
                                "$60,001-$70,000",
                                "$70,001-$100,000",
                                "$100,001 or More"
                              ) )
      
    } else if(selectVar == "partner_curr"){
      results$Var <- factor(results$Var, levels =
                              c(
                                "not living with a partner" ,
                                "living with a partner"
                              ) )
      
      
    } else if (input$input_type_TB != "Mean") {
      
      results$Var <- factor(results$Var)
    }

    
    if(input$subGrp_TB != "None" &
       !is.null(input$subGrp_TB2)) {
      if (input$subGrp_TB2 != "None") {
        results <- results %>%
          filter(groupByData == req(input$subGrp_TB2))
      } else {
        results <-  results
      }
    }
    
    
    if(input$input_type_TB == "Count")
      results <- results %>% 
      select(-Mean, -Lower, -Upper) %>%
      rename(Mean = MeanCount, 
             Lower = LowerCount, 
             Upper = UpperCount)
    
    
    if(input$input_type_TB == "Percentage")
      results <- results %>% 
      select(-MeanCount, -LowerCount, -UpperCount)
    
    
    
    SBTB <<- results
    
    results
    
  })
  
  output$resultSBTB  <- DT::renderDataTable({

    results <- req(summaryOutputSBTB())

    
    if(results$Year[1] ==  "Childhood" | results$Year[1] ==  "At birth"){


    } else if((input$input_type_TB == "Percentage" | input$input_type_TB == "Count") &  
              "groupByData" %in% names(results) ){
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]

      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")),
                       Year~groupByData + Var + variable)

    }else if((input$input_type_TB == "Percentage" | input$input_type_TB == "Count")){

      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]

      results <- dcast(melt(results, id.vars = c("Var", "Year")),
                       Year~Var + variable)

    } else if(input$input_type_TB %in% c("Mean", "Quantile") &
              "groupByData" %in% names(results) ){

      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]

      results <- dcast(melt(results, id.vars = c("groupByData", "Year")),
                       Year ~ groupByData + variable)

    } else if(input$input_type_TB %in% c("Mean", "Quantile") &
              "Var" %in% names(results)){
      return(NULL)
    }

    index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))

    notToRound <- c("<span style=\"font-size:20px\">Year</span>",
                    "<span style=\"font-size:20px\">groupByData</span>",
                    "<span style=\"font-size:20px\">Var</span>")

    if(!input$ci)
      results <- results[,-index]

    if(input$input_type_TB == "Percentage")
      colnames(results) <- gsub("Mean", "Percent", colnames(results))



    rv$tableResult$Scenario <- results

    colnames(results) <-
      paste0('<span style="font-size:20px">',colnames(results),'</span>')

    
    tables <- 
      DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
                  options = list(pageLength = 9999999, dom = 't',
                                 scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                                 scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') 
    
    
    if(input$input_type_TB == "Percentage")
      tables %>% formatRound(which(!colnames(results) %in% notToRound), digits = 1)
    else 
      tables %>% formatRound(which(!colnames(results) %in% notToRound), digits = 0)
    
  })
  
  # Display results from here  #####
  
  output$ciUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 4 (optional):")),
        checkboxInput("ci", label = "Confidence Interval", value = FALSE)
      )
    })
  
  output$downloadUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 5 (optional):")),
        downloadButton('downloadTable', 'Download Table'),
        downloadButton('downloadPlot', 'Download Plot')
      )
    })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('Table Result-',  input$input_type_TB, " ", 
            input$dynamicTB, " ", 
            input$selSB, '.xlsx', sep='')
    },
    content = function(con) {
     
      temp <- data.frame(Variable = input$dynamicTB)
  
      rv$tableResult$info <- t(temp)
      write.xlsx(rv$tableResult, con)
      rv$tableResult <- list()
    }
  )
  
  combineResults <- reactive({
    
    baseTB <-summaryOutputTB()
    
    SBTB <- try(summaryOutputSBTB(), silent = TRUE)
    

    if(class(SBTB) == "try-error") SBTB <- NULL
    
    colname <- names(baseTB)
    
    combineResults <- data.frame(Scenario = "Base", baseTB)
    
    
    if(!is.null(SBTB))
      combineResults <- rbind(combineResults, 
                              data.frame(Scenario = "Scenario", SBTB))
    
    
    combineResults
  })
  
  output$barchartBase<- renderPlotly({
    
    tables.list <- combineResults()

    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year)) +  guides(fill=guide_legend(title=NULL))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p + ggtitle(input$dynamicTB) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p + theme_bw()+ scale_fill_grey(start = 0, end = .9))
  })
  
  output$barchartSC<- renderPlotly({
    
    input$ci
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year))  +  guides(fill=guide_legend(title=NULL))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p + ggtitle(input$dynamicTB) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p + theme_bw()+ scale_fill_grey(start = 0, end = .9))
  })
  
  output$barchart<- renderPlotly({
    
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) 
    
    p <- 
      p + ggtitle(input$dynamicTB) +
      geom_bar(position=dodge, stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p + theme_bw()+ scale_fill_grey(start = 0, end = .9))
  })
  
  output$linePlotBase<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year)) + facet_wrap(~groupByData, scales = "free") 
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p + ggtitle(input$dynamicTB) +
      geom_path() +
      geom_point(size = 2)+ 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p + theme_bw())
  })
  
  output$linePlotSC<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    

    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year)) + facet_wrap(~groupByData, scales = "free") 
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p + ggtitle(input$dynamicTB) +
      geom_path() +
      geom_point(size = 2)+ 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p + theme_bw())
  })
  
  
  output$linePlot<- renderPlotly({
    
    input$ci
    
    tables.list <- combineResults()
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage" | input$input_type_TB == "Count")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year, shape=Scenario, linetype = Scenario)) + facet_wrap(~groupByData, scales = "free")
    else 
      ggplot(tables.list, aes(y = Mean, x = Year, shape=Scenario, linetype = Scenario)) 
    

    
    p <- p + ggtitle(input$dynamicTB) +
      geom_path(position = dodge)+
      geom_point(size = 2, position = dodge) + 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$input_type_TB == "Count")
      p  <-  p + ylab("Count")
    
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.25, 
                             position = dodge)
    
    ggplotly(p + theme_bw())
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      

        if(last_plot()$x$data[[1]]$type == "scatter")
          type <- "Line"
        else 
          type <- "Bar"
  
      
      paste(type,'Plot-', input$input_type_TB, "-", 
            input$dynamicTB, '.png', sep='')
    },
    content = function(con) {
      ggsave(con, width = 30, height = 20, units = "cm")
    }
  )
  
})  