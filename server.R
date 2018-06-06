library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(cowplot)
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

  allVar <- c(names(timeInvRes), names(baseRes))
  
  
  catvars <- allVar[!grepl("_con", allVar)]
  
  
  contvars <- allVar[grepl("_con", allVar)]
  
  
  subGrpVar <- c("age", "sex",  "Maori", "Pacific", "Asian",
                 "Euro")
  
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

    switch(input$input_type_TB,
           "Percentage" = selectInput("dynamicTB", temp, 
                                      choices = as.character(sort(varname.vec[catvars])), 
                                      selected = as.character(sort(varname.vec[catvars]))[1]),
           "Mean" = selectInput("dynamicTB", temp, 
                                choices =  as.character(sort(varname.vec[contvars])),
                                selected = as.character(sort(varname.vec[contvars]))[1])
           )
  })
  
  output$uiSubGrpTB <- renderUI({
    
    input$input_type_TB
    
    selectInput("subGrp_TB",
                HTML("<b> <font size=\"4\">STEP 3 (optional): </font></b> Select ByGroup:"), 
                choices = c(None='None',  subGrpVar))
  })
  
 
  # Tables starts here ####
  
  baseTB <<- NULL 
  
  
  summaryOutputTB <- reactive( { 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Mean","Quantile" )
    
    
    selectVar <- names(varname.vec) [varname.vec %in% input$dynamicTB]
    
    results <- 
    if(input$subGrp_TB == "None"){
      
      if(input$dynamicTB == "Age group"){
        temp <- timeInvRes$age_cat
        
        temp$Var <- factor(temp$Var, 
                           levels =
                             c(
                               "Early Childhood (0-14)",
                               "Teenage Years (15-19)",
                               "Young Adulthood (20-24)",
                               "Middle Adulthood (25-34)" ,
                               "Later Adulthood (35-54)" ,
                               "Older Life Working (55-64)",
                               "Older Life Retired (65-74)",
                               "Later Life (75+)"
                             ))
        
        temp
      } else {
        
        
        c(baseRes, timeInvRes)[[selectVar]]
      }
      
    } else if(input$subGrp_TB == "Age"){

      temp <- byAgeRes[[selectVar]]
      
      temp$groupByData <-
        factor(
          temp$groupByData,
          levels =
            c(
              "Early Childhood (0-14)",
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
    }  else if(input$subGrp_TB == "sex"){
      
      bySexRes[[selectVar]]
    }  else if(input$subGrp_TB == "Maori"){
      
      byMaoriRes[[selectVar]]
    }   else if(input$subGrp_TB == "Pacific"){
      
      byPacificRes[[selectVar]]
    } else if(input$subGrp_TB == "Asian"){
      
      byAsianRes[[selectVar]]
    }  else if(input$subGrp_TB == "Euro"){
      
      byEuroRes[[selectVar]]
    }  
    
    baseTB <<- results
    
    results
  })
  
  
  
  output$uiVar <- renderUI({
    # if(length(unique(summaryOutputTB()$Year))!=1)
    selectInput("Var_TB", "Select a level to compare in plot:",  
                selected = unique(summaryOutputTB()$Var)[2], 
                choices = unique(summaryOutputTB()$Var))
  })
  
  
  output$resultTB  <- DT::renderDataTable({
    
    results <- summaryOutputTB()
    
    
   if(input$input_type_TB == "Percentage" &  "groupByData" %in% names(results) ){
      
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]
      
      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
                       Year~groupByData + Var + variable)
      
    }else if(input$input_type_TB == "Percentage"){
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
    
    DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
                  options = list(pageLength = 9999999, dom = 't',
                                 scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                                 scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') %>% 
      formatRound(which(!colnames(results) %in% notToRound), digits = 1)
  })
  
  
  output$selectSB <- renderUI({
    
    
    selectInput("selSB", "Select Scenario for comparison:",
                choices =c("emp_86-91_81",
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
  
  
  # Scenario tables #####
  SBTB <<- NULL


  summaryOutputSBTB <- reactive({
    
    
    load(paste0("base/", input$selSB, ".Rata"))
 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Mean","Quantile" )
    
    selectVar <- names(varname.vec) [varname.vec %in% input$dynamicTB]
    
    
    results <- 
      if(input$subGrp_TB == "None"){
        
        if(input$dynamicTB == "Age group"){
          temp <- scenTimeInvRes$age_cat
          
          temp$Var <- factor(temp$Var, 
                             levels =
                               c(
                                 "Early Childhood (0-14)",
                                 "Teenage Years (15-19)",
                                 "Young Adulthood (20-24)",
                                 "Middle Adulthood (25-34)" ,
                                 "Later Adulthood (35-54)" ,
                                 "Older Life Working (55-64)",
                                 "Older Life Retired (65-74)",
                                 "Later Life (75+)"
                               ))
          
          temp
        } else {
          
        c(scenRes, scenTimeInvRes)[[selectVar]]
          
        }
      } else if(input$subGrp_TB == "Age"){
     
        temp <- scenByAgeRes[[selectVar]]
        
        temp$groupByData <-
          factor(
            temp$groupByData,
            levels =
              c(
                "Early Childhood (0-14)",
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
      }  else if(input$subGrp_TB == "sex"){
        
        scenBySexRes[[selectVar]]
      }  else if(input$subGrp_TB == "Maori"){
        
        scenByMaoriRes[[selectVar]]
      }   else if(input$subGrp_TB == "Pacific"){
        
        scenByPacificRes[[selectVar]]
      } else if(input$subGrp_TB == "Asian"){
        
        scenByAsianRes[[selectVar]]
      }  else if(input$subGrp_TB == "Euro"){
        
        scenByEuroRes[[selectVar]]
      }  
    
    SBTB <<- results
    
    results
    
  })
  
  output$resultSBTB  <- DT::renderDataTable({

    results <- summaryOutputSBTB()


    if(results$Year[1] ==  "Childhood" | results$Year[1] ==  "At birth"){


    } else if(input$input_type_TB == "Percentage" &  "groupByData" %in% names(results) ){
      if(length(unique(results$Var)) == 2)
        results <- results[results$Var==input$Var_TB, ]

      results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")),
                       Year~groupByData + Var + variable)

    }else if(input$input_type_TB == "Percentage"){

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

    DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
                  options = list(pageLength = 9999999, dom = 't',
                                 scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
                                 scrollCollapse = TRUE))  %>%
      formatStyle(1:ncol(results), 'font-size' = '20px') %>%
      formatRound(which(!colnames(results) %in% notToRound), digits = 1)

  })
  
  # Display results from here  #####
  
  
  output$ciUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 6 (optional):")),
        checkboxInput("ci", label = "Confidence Interval", value = TRUE)
      )
    })
  
  output$downloadUI <- 
    renderUI({
      tagList(
        h4(strong("STEP 7 (optional):")),
        downloadButton('downloadTable', 'Download Table'),
        downloadButton('downloadPlot', 'Download Plot')
      )
    })
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('Table Result-',  input$input_type_TB, " ", 
            varname.vec[input$dynamicTB], " ", 
            input$selSB, '.xlsx', sep='')
    },
    content = function(con) {
      
      print(rv$finalFormulaSB)
      
      temp <- data.frame(Variable = varname.vec[input$dynamicTB])
      
      if(input$selSB != "")
        temp$Scenario = input$selSB
      
      if(!is.null(rv$finalFormulaSB))
        temp$SubgroupFormula = rv$finalFormulaSB
      
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
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p +  ggtitle(varname.vec[input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  output$barchartSC<- renderPlotly({
    
    input$ci
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <-  p +  ggtitle(varname.vec[input$dynamicTB]) + 
      geom_bar(position="dodge", stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  
  
  output$barchart<- renderPlotly({
    
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ]
    
    tables.list$Year <- factor(tables.list$Year)
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(fill=Scenario, y = Mean, x = Year)) 
    
    p <- 
      p + ggtitle(varname.vec[input$dynamicTB]) + 
      geom_bar(position=dodge, stat = "identity") + 
      theme(text = element_text(size = 15))
    
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, position=dodge, width=0.25)
    
    ggplotly(p)
  })
  
  
  
  output$linePlotBase<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year)) + facet_wrap(~groupByData, scales = "free")
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p + ggtitle(varname.vec[input$dynamicTB]) +  
      geom_path() +
      geom_point(size = 2)+ 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p)
  })
  
  output$linePlotSC<- renderPlotly({
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    

    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year)) + facet_wrap(~groupByData, scales = "free")
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p + ggtitle(varname.vec[input$dynamicTB]) + 
      geom_path() +
      geom_point(size = 2)+ 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.2)
    
    ggplotly(p)
  })
  
  
  output$linePlot<- renderPlotly({
    
    input$ci
    
    tables.list <- combineResults()
    
    limitsGGplot <- aes(ymax = Upper, ymin=Lower)
    dodge <- position_dodge(width=0.9)
    
    tables.list$Year <- as.numeric(tables.list$Year)
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) + facet_wrap(~groupByData, scales = "free")
    else 
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) 
    

    
    p <- p +  ggtitle(varname.vec[input$dynamicTB]) +  
      geom_path(position = dodge)+
      geom_point(size = 2, position = dodge) + 
      theme(text = element_text(size = 15))
    
    if(input$input_type_TB == "Percentage")
      p  <-  p + ylab("Percentage")
    
    if(input$ci)
      p <- p + geom_errorbar(limitsGGplot, width=0.25, 
                             position = dodge)
    
    ggplotly(p)
  })
  
  
  output$boxPlotBase<- renderPlot({
    
    input$ci
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Base")
    
    colname <- names(summaryOutputTB())
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =groupByData, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    else 
      ggplot(tables.list, aes(x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varname.vec[input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
    
    p
  })
  
  
  
  output$boxPlotSC<- renderPlot({
    
    input$ci
    
    tables.list <- combineResults()
    
    tables.list <- tables.list %>% filter(Scenario == "Scenario")
    
    colname <- names(summaryOutputTB())
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =groupByData, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    else 
      ggplot(tables.list, aes(x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varname.vec[input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
    
    p
    
  })
  
  output$boxPlot<- renderPlot({
    
    input$ci
    
    tables.list <- combineResults()
    
    colname <- names(summaryOutputTB())
    
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(fill =Scenario, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(fill =Scenario, x = Year,  ymin = `Min`, lower = `X25th`, 
                              middle = `X50th`, upper = `X75th`, ymax = `Max`)) 
    
    p  <- p + ggtitle(varname.vec[input$dynamicTB]) + 
      geom_boxplot(stat = "identity") + theme(text = element_text(size = 15))
    
    p
    
  })
  

  
  output$downloadPlot <- downloadHandler(
    
    
    filename = function() {
      
      if(input$input_type_TB == "Quantile"){
        type <- "Box"
      }else{   
        if(last_plot()$x$data[[1]]$type == "scatter")
          type <- "Line"
        else 
          type <- "Bar"
      }
      
      paste(type,'Plot-', input$input_type_TB, "-", 
            input$dynamicTB, '.png', sep='')
    },
    content = function(con) {
      ggsave(con)
    }
  )
  
})  