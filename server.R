library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(openxlsx)
library(reshape2)

shinyServer(function(input, output, session) {
  
  
  baseSum <- load("base/BaseRes.Rata")
  
  
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
  
  
  # catvars <- unique(c(time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="categorical"], 
  #                     getOutcomeVars(initialSim$simframe, "categorical"),  names(binbreaks)))
  
  catvars <- allVar[!grepl("_con", allVar)]
  
  
  #contvars <- unique(c(getOutcomeVars(initialSim$simframe, "continuous"), 
  #              time_invariant_vars$Varname[time_invariant_vars$Outcome_type=="continuous"]))
  
  contvars <- allVar[grepl("_con", allVar)]
  
  # freqList <-  data.frame(Var =catvars, 
  #                         Name = as.character(trimws(varName[catvars])), 
  #                         stringsAsFactors = FALSE)
  # 
  # meansList <-  quantilesList <-data.frame(Var = contvars, 
  #                                          Name = as.character(trimws(varName[contvars])), 
  #                                          stringsAsFactors = FALSE)
  # 
  # meansList <- meansList[order(meansList$Name),]  
  # 
  # freqList <- freqList[order(freqList$Name),]  
  
  subGrpVar <- c("age", "sex",  "Maori", "Pacific", "Asian",
                 "Euro")
  
 
  
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
           "Percentage" = selectInput("dynamicTB", temp,choices = sort(catvars), 
                                      selected = catvars[1]),
           "Mean" = selectInput("dynamicTB", temp, choices =  sort(contvars),
                                selected = contvars[1])
           )
  })
  
  output$uiSubGrpTB <- renderUI({
    
    input$input_type_TB
    
    selectInput("subGrp_TB", HTML("<b> <font size=\"4\">STEP 3 (optional): </font></b> Select ByGroup:"), 
                choices = c(None='None',  subGrpVar))
  })
  
  # Subgroup formula ####
  # output$uiExprTB <- renderUI({
  #   input$input_type_TB
  #   
  #   selectInput("subGrp_TB1", HTML("<b> <font size=\"4\">STEP 4 (optional): </font></b> Select Subgroup for subgroup formula:"),
  #               choices = c(None='None',  subGrpVar))
  # })
  # 
  # output$uiExprTB1 <- renderUI({
  #   
  #   #print(names(env.base$dict$codings[[names(which(varName == input$subGrp_TB1))]]))
  #   if(input$subGrp_TB1 == "None")
  #     return()
  #   else  
  #     choice <- names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]])
  #   
  #   isolate(
  #     if(is.null(choice)){
  #       inputPanel(
  #         div( class = "ui-hide-label", style="float:left",
  #              selectInput("subGrp_TB2", input$subGrp_TB1, 
  #                          choices = c("Equals" = " == ",  "Less than" = " < ", 
  #                                      "Greater than" = " > ", "Less than or equal to" = " <= ", 
  #                                      "Greater than or equal to" = " >= ", "Not equals to " = " != "), 
  #                          selectize=FALSE)),
  #         div( class = "ui-hide-label", style="float:left", textInput("subGrpNum_TB2", ""))
  #       )
  #       
  #     } else {
  #       selectInput("subGrp_TB2", input$subGrp_TB1,
  #                   choices = choice, 
  #                   selectize=FALSE)
  #     }
  #   )
  # })
  # 
  # 
  # logisetexprTB <-eventReactive( input$completeTB, {
  #   # Depending on input$input_type, we'll generate a different
  #   # UI component and send it to the client.
  #   
  #   if(input$subGrp_TB1 == "None")
  #     return()
  #   
  #   index = env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]][
  #     (names(env.base$dict$codings[[varList$Var[varList$Name == input$subGrp_TB1]]])==
  #        input$subGrp_TB2)]
  #   
  #   if(is.null(index)){
  #     paste(varList$Var[varList$Name == input$subGrp_TB1], 
  #           paste(input$subGrp_TB2, input$subGrpNum_TB2), sep = " ")    
  #   }else{
  #     paste(varList$Var[varList$Name == input$subGrp_TB1], index, sep = " == ")
  #   }
  #   
  # })
  # 
  # logisetexprTB1 <-eventReactive(input$operatorTB, {
  #   # Depending on input$input_type, we'll generate a different
  #   # UI component and send it to the client.
  #   
  #   index = input$subGrp_TB2
  #   
  #   if(is.null(index)){
  #     paste( input$subGrp_TB1, paste(input$subGrp_TB2, input$subGrpNum_TB2), sep = " ")    
  #   }else{
  #     paste(input$subGrp_TB1, index, sep = " == ")
  #   }
  # })
  # 
  # observeEvent( input$leftBrackTB, { 
  #   rv$finalFormulaTB <- paste0("(", rv$finalFormulaTB)
  # })
  # 
  # observeEvent( input$rightBrackTB, { 
  #   rv$finalFormulaTB <-  paste0(rv$finalFormulaTB,")")
  # })
  # 
  # 
  # observeEvent( input$andTB, { 
  #   rv$finalFormulaTB <- paste(rv$finalFormulaTB, "&")
  # })
  # 
  # observeEvent( input$orTB, { 
  #   rv$finalFormulaTB <- paste(rv$finalFormulaTB,  "|")
  # })
  # 
  # observeEvent( input$completeTB, { 
  #   
  #   rv$finalFormulaTB <- paste(rv$finalFormulaTB, logisetexprTB())
  # })
  # 
  # observeEvent( input$resetTB, { 
  #   rv$finalFormulaTB <- NULL
  # })
  # 
  # output$uilogisetexprTB <- renderUI({  
  #   textareaInput("logisetexprTB",  "Subgroup formula:", value = rv$finalFormulaTB)
  # })
  # 
  # observeEvent( input$logisetexprSB, { 
  #   rv$finalFormulaTB <- input$logisetexprTB
  # })
  
  
  # Tables starts here ####
  
  baseTB <<- NULL 
  
  
  summaryOutputTB <- reactive( { 
    
    inputType = c("frequencies", "means", "quantiles")
    
    names(inputType) = c("Percentage", "Mean","Quantile" )
    
    
    
    results <- 
    if(input$subGrp_TB == "None"){
      
      c(baseRes, timeInvRes)[[input$dynamicTB]]
    } else if(input$subGrp_TB == "age"){
      
      byAgeRes[[input$dynamicTB]]
    }  else if(input$subGrp_TB == "sex"){
      
      bySexRes[[input$dynamicTB]]
    }  else if(input$subGrp_TB == "Maori"){
      
      byMaoriRes[[input$dynamicTB]]
    }   else if(input$subGrp_TB == "Pacific"){
      
      byPacificRes[[input$dynamicTB]]
    } else if(input$subGrp_TB == "Asian"){
      
      byAsianRes[[input$dynamicTB]]
    }  else if(input$subGrp_TB == "Euro"){
      
      byEuroRes[[input$dynamicTB]]
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
  
  SBTB <<- NULL
  
  # Scenario tables #####
  # SBTB <<- NULL
  # 
  # 
  # summaryOutputSBTB <- reactive({
  #   
  #   
  #   inputType = c("frequencies", "means", "quantiles")
  #   
  #   names(inputType) = c("Percentage", "Mean","Quantile" )
  #   
  #   grpbyName = varList$Var[varList$Name==input$subGrp_TB] 
  #   
  #   print(input$selSB)
  #   
  #   if(length(grpbyName) == 0) grpbyName = ""
  #   
  #   if(input$basePop == "Base population (Before scenario testing)"){
  #     results <-tableBuilderNew(rv$savedScenario[[input$selSB]], 
  #                               statistic = inputType[input$input_type_TB], 
  #                               variableName = varList$Var[varList$Name==input$dynamicTB],
  #                               grpbyName = grpbyName, CI = input$ci, 
  #                               logisetexpr = trimws(input$logisetexprTB),
  #                               envBase = env.base, basePop = TRUE, digits = 5) 
  #   } else {
  #     results <-tableBuilderNew(rv$savedScenario[[input$selSB]], 
  #                               statistic = inputType[input$input_type_TB], 
  #                               variableName = varList$Var[varList$Name==input$dynamicTB],
  #                               grpbyName = grpbyName, CI = input$ci, 
  #                               logisetexpr = trimws(input$logisetexprTB),
  #                               envBase = env.base, digits = 5)
  #   }
  #   
  #   
  #   
  #   
  #   SBTB <<-results
  #   
  #   results
  # })
  # 
  # output$resultSBTB  <- DT::renderDataTable({
  #   
  #   results <- summaryOutputSBTB()
  #   
  #   
  #   if(results$Year[1] ==  "Childhood" | results$Year[1] ==  "At birth"){
  #     
  #     
  #   } else if(input$input_type_TB == "Percentage" &  "groupByData" %in% names(results) ){
  #     if(length(unique(results$Var)) == 2)
  #       results <- results[results$Var==input$Var_TB, ]
  #     
  #     results <- dcast(melt(results, id.vars = c("Var", "groupByData", "Year")), 
  #                      Year~groupByData + Var + variable)
  #     
  #   }else if(input$input_type_TB == "Percentage"){
  #     
  #     if(length(unique(results$Var)) == 2)
  #       results <- results[results$Var==input$Var_TB, ]
  #     
  #     results <- dcast(melt(results, id.vars = c("Var", "Year")), 
  #                      Year~Var + variable)
  #     
  #   } else if(input$input_type_TB %in% c("Mean", "Quantile") & 
  #             "groupByData" %in% names(results) ){
  #     
  #     if(length(unique(results$Var)) == 2)
  #       results <- results[results$Var==input$Var_TB, ]
  #     
  #     results <- dcast(melt(results, id.vars = c("groupByData", "Year")), 
  #                      Year ~ groupByData + variable)
  #     
  #   } else if(input$input_type_TB %in% c("Mean", "Quantile") &
  #             "Var" %in% names(results)){
  #     return(NULL)
  #   }    
  #   
  #   index <- c(grep("Lower", colnames(results)), grep("Upper", colnames(results)))
  #   
  #   notToRound <- c("<span style=\"font-size:20px\">Year</span>", 
  #                   "<span style=\"font-size:20px\">groupByData</span>", 
  #                   "<span style=\"font-size:20px\">Var</span>")
  #   
  #   if(!input$ci)
  #     results <- results[,-index]
  #   
  #   if(input$input_type_TB == "Percentage")
  #     colnames(results) <- gsub("Mean", "Percent", colnames(results))
  #   
  #   
  #   
  #   rv$tableResult$Scenario <- results
  #   
  #   colnames(results) <- 
  #     paste0('<span style="font-size:20px">',colnames(results),'</span>')
  #   
  #   DT::datatable(results, rownames = FALSE, extensions = 'Scroller', escape = FALSE,
  #                 options = list(pageLength = 9999999, dom = 't',
  #                                scrollX = TRUE,  deferRender = TRUE, scrollY = 600,
  #                                scrollCollapse = TRUE))  %>%
  #     formatStyle(1:ncol(results), 'font-size' = '20px') %>% 
  #     formatRound(which(!colnames(results) %in% notToRound), digits = 1)
  #   
  # })
  
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
            input$dynamicTB, " ", 
            input$selSB, '.xlsx', sep='')
    },
    content = function(con) {
      
      print(rv$finalFormulaSB)
      
      temp <- data.frame(Variable = input$dynamicTB)
      
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
    
    p  <-  p +  ggtitle(input$dynamicTB) + 
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
    
    p  <-  p +  ggtitle(input$dynamicTB) + 
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
      p + ggtitle(input$dynamicTB) + 
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
    
    p  <- p + ggtitle(input$dynamicTB) +  geom_path() +
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
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(colour=groupByData, y = Mean, x = Year))
    else 
      ggplot(tables.list, aes(y = Mean, x = Year)) 
    
    p  <- p+  ggtitle(input$dynamicTB) +  geom_path() +
      geom_point(size = 2)+ theme(text = element_text(size = 15))
    
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
    
    if(input$input_type_TB == "Percentage")
      tables.list <- tables.list[tables.list$Var==input$Var_TB, ] 
    
    p <- if("groupByData" %in% names(tables.list))
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) + facet_wrap(~groupByData)
    else 
      ggplot(tables.list, aes(y = Mean, x = Year, colour=Scenario)) 
    
    
    p <- p +  ggtitle(input$dynamicTB) +  
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
    
    p  <- p + ggtitle(input$dynamicTB) + 
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
    
    p  <- p + ggtitle(input$dynamicTB) + 
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
    
    p  <- p + ggtitle(input$dynamicTB) + 
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