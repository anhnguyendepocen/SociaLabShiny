library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyBS)

options(shiny.maxRequestSize = 10000 * 1024 ^ 2)


dbHeader <- dashboardHeader(title = "NZ Lab",
                            tags$li(
                              a(href = 'https://drive.google.com/open?id=0ByGI4aqoCDeldHdNaDBLQzFLXzg',
                                "User guide", target = "_blank"),
                              class = "dropdown",
                              tags$style(".main-header {max-height: 58px}"),
                              tags$style(".main-header .logo {height: 60px}")
                            ))

dSiderBar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("First Page", tabName = "fb", icon = icon("home")),
    #sidebarMenuOutput("menu"),
    menuItem("Table Builder", tabName = "tb", icon = icon("table")),
    # bsPopover("menu", "",trigger = "manual",
    #           paste0("Model input - Visualising the Conceptual framework. <br> <br>",
    #                  "Scenario Builder – Simulation with a new Scenario. <br> <br>",
    #                  "Table Builder - Tabulate and/or plot the simulated outcome and compare to original."),
    #           placement ="right", options = list(container = "body")),
    br(),
    box(
      title = "Scenarios Run",
      background = "black",
      status = "danger",
      solidHeader = TRUE,
      uiOutput("selectSB"),
      width = 12
    ),
    # bsPopover("selectSB", "",
    #           paste0("Lists all scenarios run and allows a scenario to be selected for comparison with base outcomes."),
    #           placement ="right", options = list(container = "body")),
    br(),
    box(
      h5("Latest Update:"),
      h5("2017-08-30"),
      h5("Contact email:"),
      a("Roy Lay-Yee", href = "mailto:r.layyee@auckland.ac.nz"),
      br(),
      a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz"),
      width = 12,
      background = "black"
    ),
    bsPopover(
      "saveWrkspace",
      "",
      trigger = "manual",
      paste0("Saves project and its scenario settings and results. "),
      placement = "right",
      options = list(container = "body")
    )
  ),
  tags$style(".left-side, .main-sidebar {padding-top: 60px}")
)

dashboardPage(
  skin = "blue",
  title = "NZ Lab",
  dbHeader,
  dSiderBar,
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$style(
        HTML('
             .main-header .logo {
             font-weight: bold;
             font-size: 24px;
             }
             ')
        )
        ),
    tabItems(tabItem(
      "fb",
      # box(title ="Quick Link", width = 12,
      #     status = "primary",solidHeader = TRUE,
      #     p(actionButton("switchMI", "Model input"), "Visualising the Conceptual framework."),
      #     p(actionButton("switchSB", "Scenario Builder"), "Simulation with a new Scenario"),
      #     p(actionButton("switchTB", "Table Builder"), "Tabulate and/or plot the simulated outcome and compare to original.")
      #     ),
      box(
        width = 12,
        h3("New Zealand as a Social Laboratory"),
        p(
          "The aim is to model NZ society with an emphasis on the impact of
            personal and household resources in helping individuals to achieve 
            valued social goals such as doing well at school, getting a job, 
            and enjoying a good standard of living."
        ),
        p(
          "To do this, we construct a dynamic discrete-time microsimulation model 
          of the NZ population over the period 1981 to 2006, using NZ Longitudinal 
          Census data (produced by Statistics NZ) as the source for a starting sample
          as well as for statistical estimation to inform subsequent simulation. 
          Supplementary official data are used to account for population changes over time."),
        p(
          "This computer model (constituting a ‘social laboratory’) can then be used to 
          undertake projections - what would NZ look like in the future? - and scenario 
          testing - what if features of NZ society were different or were changed somehow? 
          These virtual experiments can address real questions of either scientific or 
          policy significance. "
        ),
        p(
          "We ask such pertinent questions here in a series of scenarios the results for 
          which can be interrogated and visualised via this Shiny app, an accompaniment to
          our book “Simulating Societal Change - Counterfactual Modelling for Social and 
          Policy Inquiry” (Springer)."
        ),
        
        
        h4("Disclaimer:"),
        
        p("The results in this shiny application are not official statistics They have been created 
            for research purposes from the Integrated Data Infrastructure (IDI), managed by Statistics New Zealand."), 
        p("The opinions, findings, recommendations, and conclusions expressed in this shiny application are those of
          the author(s), not Statistics NZ."), 
        p("Access to the anonymised data used in this study was provided by Statistics NZ under
          the security and confidentiality provisions of the Statistics Act 1975. Only people
          authorised by the Statistics Act 1975 are allowed to see data about a particular person, household, 
          business, or organisation, and the results in this shiny application have been confidentialised to 
          protect these groups from identification and to keep their data safe."),
        p("Careful consideration has been given to the privacy, security, and confidentiality issues associated
          with using administrative and survey data in the IDI. Further detail can be found in the Privacy impact
          assessment for the Integrated Data Infrastructure available from www.stats.govt.nz."),
        h4("To cite this application, please use the following,"),
        p(
          "Davis, P., Lay-Yee, R., Chang, K., von Randow, M., Milne, B. (2017) Shiny application: New Zealand as a Social Laboratory. 
          https://compassnz.shinyapps.io/NZLabShiny/"
        ),
        h4("The source code is stored in three places:"),
        HTML(
          "<ul>
          <li>Simario R package is at: <a href = \"https://github.com/kcha193/simarioV2\"> https://github.com/kcha193/simarioV2 </a>.</li>
          <li>Shiny application is at: <a href = \"https://github.com/kcha193/NZLabShiny\"> https://github.com/kcha193/NZLabShiny </a>.</li>
          </ul>"
        ),
        p(""),
        a(
          href = "http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
          img(src = "http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/_jcr_content/par/textimage/image.img.png/1443396492336.png",
              width = 200)
        ),
        br(),
        br(),
        actionButton(
          inputId = 'ab1',
          label = HTML(
            "<b> <font size=\"4\"> Getting Started </font> </b> <br><b> <font size=\"4\"> (User Guide)  </font></b>"
          ),
          width = "200px",
          hight = "100px",
          onclick = "window.open('https://drive.google.com/open?id=0ByGI4aqoCDeldHdNaDBLQzFLXzg', '_blank')"
        )
        )
        ),
      #box( width = 6,  includeHTML("ppt.Rhtml"))),
      
      tabItem("tb",
              # Sidebar with a slider input for the number of bins
              fluidRow(
                box(
                  title = "Variable",
                  width = 3,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "input_type_TB",
                    HTML("<b> <font size=\"4\">STEP 1: </font></b> Select Summary Measure"),
                    c("Percentage", "Mean", "Quantile")
                  ),
                  
                  uiOutput("uiTB"),
                  
                  uiOutput("uiVar"),
                  
                  uiOutput("uiSubGrpTB"),
                  uiOutput("uiExprTB"),
                  uiOutput("uiExprTB1"),
                  
                  actionButton("completeTB", "Insert"),
                  actionButton("leftBrackTB", "("),
                  actionButton("rightBrackTB", ")"),
                  actionButton("andTB", "And"),
                  actionButton("orTB", "Or"),
                  actionButton("resetTB", "Reset"),
                  uiOutput("uilogisetexprTB"),
                  selectInput(
                    "basePop",
                    HTML(
                      "<b> <font size=\"4\">STEP 5 (optional): </font></b> Apply subgroup to:"
                    ),
                    c(
                      "Base population (Before scenario testing)",
                      "Scenario population (After scenario testing)"
                    ),
                    selected = "Scenario population (After scenario testing)"
                  ),
                  
                  uiOutput("ciUI"),
                  
                  uiOutput("downloadUI"),
                  bsPopover(
                    "downloadUI",
                    "",
                    trigger = "manual",
                    paste0(
                      "You can save tables in xls format by clicking “Download Table” and save plots in png format by clicking “Download Plot”."
                    ),
                    placement = "right",
                    options = list(container = "body")
                  )
                ),
                # Show a plot of the generated distribution
                tabBox(
                  width = 9,
                  height = "750px",
                  id = "mainTabset",
                  tabPanel(title = "Base", dataTableOutput('resultTB')),
                  tabPanel(title = "Scenario",  dataTableOutput('resultSBTB')),
                  navbarMenu(
                    "Barchart (Percentage and Mean only)" ,
                    tabPanel(
                      "Base only",
                      plotlyOutput("barchartBase", width = "90%", height = "700px")
                    ),
                    tabPanel(
                      "Scenario only",
                      plotlyOutput("barchartSC", width = "90%", height = "700px")
                    ),
                    tabPanel(
                      "Base versus Scenario",
                      #verbatimTextOutput("compare"),
                      plotlyOutput("barchart", width = "90%", height = "700px")
                    )
                  ),
                  navbarMenu(
                    "Line plot (Percentage and Mean only)" ,
                    tabPanel(
                      "Base only",
                      plotlyOutput("linePlotBase", width = "90%", height = "700px")
                    ),
                    tabPanel(
                      "Scenario only",
                      plotlyOutput("linePlotSC", width = "90%", height = "700px")
                    ),
                    tabPanel(
                      "Base versus Scenario",
                      plotlyOutput("linePlot", width = "90%", height = "700px")
                    )
                  )
                  
                )
              )))
        )
)
