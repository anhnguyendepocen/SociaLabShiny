library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyBS)

options(shiny.maxRequestSize = 10000 * 1024 ^ 2)


dbHeader <- dashboardHeader(title = "SociaLab"
                            # ,
                            # tags$li(
                            #   a(href = 'https://drive.google.com/open?id=0ByGI4aqoCDeldHdNaDBLQzFLXzg',
                            #     "User guide", target = "_blank"),
                            #   class = "dropdown",
                            #   tags$style(".main-header {max-height: 58px}"),
                            #   tags$style(".main-header .logo {height: 60px}")
                            # )
                            )

dSiderBar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("First Page", tabName = "fb", icon = icon("home")),
    menuItem("Conceptual Model", tabName = "mi", icon = icon("line-chart")),
    br(),
    box(
      title = "Scenarios Run",
      background = "black",
      status = "danger",
      solidHeader = TRUE,
      uiOutput("selectSB"),
      width = 12
    ),
    br(), br(), br(), br(),
    br(), br(), br(), br(),
    br(), br(),
    menuItem("Table Builder", tabName = "tb", icon = icon("table")),
    br(),
    box(
      h5("Latest Update:"),
      h5("2018-09-20"),
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
  title = "SociaLab",
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
    tabItems(tabItem("fb",
       box(
        width = 12,
        tags$head(includeScript("google-analytics.js")),
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
          Census data (produced by Statistics NZ) as the source for a starting 
          sample (1% of 1981 census population)
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
          "We ask such pertinent questions here in a series of scenarios the results for which can
          be interrogated and visualised via this Shiny app, an accompaniment to our book 
          \"imulating Societal Change - Counterfactual Modelling for Social and Policy Inquiry\" -
          Peter Davis & Roy Lay-Yee (Springer 2018)."
        ),
        
        p("The Shiny app enables the user to customise downloadable tables or graphs to: view factor 
          distributions from the base simulation; view results of a selected scenario; and compare 
          between the base and scenario. "),
        p("The left-hand menu contains selectable items: 
          \"First Page\" - brief description of the model and app;
          \"Conceptual Model\" - schematic of the underlying framework; 
          \"Scenarios Run\" - drop-down list of pre-run scenarios; and 
          \"Table Builder\" – display of results. "),
        p("Selecting \"Table Builder\" opens two panels. The near panel allows customising the display: 
            choosing an \"outcome\" of interest; optionally specifying a sub-group and/or by-group; choosing 
            the summary measure as percentage or mean (or count) with (optional) 95% confidence limits; and 
            to download a table or plot. The far panel displays the actual results - for the base, the scenario,
            or their comparison – that can be further customised, using the task-bar, 
          in the form of a table, a bar chart or a line plot."),
        h4("Disclaimer:"),
        
        p("The results in this Shiny application are not official statistics They have been created 
            for research purposes. The opinions, findings, recommendations, and conclusions expressed in this
          Shiny application are those of
          the author(s), not Statistics NZ. Access to the anonymised data used in this study was provided by 
          Statistics NZ under
          the security and confidentiality provisions of the Statistics Act 1975. Only people
          authorised by the Statistics Act 1975 are allowed to see data about a particular person, household, 
          business, or organisation, and the results in this Shiny application have been confidentialised to 
          protect these groups from identification and to keep their data safe. Careful consideration has been
          given to the privacy, security, and confidentiality issues associated
          with using administrative and survey data. Further detail can be found in the Privacy impact
          assessment available from www.stats.govt.nz."),
        
        h4("To cite this application, please use the following,"),
        p(
          "Davis, P., Lay-Yee, R., Chang, K., von Randow, M., (2018) Shiny application: New Zealand as a Social Laboratory.",  
          a("https://compassnz.shinyapps.io/SociaLabShiny/", href = "https://compassnz.shinyapps.io/SociaLabShiny/")
        ),
        h4("The source code is stored in two places:"),
        HTML(
          "<ul>
          <li>Simario R package is at: <a href = \"https://github.com/kcha193/simarioV2\"> https://github.com/kcha193/simarioV2 </a>.</li>
          <li>Shiny application is at: <a href = \"https://github.com/kcha193/SociaLabShiny\"> https://github.com/kcha193/SociaLabShiny </a>.</li>
          </ul>"
        ),
        p(""),
        a(
          href = "http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html",
          img(src = "http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass/_jcr_content/par/contentblock_1919153551/par/image.img.png/1529547380837.png",
              width = 200)
        ),
        br(),
        br()
        )
        ),
       
       tabItem("mi", 
               fluidRow(
                 box(title ="", width = 12,
                     status = "success",solidHeader = TRUE,
                     tags$head(includeScript("google-analytics.js")),
                     img(src='Picture1.png', align = "left", 
                         height="550", width="930")))),
       
      tabItem("tb",
              # Sidebar with a slider input for the number of bins
              fluidRow(
                
                column(width = 3,
                box(
                  title = "Scenario description",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE, 
                  tags$head(includeScript("google-analytics.js")),
                  dataTableOutput("scenDes")),
                box(
                  title = "Variable",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput(
                    "input_type_TB",
                    HTML("<b> <font size=\"4\">STEP 1: </font></b> Select Summary Measure"),
                    choices = c("Percentage", "Count", "Mean"), selected = "Percentage"
                  ),
                  
                  uiOutput("uiTB"),
                  uiOutput("uiVar"),
                  uiOutput("uiSubGrpTB"),
                  #uiOutput("uiExprTB"),
                  uiOutput("uiExprTB1"),
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
                )),
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
