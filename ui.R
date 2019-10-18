## ui.R ##

shinyUI(dashboardPage(
  skin = "purple",
  dashboardHeader(title = "The job landscape of the future?", titleWidth = 350),
  dashboardSidebar(
    width = 250,
    sidebarUserPanel("Michael Griffin"),
    br(),
    sidebarMenu(
      menuItem("Intro", 
               tabName = "intro", 
               icon = icon('book-open')),
      br(),
       menuItem(
        "Headlines", 
        tabName = "time", 
        icon = icon('calendar')
        ),     
      menuItem(
        "Individual jobs",
        tabName = "jobs",
        icon = icon('briefcase')
      ),
      br(),
        menuItem(
        "Across industries I",
        tabName = "visuals",
        icon = icon('briefcase')
      ),
     menuItem(
        'Across states', 
        tabName = "maps", 
        icon = icon('map')),      
     
     menuItem(
        "Across industries II",
        tabName = "shape",
        icon = icon('briefcase')
      ),
     menuItem(
        "By capabilities",
        tabName = "skills",
        icon = icon('brain')
      ),
      
      menuItem(
        "Updated modelling",
        tabName = "model",
        icon = icon('cog')
      ),
      
      br(),
      menuItem(
        "Data drill-down",
        tabName = "data",
        icon = icon('database')
      ),
      menuItem("Background", 
               tabName = "background", 
               icon = icon('book')),
      br(),
      selectizeInput("selected_year",
                     label = "Select year to Display",
                     selected = "2019",
                     choices = choice_year),
      
      pickerInput(
        "selected_state",
        label = "Select state to Display",
        selected = "SELECT ALL",
        choices = choice_state,
        choicesOpt = list('actions-box' = TRUE),
      ),
      pickerInput(
        "selected_job",
        label = "Select job type to Display",
        selected = "SELECT ALL",
        choices = choice_job,
        choicesOpt = list('actions-box' = TRUE)
      )
    )
  ),
  
dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
  ),
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(style = "padding: 20px;",
                h1("How vulnerable are US jobs to automation in the medium-term?"),
                br(),
                em(h3("\"47% could be done by machines \'over the next decade or two\'\"")),
                p("Carl Benedikt Frey and Michael Osborne of Oxford University"),
                br(),
                h2("Project aims"),
                br(),
                h3("1)	Provide a dynamic tool for visualisation research on the possible impact of automation/computerisation of jobs over the medium-term"),
                h3("2)	Update the original 2012 research to latest available information"),
                br(),
                br(),
                h2("Output"),
                br(),
                h4("This dashboard enables users to explore a variety of questions which could be of use to individuals, organisations and policy-makers:"),
                br(),
                em(
                  h4("1) Individuals: is my current or future role likley to be replaced? Are there particular skills to emphasise on to help future-proof careers?"),
                  h4("2) Policy-makers: how many jobs are at risk of computerisation?  How are high risk roles distributed across states and industries?"),
                  h4("3) Businesses: what are the threats and opportunities which arise with the possible changes in job landsdcape? Could retraining employees to adjacent roles make sense?"),
                  h4("4) General - has the forecast or analysis changed since the original 2012 research?")),
                br(),
                h2("Ongoing work"),
                br(),
                h4("Full update to 2018 data with re-run of ML analysis")
                
              
              )),
      tabItem(tabName = "time",
        fluidRow(style = "padding: 20px;",
                 h3("Distribution of jobs by likelihood of automation")),
        fluidRow(
          infoBoxOutput("highBox"),
          infoBoxOutput("mediumBox"),
          infoBoxOutput("lowBox")
        ),
        fluidRow(
          infoBoxOutput("highBoxPerc"),
          infoBoxOutput("mediumBoxPerc"),
          infoBoxOutput("lowBoxPerc")
        ),
        fluidRow(
          column(width = 6, box(width=12, plotOutput("time"))),
          column(width = 6, box(width=12, plotOutput("time2")))
          )
      ),
      tabItem(
        tabName = "jobs",
        fluidRow(style = "padding: 20px;",
                 h3("Modelling of probability of automation across jobs"),
                 h4("Hover over points to invetsigate specific roles")
                 ),
        fillPage(
          plotlyOutput("jobs", height = "100%", width = "100%"))
      ),  
      tabItem(
        tabName = "visuals",
        fluidRow(style = "padding: 20px;",
          h3("Plots of absolute and relative risk classification by job area")),
        fluidRow(box(
          plotOutput("distn"), width = 600, height = 400
        )),
        fluidRow(box(
          plotOutput("distn2"), width = 600, height = 400
        ))
      ),
      tabItem(tabName = "shape",
              fluidRow(style = "padding: 20px;",
                       h3("Breakdown of job types by likelihood of automation")),
              fillPage(
                plotOutput("shape", height = "100%", width = "100%"))
              ),      
      tabItem(tabName = "maps",
              fluidRow(style = "padding: 20px;",
                h3("Breakdown of high/medium/low risk roles by state")),
              fluidRow(
                column(width=6, box(width=12,htmlOutput("maphigh"))),
                column(width=6, box(width=12,htmlOutput("mapmedium")))
              ),
              fluidRow(
                column(width=6, box(width=12,htmlOutput("maplow"))),
                column(width=6, box(width=12,style = "padding: 20px;",
                                    h3("Observations:"),
                                    h4("The concentration of high risk roles varies signficantly by state from ~37% to ~60%"),
                                    h4("This analysis suggests the most vulnerable states are North/South Dakota, Nevada, Oklahama, Mississipi, South Carolina, Arkansas"),
                                    br(),
                                    h3("Approach:"),
                                    h4("Plots reflect relative % split of job count within each state, between high/med/low/unclassified segments")
                                    )))
              ),
      tabItem(tabName = "skills",
              fluidRow(style = "padding: 20px;",
                       h3("Analysis of job characteristics across risk category")),
              fluidRow(
                column(width=6, box(width=12, plotOutput("skills"))),
                column(width=6, box(width=12, plotOutput("bottleneck")))
              ),
              fluidRow(
                column(width=6, box(width=12, plotOutput("pay"))),
                column(width=6, box(width=12, style = "padding: 20px;",
                                    h3("Observations:"),
                                    h4("Roles at higher risk of automation tend to rely more on physical and sensory abilities whilst low risk roles require higher cognitive levels"),
                                    h4("Salary and estimated likelihood of automation are negatively correlated: higher risk roles tend to have lower annual salaries, suggesting there may be an important class dimension in the impact"),
                                    br(),
                                    h3("Approach:"),
                                    h4("Nine 'botteneck variables' are proposed in (1) and used as the explanatory features to estimate the likelihood of automation")
                                    )))
              ),
      tabItem(tabName = "model",
              fluidRow(style = "padding: 20px;",
                       h3("Updated modelling based on latest job profiles")),
              fluidRow(
                column(width = 6, box(width=12, plotOutput("time_mod"))),
                column(width = 6, box(width=12, plotOutput("time2_mod")))
              ),
              fluidRow(style = "padding: 20px;",
                h3("Approach"),
                h4("- Analysis used 2019 skills data from O-NET to capture latest job skill profiles, including new roles addes since 2010"),
                h4("- Use same ground truth labels as 2012 research to train Gaussian classfier model"),
                h4("- Analysis implemented in python using sklearn package with rational quadratic kernel"),
                br(),
                h3("Further work"),
                h4("- Further investigation to align day 1 modelling aginst report - explore detailed kernal and cross-validation approach"),
                h4("- Update skills and models in every year of data")
                
              )
      ),
      tabItem(tabName = "data",
              fluidRow(style = "padding: 20px;",
                DT::dataTableOutput("table")
              )),
      tabItem(tabName = "background",
              fluidRow(style = "padding: 20px;",
                h2("The author"),
                br(),
                h4("Mike Griffin is training at the NYC data academy and has several years of experience in strategy/analytics roles in finance"),
                h4("He studied Natural Sciences (Physics) at the University of Cambridge and Management at the Judge Business School"),
                h4("Mike retains a strong passion for science and technology and enjoys sports, boardgames and advantures in nature!"),
                br(),
                br(),
                h2("The project"),
                br(),
                h4("Code and approach"),
                a("https://github.com/mfgriffin467/Shiny_automation"),
                br(),
                br(),
                h4("Detailed job and capability data is sourced from annual datasets here:"),
                a("https://www.bls.gov/oes/tables.htm"),
                br(),
                a("https://www.onetonline.org/"),
                br(),
                br(),
                h4("Predictions are based on outputs from this research:"),
                a("https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf")
 
              ))
    )
  )
))
