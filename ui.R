## ui.R ##

shinyUI(dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Machines & humans working together", titleWidth = 400),
  dashboardSidebar(
    sidebarUserPanel("Michael Griffin"),
    br(),
    sidebarMenu(
      menuItem("Intro", 
               tabName = "intro", 
               icon = icon('book-open')),
      br(),
       menuItem(
        "Over time", 
        tabName = "time", 
        icon = icon('calendar')),     
      menuItem(
        "Across industries I",
        tabName = "visuals",
        icon = icon('briefcase')
      ),
      menuItem(
        "Across industries II",
        tabName = "shape",
        icon = icon('briefcase')
      ),
      menuItem(
        'Across states', 
        tabName = "maps", 
        icon = icon('map')),

      menuItem(
        "By capabilities",
        tabName = "skills",
        icon = icon('brain')
      ),
      
      br(),
      menuItem(
        "Data drill-down",
        tabName = "data",
        icon = icon('database')
      ),
      menuItem("Background", 
               tabName = "background", 
               icon = icon('cog')),
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
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(style = "padding: 20px;",
                h1("How vulnerable are US jobs to automation in the medium-term?"),
                em(h3("\"47% could be done by machines \'over the next decade or two\'\"")),
                p("Carl Benedikt Frey and Michael Osborne of Oxford University"),
                br(),
                h3("Project aims"),
                br(),
                h4("1)	Provide a dynamic tool for visualisation research on the possible impact of automation/computerisation of jobs over the medium-term"),
                h4("2)	Update the original 2012 research to latest available information"),
                br(),
                br(),
                p("This dashboard enables users to explore a variety of questions which could be of use to individuals, organisations and policy-makers:"),
                em(
                  p("1)	How many jobs are at risk of computerisation?"),
                  p("2)  How are high risk roles distributed across states and industries?"),
                  p("3)  Is my current / future role likley to be replaced?"),
                  p("4)  Which skills are more or less likley to be automated"),
                  p("5)  How has the landscape changed since the 2012 research?")
                  ),
                br(),
                h3("The caveats"),
                br(),
                p("Making robust predictions is incredibly difficult. This approach focusses on the narrow question of technical feasibility of automation; this ignores the realities of implementation of possible policy mitigation"),
                p("Fuller detailes are provided in the background section and blog post"),
                br(),
                h3("Ongoing work"),
                br(),
                p("Full update to 2018 data with re-run of ML analysis")
                
              
              )),
      tabItem(tabName = "time",
        fluidRow(style = "padding: 20px;",
                 h2("Distribution of jobs by likelihood of automation")),
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
          column(width = 6, box(width=12, plotOutput("time2"))))
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
              fillPage(
                plotOutput("shape", height = "100%", width = "100%"))
              ),      
      tabItem(tabName = "maps",
              fluidRow(style = "padding: 20px;",
                h3("Automation classes by US state")),
              fluidRow(
                column(width=6, box(width=12,htmlOutput("maphigh"))),
                column(width=6, box(width=12,htmlOutput("mapmedium")))
              ),
              fluidRow(
                column(width=6, box(width=12,htmlOutput("maplow"))),
                column(width=6, box(width=12,style = "padding: 20px;",
                                    h4("Observations:"),
                                    p("Jobs losses are focussed in the states with large workforces, unsurprisingly"),
                                    br(),
                                    h4("Approach:"),
                                    p("Plots reflect (absolute) job count within each classification and state")
                                    )))
              ),
      tabItem(tabName = "skills",
              fluidRow(
                column(width=6, box(width=12, plotOutput("skills"))),
                column(width=6, box(width=12, plotOutput("bottleneck")))
              ),
              fluidRow(
                column(width=6, box(width=12, plotOutput("pay"))),
                column(width=6, box(width=12, style = "padding: 20px;",
                                    h4("Observations:"),
                                    p("- Roles at higher risk of automation tend to rely more on physical and sensory abilities whilst low risk roles require higher cognitive levels"),
                                    p("- Higher risk roles tend to have lower annual salaries, suggesting there may be an important class dimension in the impact"),
                                    br(),
                                    h4("Approach:"),
                                    p("- All mean figures reflect a simple count-based mean of all sub-catgories of a segment - these are not weighted by job numbers"),
                                    p("- Nine 'botteneck variables' are proposed and used as the explanatory features to estimate the likelihood of automation")
                                    )))
              ),
      tabItem(tabName = "data",
              fluidRow(style = "padding: 20px;",
                DT::dataTableOutput("table")
              )),
      tabItem(tabName = "background",
              fluidRow(style = "padding: 20px;",
                h2("The author"),
                p("Mike Griffin is training at the NYC data academy and has several years of experience in strategy/analytics roles in finance"),
                p("He studied Natural Sciences (Physics) at the University of Cambridge and Management at the Judge Business School"),
                p("Mike retains a strong passion for science and technology and enjoys sports, boardgames and advantures in nature!"),
                br(),
                br(),
                h2("The project"),
                h3("Code and approach"),
                a("https://github.com/mfgriffin467/Shiny_automation"),
                h3("Datasets"),
                br(),
                p("Detailed jobs data is sourced from annual datasets here:"),
                a("https://www.bls.gov/oes/tables.htm"),
                br(),
                br(),
                p("Predictions are based on outputs from this research:"),
                a("https://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf")
 
              ))
    )
  )
))
