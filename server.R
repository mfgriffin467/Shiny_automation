## server.R ##
shinyServer(function(input, output, session) {

scaleFUN <- function(x) sprintf("%.2f", x)
  
  
# Generic tables ####

#Create filtered summary for histogram
data_all_filter  = reactive({
if (input$selected_state == "SELECT ALL" & input$selected_job == "SELECT ALL") {
  data_df[data_df$YEAR == input$selected_year,]
} else if (input$selected_state == 'SELECT ALL') {
  data_df[data_df$YEAR == input$selected_year & data_df$top_level_job_category_desc == input$selected_job,]
} else if (input$selected_job == 'SELECT ALL') {
  data_df[data_df$YEAR == input$selected_year & data_df$STATE == input$selected_state,]
} else
  data_df[data_df$YEAR == input$selected_year & data_df$top_level_job_category_desc == input$selected_job & data_df$STATE == input$selected_state ,]
})


# Create all jobs df with year select all option, optional jobs and state 
data_all_year_filter  = reactive({
  if (input$selected_state == "SELECT ALL" & input$selected_job == "SELECT ALL") {
    data_df
  } else if (input$selected_state == 'SELECT ALL') {
    data_df[data_df$top_level_job_category_desc == input$selected_job,]
  } else if (input$selected_job == 'SELECT ALL') {
    data_df[data_df$STATE == input$selected_state,]
  } else
    data_df[data_df$top_level_job_category_desc == input$selected_job & data_df$STATE == input$selected_state,]
})

# Create all jobs df with state select all option, optional jobs 
data_job_filter  = reactive({
  if (input$selected_job == "SELECT ALL") {
    data_df[data_df$YEAR == input$selected_year,]
} else
    data_df[data_df$YEAR == input$selected_year & data_df$top_level_job_category_desc == input$selected_job,]
})

# Create all jobs df with jobs select all option, optional states 
data_state_filter  = reactive({
  if (input$selected_state == "SELECT ALL") {
    data_df[data_df$YEAR == input$selected_year,]
  } else
    data_df[data_df$YEAR == input$selected_year & data_df$STATE == input$selected_state,]
})

# Create filtered table, all jobs select state
summary_filter_data_df = reactive({
  data_state_filter() %>% 
  group_by(prob_automation_class, top_level_job_category_desc) %>%
  summarise(current_jobs = sum(TOT_EMP))
})


# Info boxes ####

output$highBox = renderInfoBox({    
  max_value = data_all_filter() %>% 
              filter(prob_automation_class == "1) High" ) %>% 
              summarise(round(sum(TOT_EMP)/10**6),digits=2)
              infoBox(color = "red",
                      title = "# jobs millions",
                      subtitle = "in selection",
                      max_value, 
                      icon = icon("hand-o-down")) 
  })


  output$mediumBox = renderInfoBox({    
    max_value = data_all_filter() %>% 
      filter(prob_automation_class == "2) Medium" ) %>% 
      summarise(round(sum(TOT_EMP)/10**6),digits=2)
    infoBox(color = "orange",
            title = "# jobs millions",
            subtitle = "in selection",
            max_value, 
            icon = icon("exchange-alt")) 
  })
  
  output$lowBox = renderInfoBox({    
    max_value = data_all_filter() %>% 
      filter(prob_automation_class == "3) Low" ) %>% 
      summarise(round(sum(TOT_EMP)/10**6),digits=2)
    infoBox(color = "green",
            title = "# jobs millions",
            subtitle = "in selection",
            max_value, 
            icon = icon("hand-o-up")) 
  })
  
  output$highBoxPerc = renderInfoBox({    
    subset = data_all_filter() %>% 
      filter(prob_automation_class == "1) High" ) %>% 
      summarise(jobs= sum(TOT_EMP))
    
    total = data_all_filter() %>% 
      summarise(jobs= sum(TOT_EMP))
    
    value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
    infoBox(color = "red",
            title = "% jobs at high risk",
            subtitle = "in selection",
            value, 
            icon = icon("hand-o-down")) 
  })
  
  output$mediumBoxPerc = renderInfoBox({    
    subset = data_all_filter() %>% 
      filter(prob_automation_class == "2) Medium" ) %>% 
      summarise(jobs= sum(TOT_EMP))
    
    total = data_all_filter() %>% 
      summarise(jobs= sum(TOT_EMP))
    
    value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
    infoBox(color = "orange",
            title = "% jobs at medium risk",
            subtitle = "in selection",
            value, 
            icon = icon("exchange-alt")) 
  })
  
  output$lowBoxPerc = renderInfoBox({    
    subset = data_all_filter() %>% 
      filter(prob_automation_class == "3) Low" ) %>% 
      summarise(jobs= sum(TOT_EMP))
    
    total = data_all_filter() %>% 
      summarise(jobs= sum(TOT_EMP))
    
    value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
    infoBox(color = "green",
            title = "% jobs at low risk",
            subtitle = "in selection",
            value, 
            icon = icon("hand-o-up")) 
  })
    
# Render tables ####
  
  
output$table = DT::renderDataTable({
    datatable(data_all_filter() %>% 
              select(keep_columns) %>%
              rename("Job count" = TOT_EMP, "Av annual salary" = A_MEAN, "Low level job group" = major_job_description, 
                     "High level job group" = top_level_job_category_desc, "Likelihood of automation" = prob_automation_class,
                     "Cognitive ability requirements" = cognitive_abilities, "Physical ability requirements" = physical_abilities,
                     "Psychomotor ability requirements" = psychomotor_abilities, "Sensory ability requirements" = sensory_abilities) %>% 
              arrange(desc(Probability)),
              caption = "Use search bar to explore detail with state/type bucket",
              options = list(pageLength=5))
  })

  

# Render charts ####
  

output$time = renderPlot(data_all_year_filter() %>%
                         group_by(YEAR,prob_automation_class) %>% 
                         summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
                         ggplot() 
                          + geom_area(aes(x=YEAR,y=current_jobs,fill=prob_automation_class))
                          + xlab("Jobs over time")
                          + ylab("Count of US jobs (millions)")
                          + scale_y_continuous(labels=scaleFUN)
                          + ggtitle("Absolute figures")
                          + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
                          + theme(text = element_text(size = 20),legend.position = "bottom"))

output$time2 = renderPlot(data_all_year_filter() %>%
                           group_by(YEAR,prob_automation_class) %>% 
                           summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
                           ggplot() 
                         + geom_area(aes(x=YEAR,y=current_jobs,fill=prob_automation_class),position = "fill")
                         + xlab("Jobs over time")
                         + ylab("Proportion of US jobs (millions)")
                         + scale_y_continuous(labels = scales::percent)
                         + ggtitle("Relative proportions")
                         + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
                         + theme(text = element_text(size = 20),legend.position = "bottom"))

output$maphigh <- renderGvis({
    gvisGeoChart(data = data_job_filter() %>%  
                filter(prob_automation_class == "1) High") %>% 
                group_by(STATE) %>%
                summarise(current_jobs = sum(TOT_EMP)/10**6),
                locationvar = "STATE",
                colorvar = "current_jobs",
                options = list(
                  region = "US",
                  displayMode = "regions",
                  resolution = "provinces",
                  colorAxis="{colors:['white', 'red']}",
                  width = "700",
                  height = "350"
                  )
    )
  })

output$mapmedium <- renderGvis({
  gvisGeoChart(data = data_job_filter() %>%  
                 filter(prob_automation_class == "2) Medium") %>% 
                 group_by(STATE) %>%
                 summarise(current_jobs = sum(TOT_EMP)/10**6),
               locationvar = "STATE",
               colorvar = "current_jobs",
               options = list(
                 region = "US",
                 displayMode = "regions",
                 resolution = "provinces",
                 colorAxis="{colors:['white', 'orange']}",
                 width = "700",
                 height = "350"
               )
  )
})

output$maplow <- renderGvis({
  gvisGeoChart(data = data_job_filter() %>%  
                 filter(prob_automation_class == "3) Low") %>% 
                 group_by(STATE) %>%
                 summarise(current_jobs = sum(TOT_EMP)/10**6),
               locationvar = "STATE",
               #header = 'test',
               colorvar = "current_jobs",
               #title = "Absolute job count at high risk, within selected sector",
               options = list(
                 title = "Test",
                 region = "US",
                 displayMode = "regions",
                 resolution = "provinces",
                 colorAxis="{colors:['white', 'green']}",
                 width = "700",
                 height = "350"
               )
  )
})

  
output$shape = renderPlot(
  data_state_filter() %>% 
  filter(!is.na(Probability)) %>%  ####HACK######################################
  group_by(rounded_prob, top_level_job_category_desc) %>%
  summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
  ggplot()
    + geom_histogram(aes(x=rounded_prob,weight=current_jobs,fill=top_level_job_category_desc),bins=40)
    + geom_vline(aes(xintercept = 0.3),color = "green")
    + geom_vline(aes(xintercept = 0.7),color = "red")
    + xlab("Estimated likelihood of role computerisation")
    + scale_x_continuous(labels = scales::percent)
    + ylab("Count of US jobs (millions)")
    + scale_y_continuous(labels=scaleFUN)
    + ggtitle("Distribution of jobs by likelihood of automation, within segment")
    + theme_minimal()    
    + theme(text = element_text(size = 20),legend.position = "bottom")
    + scale_fill_brewer(palette = "Spectral",name="Job category"),
  height = 800,
  width = 1600
)
  
  
output$distn = renderPlot(
  ggplot(data = summary_filter_data_df())
  + geom_col(aes(x = reorder(top_level_job_category_desc,-current_jobs),
      y = current_jobs/10**6,
      fill = prob_automation_class
    ))
  + theme_minimal()
  + ylab("Count of US jobs (millions)")
  + scale_y_continuous(labels=scaleFUN)
  + theme(text = element_text(size = 20),axis.text.x=element_blank(),axis.title.x=element_blank(),legend.position = "top")
  + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
  + scale_x_discrete(
    labels = function(x)
    lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse = "\n"))
  )
  

  
output$distn2 = renderPlot(
  ggplot(data = summary_filter_data_df()) + xlab("Job group") + geom_col(
    aes(x = reorder(top_level_job_category_desc,-current_jobs),
        y = current_jobs,
        fill = prob_automation_class),
    position = "fill") 
  + theme_minimal()
  + ylab("Proportion by class")
  + scale_y_continuous(labels = scales::percent)
  + theme(text = element_text(size = 20),legend.position = "none")
  + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
  + scale_x_discrete(
      labels = function(x)
      lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse = "\n"))
  )




output$skills = renderPlot(data_all_filter() %>%
                             group_by(prob_automation_class) %>%
                             summarise(cognitive = mean(cognitive_abilities),
                                       physical = mean(physical_abilities),
                                       pyschomotor = mean(psychomotor_abilities),
                                       sensory = mean(sensory_abilities)) %>% 
                             gather("skill","level",-prob_automation_class) %>%
                             ggplot(aes(x=prob_automation_class,fill=skill,y=level))
                           + geom_col(stat="identity",position="dodge")
                           + xlab("Likelihood of computerisation")
                           + scale_fill_brewer(palette = "Blues")     
                           + theme_minimal()
                           + theme(text = element_text(size = 20))
                           + ggtitle("Ability breakdown")
)

output$bottleneck = renderPlot(data_all_filter() %>%
                                 group_by(prob_automation_class) %>%
                                 summarise(fine_arts = mean(fine_arts),
                                           finger_dexterity = mean(finger_dexterity),
                                           manual_dexterity = mean(manual_dexterity),
                                           negotiation = mean(Negotiation),
                                           originality = mean(Originality),
                                           persuasion = mean(Persuasion),
                                           social_perceptiveness = mean(social_perceptiveness),
                                           assisting_and_caring = mean(assisting_and_caring),
                                           cramped_work_space = mean(cramped_work_space)
                                           ) %>% 
                                 gather("skill","level",-prob_automation_class) %>%
                                 ggplot(aes(x=prob_automation_class,fill=skill,y=level))
                               + geom_col(stat="identity",position="dodge")
                               + xlab("Likelihood of computerisation")
                               + scale_fill_brewer(palette = "Pastel1")     
                               + theme_minimal()
                               + theme(text = element_text(size = 20))
                               + ggtitle("Bottleneck features")
)         

output$pay = renderPlot(data_all_filter() %>%
                                 group_by(prob_automation_class) %>%
                                 summarise(mean_salary = mean(A_MEAN)) %>% 
                                 ggplot(aes(x=prob_automation_class,y=mean_salary))
                               + geom_col()
                              + xlab("Likelihood of computerisation")
                              + ylab("Mean annual salary within bucket")
                              + scale_fill_brewer(palette = "Pastel1")     
                               + theme_minimal()
                               + theme(text = element_text(size = 20))
                               + ggtitle("Salary across automation classes")
)         


output$time_mod = renderPlot(data_all_year_filter() %>%
                           group_by(YEAR,prob_class_new) %>% 
                           summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
                           ggplot() 
                         + geom_area(aes(x=YEAR,y=current_jobs,fill=prob_class_new))
                         + xlab("Jobs over time")
                         + ylab("Count of US jobs (millions)")
                         + scale_y_continuous(labels=scaleFUN)
                         + ggtitle("Absolute figures")
                         + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
                         + theme(text = element_text(size = 20),legend.position = "bottom"))

output$time2_mod = renderPlot(data_all_year_filter() %>%
                            group_by(YEAR,prob_class_new) %>% 
                            summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
                            ggplot() 
                          + geom_area(aes(x=YEAR,y=current_jobs,fill=prob_class_new),position = "fill")
                          + xlab("Jobs over time")
                          + ylab("Proportion of US jobs (millions)")
                          + scale_y_continuous(labels = scales::percent)
                          + ggtitle("Relative proportions")
                          + scale_fill_brewer(palette = "Spectral",name="Likelihood of automation")
                          + theme(text = element_text(size = 20),legend.position = "bottom"))


})