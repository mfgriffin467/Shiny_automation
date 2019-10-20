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


# Create shared frequency table for maps

job_states = reactive({
  data_job_filter() %>% 
    group_by(STATE) %>%
    summarise(n_state=n())
})

job_prob_states = reactive({
  data_job_filter() %>% 
    group_by(prob_automation_class,STATE) %>%
    summarise(n_state_prob=n())
})

job_maps = reactive({
  left_join(job_prob_states(),job_states(),by="STATE") %>% 
  mutate(proportion = n_state_prob/n_state)
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
            icon = icon("percent")) 
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
            icon = icon("percent")) 
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
            icon = icon("percent")) 
  })
    

output$newBoxPerc = renderInfoBox({    
  subset = data_all_filter() %>% 
    filter(prob_automation_class == "4) New / Unclassified" ) %>% 
    summarise(jobs= sum(TOT_EMP))
  
  total = data_all_filter() %>% 
    summarise(jobs= sum(TOT_EMP))
  
  value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
  infoBox(color = "blue",
          title = "% in new or unclassified jobs",
          subtitle = "in selection",
          value, 
          icon = icon("percent")) 
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
  
# Headline time stats

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
                          + theme(text = element_text(size = 20),legend.position = "bottom")
                         )

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
                         + theme(text = element_text(size = 20),legend.position = "bottom")                         )


# Jobs page

color_map <- c("1) High" = "b) high risk", "2) Medium"="c) medium risk", "3) Low" = "a) low risk")
# hack relies on alphabetic ordering


# Aggregate job views

output$jobs = renderPlotly({
                        data_all_filter() %>%
                        group_by(OCC_TITLE, Probability, prob_automation_class) %>%
                        summarise(job_number = sum(TOT_EMP)) %>%
                        select(OCC_TITLE,job_number,Probability,prob_automation_class) %>% 
                        plot_ly(x = ~ Probability, y = ~job_number,text = ~ OCC_TITLE, color= ~ color_map[prob_automation_class]) %>% 
          #              plot_ly(x = ~ Probability, y = ~job_number, color= ~ color_map[prob_automation_class]) %>% 
          #              add_text(text = ~ OCC_TITLE) %>% 
                        add_markers() %>% 
                        layout(height = 700,width = 1600)
                          })


# Headline stats by industry 1

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
      lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse = "\n")),
  height = 350
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
      lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse = "\n")),
  height=350
)


# Across states map

output$maphigh <- renderGvis({
    gvisGeoChart(data = job_maps() %>%  
                filter(prob_automation_class == "1) High") %>% 
                select(STATE,proportion),
                locationvar = "STATE",
                colorvar = "proportion",
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
  gvisGeoChart(data = job_maps() %>%  
                 filter(prob_automation_class == "2) Medium") %>% 
                 select(STATE,proportion),
               locationvar = "STATE",
               colorvar = "proportion",
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
  gvisGeoChart(data = job_maps() %>%  
                 filter(prob_automation_class == "3) Low") %>% 
                 select(STATE,proportion),
               locationvar = "STATE",
               colorvar = "proportion",
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


# Across industries II tab
  
output$shape = renderPlot(
  data_state_filter() %>% 
  filter(!is.na(Probability)) %>% 
  group_by(rounded_prob, top_level_job_category_desc) %>%
  filter(top_level_job_category_desc != "Farming, Fishing, and Forestry") %>%  # Remove tiny segment for clean visualisation
  summarise(current_jobs = sum(TOT_EMP)/10**6) %>% 
  ggplot()
    + geom_histogram(aes(x=rounded_prob,weight=current_jobs,fill=top_level_job_category_desc),bins=40)
    + geom_vline(aes(xintercept = 0.3),color = "green")
    + geom_vline(aes(xintercept = 0.7),color = "red")
    + xlab("Estimated likelihood of role computerisation")
    + scale_x_continuous(labels = scales::percent)
    + ylab("Count of US jobs (millions)")
    + scale_y_continuous(labels=scaleFUN)
    + theme_minimal()    
    + theme(text = element_text(size = 20),legend.position = "bottom")
    + scale_fill_brewer(palette = "Spectral",name="Job category"),
  height = 700,
  width = 1600
)
  


# Job characteristics 

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
                           + theme(text = element_text(size = 20)),
                           height=350
#                           + ggtitle("Ability breakdown")
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
                               + theme(text = element_text(size = 20)),
                               height=350
#                               + ggtitle("Bottleneck features")
)         

output$pay = renderPlot(data_all_filter() %>%
                          mutate(salary_weighted = A_MEAN * TOT_EMP ) %>% 
                          group_by(OCC_CODE, Probability) %>% 
                          summarise(Mean_salary_k = sum(salary_weighted)/sum(TOT_EMP)/10**3) %>% 
                          ggplot(aes(x=Probability,y=Mean_salary_k))
                        + geom_point() + geom_smooth(method = 'lm')
                        + xlab("Likelihood of computerisation")
                        + ylab("Mean annual salary k")
                        + scale_fill_brewer(palette = "Pastel1")     
                        + theme_minimal()
                        + theme(text = element_text(size = 20)),
                        height =350
#                        + ggtitle("Salary across automation classes")
)         

#New modelling tab

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

output$highBoxPercMod = renderInfoBox({    
  subset = data_all_filter() %>% 
    filter(prob_class_new == "1) High" ) %>% 
    summarise(jobs= sum(TOT_EMP))
  
  total = data_all_filter() %>% 
    summarise(jobs= sum(TOT_EMP))
  
  value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
  infoBox(color = "red",
          title = "% jobs at high risk",
          subtitle = "in selection",
          value, 
          icon = icon("percent")) 
})

output$mediumBoxPercMod = renderInfoBox({    
  subset = data_all_filter() %>% 
    filter(prob_class_new == "2) Medium" ) %>% 
    summarise(jobs= sum(TOT_EMP))
  
  total = data_all_filter() %>% 
    summarise(jobs= sum(TOT_EMP))
  
  value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
  infoBox(color = "orange",
          title = "% jobs at medium risk",
          subtitle = "in selection",
          value, 
          icon = icon("percent")) 
})

output$lowBoxPercMod = renderInfoBox({    
  subset = data_all_filter() %>% 
    filter(prob_class_new == "3) Low" ) %>% 
    summarise(jobs= sum(TOT_EMP))
  
  total = data_all_filter() %>% 
    summarise(jobs= sum(TOT_EMP))
  
  value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
  infoBox(color = "green",
          title = "% jobs at low risk",
          subtitle = "in selection",
          value, 
          icon = icon("percent")) 
})


output$newBoxPercMod = renderInfoBox({    
  subset = data_all_filter() %>% 
    filter(prob_class_new == "4) New / Unclassified" ) %>% 
    summarise(jobs= sum(TOT_EMP))
  
  total = data_all_filter() %>% 
    summarise(jobs= sum(TOT_EMP))
  
  value = 100* round(sum(subset$jobs)/sum(total$jobs),digits=3)
  infoBox(color = "blue",
          title = "% in new or unclassified jobs",
          subtitle = "in selection",
          value, 
          icon = icon("percent")) 
}) 


})