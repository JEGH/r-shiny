##STATS----
stat_value <- reactiveValues(title = '',text = '')

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

df_stats <- reactiveValues(data = data.frame(
  
  KPI = c('Mean Time To Failure (MTTF)', 
          'Mean Time To Critical Failure (MTTCF)',
          'Mean Time Between Failure (MTBF) ',
          'Mean Cost To Repair (MCTR)',
          'Mean Time To Repair (MTTR)'),
  Values = c("~60 days",   "~200 days",     "~48 days",    "~55 k",    "~2 weeks"),
  Actions = shinyInput(actionButton, 5, 'button_', label = "Description", onclick = 'Shiny.onInputChange(\"select_link_description\",  this.id)' ),
  stringsAsFactors = FALSE,
  row.names = 1:5
))


output$table_stats <- DT::renderDataTable(
  df_stats$data, server = FALSE, escape = FALSE, selection = 'none'
)

observeEvent(input$select_link_description, {
  selectedRow <- as.numeric(strsplit(input$select_link_description, "_")[[1]][2])
  if(df_stats$data[selectedRow,"KPI"] == "Mean Time To Failure (MTTF)"){
    stat_value$title <<- paste('Mean Time To Failure')
    stat_value$text <<-  paste('Denotes the expected time to failure for a non-repairable system')
  }
  else if(df_stats$data[selectedRow,"KPI"] == "Mean Time To Critical Failure (MTTCF)"){
    stat_value$title <<- paste('Mean Time To Critical Failure')
    stat_value$text <<- paste('Denotes the expected time to failure for a non-repairable system.
                              Series MTTCF, or simply MTCF typically includes all failures without regard 
                              to any fault tolerance that may exist, whereas, the “C” in MTTCF indicates that only “critical” 
                              failures are counted, i.e., those that will cause the system to not meet specification requirements')
  }
  else if(df_stats$data[selectedRow,"KPI"] == "Mean Time Between Failure (MTBF)"){
    stat_value$title <<- paste('Mean Time Between Failure')
    stat_value$text <<- paste('Is the predicted elapsed time between inherent failures of a Inductions Motors, during normal system operation. MTBF can be calculated 
                              as the arithmetic mean (average) time between failures of a system')
  }
  else if(df_stats$data[selectedRow,"KPI"] == "Mean Cost To Repair (MCTR)"){
    stat_value$title <<- paste('Mean Cost To Repair')
    stat_value$text <<- paste(' Operations can be measured by production stops and compared 
                              to the amount of time to get the production back to desired capacity.
                              This metric includes production or quality loss while out of commission; 
                              scrap created by the failure; labor, parts, and services to repair; 
                              and other costs to return to desired production levels')
  }
  else if(df_stats$data[selectedRow,"KPI"] == "Mean Time To Repair (MTTR)"){
    stat_value$title <<- paste('Mean Time To Repair')
    stat_value$text <<- paste('Is a basic measure of the maintainability of repairable items. 
                              It represents the average time required to repair a failed component or device.
                              [1] Expressed mathematically, it is the total corrective maintenance time for failures 
                              divided by the total number of corrective maintenance actions for failures during a given period of time.
                              [2]')
  }
  #stat_value$detail <<- paste('click on ',df_stats$data[selectedRow,1])
  })


output$description_title <- renderText({
  
  stat_value$title
  
})

output$description_text <- renderText({
  
  stat_value$text
  
})

#Mean Time To Failure
output$gauge_MTTF = renderGauge({
  gauge(
    input$value_MTTF,
    min = 0,
    max = 100,
    sectors = gaugeSectors(
      success = c(50, 100),
      warning = c(30, 50),
      danger = c(0, 30)
    )
  )
})

#Mean Time To Critical Failure
output$gauge_MTTCF = renderGauge({
  gauge(
    input$value_MTTCF,
    min = 0,
    max = 100,
    sectors = gaugeSectors(
      success = c(50, 100),
      warning = c(30, 50),
      danger = c(0, 30)
    )
  )
})


#Mean Time Between Failure
output$gauge_MTBF = renderGauge({
  gauge(
    input$value_MTBF,
    min = 0,
    max = 100,
    sectors = gaugeSectors(
      success = c(50, 100),
      warning = c(30, 50),
      danger = c(0, 30)
    )
  )
})


#Mean Cost To Repair
output$gauge_MCTR = renderGauge({
  gauge(
    input$value_MCTR,
    min = 0,
    max = 100,
    sectors = gaugeSectors(
      success = c(50, 100),
      warning = c(30, 50),
      danger = c(0, 30)
    )
  )
})

#Mean Time To Repair
output$gauge_MTTR = renderGauge({
  gauge(
    input$value_MTTR,
    min = 0,
    max = 100,
    sectors = gaugeSectors(
      success = c(50, 100),
      warning = c(30, 50),
      danger = c(0, 30)
    )
  )
})


