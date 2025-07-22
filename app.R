# Load required libraries
library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(pdftools)
install.packages("pdftools", repo="https://cloud.r-project.org/")
library(lubridate)

PageTitle <- div(img(src="img1.png", height = 40, width = 40), 
                 span("BNT211 Prodigy sensor data plotter"),
                 style={'text-align:center;'})

Footer <- div(
  a(href = "mailto:dominik.ternes@biontech.de?subject=Sensor%20Data%20Plotter",
    img(src = "img2.png", height = 50, width = 200),
  ),
  style = 'text-align:center; padding-bottom:20px;'
)

BrowserTitle <- "BNT211 Trending data plotter"
theme <- bs_theme(version = 4, preset = "minty")

options(shiny.maxRequestSize = 100 * 1024^2)

# Define functions

TCT_Batch_id_extract <- function(input, output){
  #Get batch_id from pdf
  output <- data.frame(Text = unlist(strsplit(pdf_text(input), "\n", fixed = TRUE))) %>%  #Read Protocol pdf
  filter(grepl("^Info 1 product", Text)) %>% #Filter all lines starting with a Date, which are matrix and operator events
  separate(Text, into = c("Info", "batch_id"), sep = "\\s{2,}") %>% 
  pull(batch_id)
}

TCT_Pdf_extract <- function(input, output){
  #Get data from pdf and extract relevant event info
  outout <- data.frame(Text = unlist(strsplit(pdf_text(input), "\n", fixed = TRUE))) %>% #Read Protocol pdf
    filter(grepl("^\\d{4}-\\d{2}-\\d{2}", Text)) %>% #Filter all lines starting with a Date, which are matrix and operator events
    separate(Text, into = c("Date", "Time", "Operator", "Process"), sep = "\\s{2,}") %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
    mutate(Day = as.integer(Date - min(.$Date)))
}

TCT_log_event <- function(input, input2, output){
  #Extract starting date and convert it into ms
  starting_time <- first(input$Time)
  starting_ms <- first(input$Time_ms)
  starting_time_ms <- as.numeric(hms(starting_time))*1000 #use lubridate package to convert time into ms
  starting_time_ms_offset <- starting_time_ms - starting_ms #calculate offset to find out when the device started counting
  
  #Merge with data and calculate ms relative to starting date and ms
  output <- right_join(input, input2) %>% 
    distinct(Time, .keep_all = T) %>% 
    mutate(Time_ms = ifelse(is.na(Time_ms), as.numeric(hms(.$Time))*1000, Time_ms)) %>% #convert time into ms
    mutate(Time_ms = ifelse(is.na(Day_event) & Day == 0, 
                            Time_ms-starting_time_ms_offset, #calculate ms for day 0 from time
                            ifelse(is.na(Day_event) & Day > 0, Time_ms+Day*as.numeric(hms("24:00:00"))*1000-starting_time_ms_offset, Time_ms))) %>% #calculate ms for day >0 from time
    select(Time_ms, Time, Day, Process) %>% 
    mutate(Time_h = round(Time_ms/3600000, 3)) %>% 
    mutate(Event_type = ifelse(grepl("Take Sample", Process), "Sampling",
                               ifelse(Process %in% c("Feed (port: 3, vol (+): 30)", 
                                                     "Activate shaker (shaker type 2)",
                                                     "Deactivate shaker",
                                                     "Volume reduction (port: 3, vol = 70)",
                                                     "Transduction (reagent vol.: 10)",
                                                     "Spin (120 min, 32 C)",
                                                     "Medium Bag changed",
                                                     "Culture wash (cycles: 1)",
                                                     "Activate shaker (shaker type 2)",
                                                     "Media exchange (port: 3, vol (-/+): 130/180)",
                                                     "Harvest"), "Matrix", "Others"))) %>% 
    mutate(Event_text = ifelse(Process == "Feed (port: 3, vol (+): 30)", "Feed", 
                               ifelse(Process == "Activate shaker (shaker type 2)", "Shaker on", 
                                      ifelse(Process == "Deactivate shaker", "Shaker off", 
                                             ifelse(Process == "Volume reduction (port: 3, vol = 70)", "Vol. red.", 
                                                    ifelse(Process == "Transduction (reagent vol.: 10)", "Transduction", 
                                                           ifelse(Process == "Spin (120 min, 32 C)", "Spin", 
                                                                  ifelse(Process == "Medium Bag changed", "Medium bag exchange", 
                                                                         ifelse(Process == "Culture wash (cycles: 1)", "Culture wash", 
                                                                                ifelse(Process == "Activate shaker (shaker type 2)", "Shaker on", 
                                                                                       ifelse(Process == "Media exchange (port: 3, vol (-/+): 130/180)", "Media exchange", 
                                                                                              ifelse(Process == "Harvest", "Harvest", 
                                                                                                     ifelse(Process == "Take Sample", "Sample", Process)))))))))))))
  
}

# Define UI
ui <- fluidPage(theme = bs_theme_update(theme, primary = "#BECD32", secondary = "#005A64", 
                                        font_scale = NULL, spacer = "1rem", preset = "minty"),
                titlePanel(PageTitle, windowTitle = BrowserTitle),
                tags$head(tags$style(
                  HTML('.custom-sidebar {margin-top: 30px;}'))),
                sidebarLayout(
                  sidebarPanel(
                    class = "custom-sidebar",
                    
                    fluidRow(
                      column(12, align = "center",
                             HTML("<p style='font-size: 20px;'> <b>How to plot</b> </p>")
                      )
                    ),
                    
                    HTML("<p style='font-size: 14px;'> Upload the TCT <i>Protocol.pdf</i>, the <i>tec_log.csv</i> and the <i>ps_log.csv</i> from the exported TCT protocol folder of the Prodigy device. 
                    You can upload and plot/merge multiple batches. Using the <i>Show events</i> checkboxes, the Activity Matrix, sampling time points and or any other operator interaction can be added to the plot. Report bugs, click footer.</p>
                    <p style='margin-top: -10px'> Runtime info </p>
                    <p style='font-size: 14px; margin-top: -15px'> <i>tec_log</i>: ~2E+06 datapoints ~15 sec/batch.<br><i>ps_log</i>: ~2E+07 datapoints ~30 sec/batch. </p>
                    <p style='font-size: 12px; margin-top: -10px; text-align: center;' title='This Shiny web application is a tool designed for analyzing CliniMACS Prodigy sensor data. Users can effortlessly upload temperature and pressure sensor data files, and the app enables them to interactively plot this data while overlaying events. This feature offers a clear visual representation of specific occurrences in the sensor data.'> General Info About This App. &#9432; </p>
                         <p></p>"),
                    
                    #textInput("batch_id", "Enter Batch ID (if any):"),
                    fileInput("file_tct", "Choose TCT Protocol file", accept = ".pdf"),
                    
                    div(
                      HTML("<p style='font-size: 14px; margin-top: -20px; margin-bottom: 0px'> Show events: </p>"),
                      fluidRow(
                        column(4, checkboxInput("matrix", HTML("<span style='font-size: 12px; vertical-align: 0.185em;'>Activity Matrix &#9632</span>")), style = "color: #005A64;"), 
                        column(4, checkboxInput("sampling", HTML("<span style='font-size: 12px; vertical-align: 0.185em;'>Samplings &#9632</span>")), style = "color: #BECD32;"),
                        column(4, checkboxInput("others", HTML("<span style='font-size: 12px; vertical-align: 0.185em;'>Others &#9632</span>")), style = "color: #CCCCFF;"))
                    ),
                    
                    fluidRow(
                      column(12, align = "center",
                             HTML("<p style='font-size: 20px;'> <b>Temperature sensors</b> </p>")
                      )
                    ),
                    
                    fileInput("file", "Choose tec_log File", accept = ".csv"),
                    
                    # Wrap the button in a div with a centered-button class
                    actionButton("plotTempButton", "Plot Temperature", style = "display: block; margin: 0 auto;"),
                    
                    div(
                      style = "text-align: center;",
                      HTML("<p></p>
                      <img src='Temperature_sensors.png' alt='Temperature sensors' style='max-width: 80%; height: auto;'>"
                      )
                    ),
                    
                    fluidRow(
                      column(12, align = "center",
                             HTML("<p style='font-size: 20px; margin-top: 30px;'> <b>Pressure sensors</b> </p>")
                      )
                    ),
                    
                    fileInput("file_ps", "Choose ps_log File", accept = ".csv"),
                    
                    # Wrap the button in a div with a centered-button class
                    actionButton("plotPressureButton", "Plot Pressure", style = "display: block; margin: 0 auto;"),
                    
                    
                    div(
                      style = "text-align: center;",
                      HTML("<p></p>
                      <img src='Pressure_sensors.png' alt='Pressure sensors' style='max-width: 80%; height: auto;'>"
                      )
                    ),
                    
                  ),
                  mainPanel(
                    
                    style = "margin-top: 100px;",
                    
                    div(
                      style = "margin-left: 20px;",  # Adjust the margin as needed
                      plotlyOutput("sensorPlot", width = "100%", height = "500px")
                    ),
                    # Add Chamber_speed plot
                    div(
                      style = "margin-left: 0px;",  # Adjust the margin as needed
                      plotlyOutput("sensorPlot_chamber", width = "100%", height = "150px")
                    ),
                    div(
                      style = "margin-top: 0px;",  # Adjust the margin as needed
                      plotlyOutput("sensorPlot_ps", width = "100%", height = "500px")),
                    # Add Pump_speed plot
                    div(
                      style = "margin-left: 10px;",  # Adjust the margin as needed
                      plotlyOutput("sensorPlot_pump", width = "100%", height = "150px")
                    ),
                  )
                ),
                # Footer
                hr(),
                Footer
)

server <- function(input, output) {
  
  month_mapping <- c(" Jan" = 1, " Feb" = 2, " Mrz" = 3, " Apr" = 4, 
                     " Mai" = 5, " Jun" = 6, " Jul" = 7, " Aug" = 8, " Sep" = 9)
  
  # Initialize a dataframa to store data frames for each uploaded file
  temperature_data <- data.frame()
  pressure_data <- data.frame()
  
  
  observeEvent(input$plotTempButton, {
    req(input$file)
    
    file_tct <- input$file_tct
    
    if (!is.null(file_tct)) {
    batch_id <- TCT_Batch_id_extract(input = input$file_tct$datapath) 
    }
    
    df <- read.delim(input$file$datapath, sep = ";") %>%
      select(Time_ms = 1, Chamber = 3, HEC = 4, IR = 5, Time, Cspeed = 7) %>% 
      mutate(Time_h = round(Time_ms/3600000, 3)) %>% 
      gather(Sensor, Temp, 2:4) %>% 
      mutate(Temp = ifelse(
        str_detect(Temp, "\\s*[A-Za-z]+\\s*"),
        paste0(str_replace(Temp, "\\s*[A-Za-z]+\\s*", ""), ".", month_mapping[str_extract(Temp, "\\s*([A-Za-z]+)\\s*")]),
        Temp)) %>% 
      mutate(Day_event = ifelse(str_detect(Time, "^00:00:0[0-9]"), "yes", "no")) %>%
      mutate(Day = ifelse(Day_event == "yes", cumsum(Day_event == "yes"), NA)) %>% 
      fill(Day) %>% 
      mutate(Day = ifelse(Sensor=="IR", Day-14, 
                          ifelse(Sensor=="HEC", Day-7, Day))) %>% 
      mutate(Day = ifelse(is.na(Day), 0, Day),
             Chamber = "Chamber")
    
    if (!is.null(file_tct)) {
      df$Sensor <- paste(df$Sensor, " (", batch_id, ")", sep = "")
      df$Chamber <- paste(df$Chamber, " (", batch_id, ")", sep = "")
    }
    
    # Append the processed data frame to the list
    temperature_data <<- bind_rows(temperature_data, df)
    
    if (!is.null(file_tct)) {
    # Extract events from TCT protocol
    event_meta <- TCT_Pdf_extract(input = input$file_tct$datapath)
    
    # Calculate and determine Time_ms of events from TCT protocol
    event_meta_list <- TCT_log_event(input = temperature_data, input2 = event_meta)
    
    # Establish vertical lines
    vertical.lines <- c(temperature_data %>% filter(Day_event == "yes") %>% distinct(Time_h) %>% pull(Time_h))
    event.lines.m <- c(event_meta_list %>% filter(Event_type == "Matrix") %>%  pull(Time_h))
    event.text.m <- c(event_meta_list %>% filter(Event_type == "Matrix") %>%  pull(Event_text))
    event.lines.s <- c(event_meta_list %>% filter(Event_type == "Sampling") %>%  pull(Time_h))
    event.text.s <- c(event_meta_list %>% filter(Event_type == "Sampling") %>%  pull(Event_text))
    event.lines.o <- c(event_meta_list %>% filter(Event_type == "Others") %>%  pull(Time_h))
    event.text.o <- c(event_meta_list %>% filter(Event_type == "Others") %>%  pull(Event_text))
    
    # Establish line plotting commands
    # Attach events
    # Matrix
    p.m <- geom_vline(xintercept = event.lines.m,
                      color = "#005A64", linewidth=0.5)
    p.m.t <- annotate("text", x=event.lines.m, y=c(seq(40, 42, length.out = 8), seq(42, 40, length.out = 3)), label=event.text.m, size = 2.5)
    # Samplings
    p.s <- geom_vline(xintercept = event.lines.s,
                      color = "#BECD32", linewidth=0.5) 
    p.s.t <- annotate("text", x=event.lines.s, y=seq(20, 22, length.out = length(event.lines.s)), label=event.text.s, size = 2.5)
    # Others
    p.o <- geom_vline(xintercept = event.lines.o,
                      color = "#CCCCFF", linewidth=0.5)
    p.o.t <- annotate("text", x=event.lines.o, y=seq(0, 10, length.out = length(event.lines.o)), label=event.text.o, size = 2.5)
    
    }
    
    # Update the plot in output$sensorPlot
    output$sensorPlot <- renderPlotly({
      if (nrow(temperature_data) == 0)
        return(NULL)

      # Plot without events
      p <- ggplot(temperature_data, aes(x = Time_h, y = Temp, color = Sensor)) +
        geom_line() +
        labs(x = "Time since start (h)", y = "Temperature (°C)") +
        theme_classic() +
        geom_vline(xintercept = vertical.lines, linetype="dotted", 
                   color = "grey", linewidth=0.5) +
        annotate("text", x=50, y=43, label="Day 2", angle=90) +
        annotate("text", x=168, y=43, label="Day 7", angle=90) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
      
      if (!is.null(file_tct)) {
        # Allow option to show events button
        if(input$matrix & input$sampling & input$others){
          ggplotly(p + p.m + p.m.t + p.s + p.s.t + p.o + p.o.t)
        } else {
          if(input$matrix & input$sampling){
            ggplotly(p + p.m + p.m.t + p.s + p.s.t)
          } else {
            if(input$matrix){
              ggplotly(p + p.m + p.m.t)
            } else {
              if(input$sampling & input$others){
                ggplotly(p + p.s + p.s.t + p.o + p.o.t)
              } else {
                if(input$sampling){
                  ggplotly(p + p.s + p.s.t)
                } else {
                  if(input$others){
                    ggplotly(p + p.o + p.o.t)
                  } else {ggplotly(p)}
                }}}}}
      } else {ggplotly(p)}
      })
    
    # Update the plot in output$sensorPlot_chamber
    output$sensorPlot_chamber <- renderPlotly({
      if (nrow(temperature_data) == 0)
        return(NULL)
      
      vertical.lines_cs <- c(temperature_data %>% filter(Day_event == "yes") %>% distinct(Time_h) %>% pull(Time_h))
      event.lines <- c(event_meta_list %>% pull(Time_h))
      
      pc <- ggplot(temperature_data, aes(x = Time_h, y = Cspeed, colour = Chamber)) +
        geom_line() +
        labs(x = NULL, y = "Speed (RPM)", colour = "Motion controller") +
        theme_classic() +
        geom_vline(xintercept = vertical.lines_cs, linetype="dotted", 
                   color = "grey", linewidth=0.5) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
      
      
      if (!is.null(file_tct)) {
      # Allow option to show events button
      if(input$matrix & input$sampling & input$others){
        ggplotly(pc + p.m + p.s + p.o)
      } else {
        if(input$matrix & input$sampling){
          ggplotly(pc+ p.m + p.s)
        } else {
          if(input$matrix){
            ggplotly(pc+ p.m)
          } else {
            if(input$sampling & input$others){
              ggplotly(pc + p.s + p.o)
            } else {
              if(input$sampling){
                ggplotly(pc + p.s)
              } else {
                if(input$others){
                  ggplotly(pc + p.o)
                } else {ggplotly(pc)}
              }}}}}
      } else {ggplotly(pc)}
    })
    
  })
  
  observeEvent(input$plotPressureButton, {
    req(input$file_ps)
    
    file_tct <- input$file_tct
    
    if (!is.null(file_tct)) {
      batch_id <- TCT_Batch_id_extract(input = input$file_tct$datapath) 
    }
    
    df_ps <- read.delim(input$file_ps$datapath, sep = ";") %>%
      select(Time_ms = 1, PS1 = 3, PS2 = 4, PS3 = 6, Time, Pspeed = 5) %>% 
      mutate(Time_h = round(Time_ms/3600000, 3)) %>% 
      gather(Sensor, Mbar, 2:4) -> data_ps_1
    
    data_ps_1 %>% 
      mutate(Day_event = ifelse(str_detect(Time, "^00:00:[0-1][0-9]"), "yes", "no")) %>%
      filter(Day_event == "yes") %>%
      group_by(Time_h) %>%
      filter(Time_ms == min(Time_ms)) %>% 
      ungroup() %>% 
      select(Time_ms, Sensor, Day_event) %>% 
      mutate(Day = ifelse(Day_event == "yes", cumsum(Day_event == "yes"), "no")) %>% 
      mutate(Day = ifelse(Sensor=="PS3", Day-14, 
                          ifelse(Sensor=="PS2", Day-7, Day))) -> data_ps_2
    
    df_ps <- left_join(data_ps_1, data_ps_2) %>%  
      fill(Day) %>% 
      mutate(Day = ifelse(is.na(Day), 0, Day),
             Day_event = ifelse(is.na(Day_event), "no", Day_event),
             Pump = "Pump")
    
    if (!is.null(file_tct)) {
      df_ps$Sensor <- paste(df_ps$Sensor, " (", batch_id, ")", sep = "")
      df_ps$Pump <- paste(df_ps$Pump, " (", batch_id, ")", sep = "")
    }
    
    # Append the processed data frame to the list
    pressure_data <<- bind_rows(pressure_data, df_ps) %>% 
      filter(row_number() %% 2 == 0) %>% # make dataset thinner 2x
      filter(row_number() %% 2 == 0 )%>% # make dataset thinner 4x
      filter(row_number() %% 2 == 0) # make dataset thinner 8x
    
    if (!is.null(file_tct)) {
      # Extract events from TCT protocol
      event_meta <- TCT_Pdf_extract(input = input$file_tct$datapath)
      
      # Calculate and determine Time_ms of events from TCT protocol
      event_meta_list <- TCT_log_event(input = pressure_data, input2 = event_meta)
      
      # Establish vertical lines
      vertical.lines_ps <- c(pressure_data %>% filter(Day_event == "yes") %>% distinct(Time_h) %>% pull(Time_h))
      event.lines.m <- c(event_meta_list %>% filter(Event_type == "Matrix") %>%  pull(Time_h))
      event.text.m <- c(event_meta_list %>% filter(Event_type == "Matrix") %>%  pull(Event_text))
      event.lines.s <- c(event_meta_list %>% filter(Event_type == "Sampling") %>%  pull(Time_h))
      event.text.s <- c(event_meta_list %>% filter(Event_type == "Sampling") %>%  pull(Event_text))
      event.lines.o <- c(event_meta_list %>% filter(Event_type == "Others") %>%  pull(Time_h))
      event.text.o <- c(event_meta_list %>% filter(Event_type == "Others") %>%  pull(Event_text))
      
      # Establish line plotting commands
      # Attach events
      # Matrix
      p.m <- geom_vline(xintercept = event.lines.m,
                        color = "#005A64", linewidth=0.5)
      p.m.t <- annotate("text", x=event.lines.m, y=c(seq(1000, 1500, length.out = 8), seq(1500, 1000, length.out = 3)), label=event.text.m, size = 2.5)
      # Samplings
      p.s <- geom_vline(xintercept = event.lines.s,
                        color = "#BECD32", linewidth=0.5) 
      p.s.t <- annotate("text", x=event.lines.s, y=seq(-500, -450, length.out = length(event.lines.s)), label=event.text.s, size = 2.5)
      # Others
      p.o <- geom_vline(xintercept = event.lines.o,
                        color = "#CCCCFF", linewidth=0.5)
      p.o.t <- annotate("text", x=event.lines.o, y=seq(-1000, -500, length.out = length(event.lines.o)), label=event.text.o, size = 2.5)
      
    }
    
    
    # Update the plot in output$sensorPlot_ps
    output$sensorPlot_ps <- renderPlotly({
      if (nrow(pressure_data) == 0)
        return(NULL)
      
      vertical.lines_ps <- c(pressure_data %>% filter(Day_event == "yes") %>% distinct(Time_h) %>% pull(Time_h))
      
      pp <- ggplot(pressure_data, aes(x = Time_h, y = Mbar, color = Sensor)) +
        geom_line() +
        labs(x = "Time since start (h)", y = "Pressure (mbar)") +
        theme_classic() +
        geom_vline(xintercept = vertical.lines_ps, linetype="dotted", 
                   color = "grey", linewidth=0.5) +
        annotate("text", x=50, y=2000, label="Day 2", angle=90) +
        annotate("text", x=168, y=2000, label="Day 7", angle=90) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
      
      if (!is.null(file_tct)) {
        # Allow option to show events button
        if(input$matrix & input$sampling & input$others){
          ggplotly(pp + p.m + p.m.t + p.s + p.s.t + p.o + p.o.t)
        } else {
          if(input$matrix & input$sampling){
            ggplotly(pp + p.m + p.m.t + p.s + p.s.t)
          } else {
            if(input$matrix){
              ggplotly(pp + p.m + p.m.t)
            } else {
              if(input$sampling & input$others){
                ggplotly(pp + p.s + p.s.t + p.o + p.o.t)
              } else {
                if(input$sampling){
                  ggplotly(pp + p.s + p.s.t)
                } else {
                  if(input$others){
                    ggplotly(pp + p.o + p.o.t)
                  } else {ggplotly(pp)}
                }}}}}
      } else {ggplotly(pp)}
    })
    
    # # Update the plot in output$sensorPlot_pump
    output$sensorPlot_pump <- renderPlotly({
      if (nrow(pressure_data) == 0)
        return(NULL)

      vertical.lines_pu <- c(pressure_data %>% filter(Day_event == "yes") %>% distinct(Time_h) %>% pull(Time_h))

      ppu <- ggplot(pressure_data, aes(x = Time_h, y = Pspeed, colour = Pump)) +
        geom_line() +
        labs(x = NULL, y = "Speed (units/sec)", colour = "Pump speed") +
        theme_classic() +
        geom_vline(xintercept = vertical.lines_pu, linetype="dotted", 
                   color = "grey", linewidth=0.5) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 15))

      if (!is.null(file_tct)) {
        # Allow option to show events button
        if(input$matrix & input$sampling & input$others){
          ggplotly(ppu + p.m + p.s + p.o)
        } else {
          if(input$matrix & input$sampling){
            ggplotly(ppu+ p.m + p.s)
          } else {
            if(input$matrix){
              ggplotly(ppu+ p.m)
            } else {
              if(input$sampling & input$others){
                ggplotly(ppu + p.s + p.o)
              } else {
                if(input$sampling){
                  ggplotly(ppu + p.s)
                } else {
                  if(input$others){
                    ggplotly(ppu + p.o)
                  } else {ggplotly(ppu)}
                }}}}}
      } else {ggplotly(ppu)}
    })
  })
}

# Run the application
shinyApp(ui, server)
