source("global.R")
source("css.R")

ui <- fluidPage(
  
    theme = bs_theme(version = 4, bootswatch = "lux"),
    tags$head(tags$style(css)),

    fluidRow(
        column(8, 
        HTML("
        <span>
            <span style='padding-top:5px'>
                <img class='imgFloater' src='logo_OECD_DEV_en.jpg' height=100, width=150>
                <p style='font-size:2px'>    </p>
                <b style='margin-bottom:0px !important;padding-top:15px;font-size:28px;color:#007fc0'>Global Vaccination Target Dashboard</b>
            </span>
            <br>
            <span style='font-size:14px'>
                <i>A no-frills dashboard for tracking achievement of WHO targets worldwide</i>
            </span>
        </span>
        <br>")
        ), 
        column(4, style="padding-top:30px; padding-right:50px", align="right", 
               actionButton("showInstructions", label="How To Use"))
    ),
    
    fluidPage(
      br(),
        tabsetPanel(
          tabPanel("Gauges",
                   fluidRow(
                     column(12, 
                            fluidRow(
                              column(6, align="center", 
                                     HTML("<br><span style='font-size:18px;color:#007fc0'><b>Latest data</b></span>"),
                                     selectInput(
                                       inputId = "dateInputNull",
                                       label = "Average share of people fully vaccinated out of total population across all members of group",
                                       choices = date_with_lag,
                                       selected = date_with_lag,
                                       multiple=F, 
                                       width="66%")),
                              column(6, align="center",
                                     HTML("<br><span style='font-size:18px;color:#007fc0'><b>Reference period (monthly average)</b></span>"),
                                     selectInput(
                                       inputId = "dateInput",
                                       label = "Average share of people fully vaccinated out of total population across all members of group",
                                       choices = date_choices,
                                       selected = date_choices[5],
                                       multiple=F, 
                                       width="66%")),
                              column(6, align="center", style="border-right-color: #bbb;border-right-height: 50px; border-right-width: 2px;border-right-style: solid;",
                                     plotOutput("incomePlots", height=250)
                                     ),
                              column(6, align="center",
                                     plotOutput("incomePlots2", height=250)
                                     ),
                              column(12,
                                     fluidRow(
                                       column(6, align = "center", 
                                              selectInput("countryInput",
                                                          label = "Choose first comparison country or region",
                                                          choices = choice_list1,
                                                          selected = blank_text1,
                                                          multiple = F,
                                                          width='66%'
                                              )),
                                       column(6, align="center",
                                              selectInput("regionInput",
                                                          label = "Choose second comparison country or region",
                                                          choices = choice_list2,
                                                          selected = blank_text2,
                                                          multiple = F,
                                                          width="66%")
                                       )
                                     )
                              ),
                              column(6, align="center", style="border-right-color: #bbb;border-right-height: 50px; border-right-width: 2px;border-right-style: solid;",
                                     plotOutput("choicePlots", height=250),
                                     HTML("<p style='font-size:12px'><i>Note: Country values are the ratio of people fully vaccinated and their total population, income groups and continents are the average of this value across all members of the designation.</i></p>")
                                     ),
                              column(6, align="center",
                                     plotOutput("choicePlots2", height=250),
                                     HTML("<p style='font-size:12px'><i>Note: Country values are the ratio of people fully vaccinated and their total population, income groups and continents are the average of this value across all members of the designation.</i></p>")
                              )
                            )
                     )
                   )
          ),
          tabPanel("Line graphs",
                   br(),
                     fluidRow(
                       column(6, align="center",
                              selectInput("countryInput2",
                                          label = "Choose first comparison country or region",
                                          choices = choice_list1,
                                          selected = blank_text1,
                                          multiple = F,
                                          width='66%'
                              )),
                       column(6, align="center",
                              selectInput("regionInput2",
                                          label = "Choose second comparison country or region",
                                          choices = choice_list2,
                                          selected = blank_text2,
                                          multiple = F,
                                          width="66%"))
                              ),
                   br(),
                   fluidRow(
                     column(1),
                     column(10, echarts4rOutput("echartPlot", height=450)),
                     column(1)
                   )
            )
        ),
      br(), br(),
      p(align="right", style="font-size:12px", "Data source: ", tags$a(href="https://ourworldindata.org/coronavirus", target="_blank", "Our World in Data"))
        
    )
)

server <- function(input, output, session) {
    
  # Create modal to open upon load & response to actionButton
  modal <- modalDialog(
    title = "How To Use",
    HTML("<h5 style='color:#007fc0'>Visualization</h5>
             <center><img src='gauge-explanation.png', height=220px, width=625px></center>
             <br>
             The <a href='https://www.who.int/news/item/07-10-2021-who-un-set-out-steps-to-meet-world-covid-vaccination-targets' target='_blank'>WHO </a> has set two global targets, 40% of the population fully vaccinated everywhere by the end of 2021 and 70% by mid 2022.
             This app was created to track progress towards these global vaccination targets. 
             <br>
             <br>
             There are two sets of gauges, one set showing <b>current</b> rates for average share of population fully vaccinated, another set showing this same indicator for a chosen <b>reference period</b>. 
             By default, each set of gauges shows income group aggregates for their respective period, with options for choosing a country and a region as a comparator.
             "),
    HTML("<hr>
            <h5 style='color:#007fc0'>Data</h5>
            All data comes for <a href='https://ourworldindata.org/coronavirus' target='_blank'>Our World in Data</a>. 
            The income group and regional aggregates are calculated by taking the average of people fully vaccinated per hundred across each member designated in the group. 
            This highlights the disparities across the region or income group in reaching an overall adequate level of vaccination.
            <br>
            <br>
            Any missing data is filled by carrying the latest observation forward. 
            Latest observations are shown using a lag of 2 days so that values are closer to full reporting.
            Most countries report daily, but some data may be up to 2 weeks old. 
            Income groups groups are designated according to the World Bank's income classification,
            found <a href='https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups' target='_blank'>here.</a>")

  )
  
  # Upon load modal
  showModal(modal)
  
  # Upon click modal
  observeEvent(input$showInstructions, {
    
    showModal(modal)
    
  })

  # Loads line graph page with previous choices
  observeEvent(c(input$countryInput), {
    
    updateSelectInput(session, "countryInput2", selected = input$countryInput)
    
  })
  
  observeEvent(c(input$regionInput), {
    
    updateSelectInput(session, "regionInput2", selected = input$regionInput)
    
  })
  
  # Line graph rendering
  observeEvent(c(input$countryInput2, input$regionInput2), {
    
    country_input <- input$countryInput2
    region_input <- input$regionInput2
    
    data_line <- data_owid %>%
      filter(location %in% c(country_input, region_input)) %>%
      select(location, date, people_fully_vaccinated_per_hundred) %>%
      arrange(location, date) %>%
      group_by(location) %>%
      mutate(people_fully_vaccinated_per_hundred = zoo::na.locf(people_fully_vaccinated_per_hundred, na.rm=F)) %>%
      drop_na() %>%
      ungroup() %>%
      rename("income" = "location")
    
    data_line <- rbind(data_tidy_raw, data_line)
    
    data_line <- data_line %>%
      mutate(people_fully_vaccinated_per_hundred = round(people_fully_vaccinated_per_hundred, digits=2)) %>%
      rename("val" = "people_fully_vaccinated_per_hundred")
    
    output$echartPlot <- renderEcharts4r({
      
      echarts4r::e_common(
        font_family = "Arial",
        theme = NULL
      )
      
      max <- list(
        name = "Max",
        type = "max"
      )
      
      data_line %>%
        mutate(date = as.Date(date)) %>%
        group_by(income) %>%
        e_charts(date) %>%
        e_line(val) %>%
        e_mark_line(data = list(yAxis = 70), title = "Mid-2022 target, \n70% vaccination \nrate", color="red") %>%
        e_mark_line(data = list(yAxis = 40), title = "End of 2021 target, \n40% vaccination \nrate") %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(type = "scroll") %>%
        e_toolbox(bottom = 0) %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_mark_point(data = max)
      
    })
    
  })
  
  
  observeEvent(c(input$countryInput, input$regionInput), {
    
    country_input <- input$countryInput 
    region_input <- input$regionInput
    
    dat <- data_tidy_raw %>%
      group_by(income) %>%
      filter(date == max(date)) %>%
      select(income, people_fully_vaccinated_per_hundred) %>%
      mutate(people_fully_vaccinated_per_hundred = round(people_fully_vaccinated_per_hundred, digits=0))
    
    dat_circle <- dat %>%
      mutate(pct = people_fully_vaccinated_per_hundred,
             pct = round(pct, digits=1),
             tidy_vax = round(people_fully_vaccinated_per_hundred, digits=1),
             color_des = ifelse(people_fully_vaccinated_per_hundred < 40, "low", 
                                ifelse(people_fully_vaccinated_per_hundred > 70, "high", "medium"))) %>%
      mutate(income = factor(income, levels = c("World", "Low income", "Lower middle income",
                                                "Upper middle income", "High income"))) %>%
      arrange(income) %>%
      mutate(people_fully_vaccinated_per_hundred = ifelse(people_fully_vaccinated_per_hundred > 101, 101, people_fully_vaccinated_per_hundred))
    
    group.colors <- c(low = "#ff6f69", medium = "#ffcc5c", high ="#88d8b0")
    
    output$incomePlots <- renderPlot({
      
      dat_circle %>%
        ggplot(aes(xmax = 3, xmin = 2, ymax = 70, ymin = 0)) +
        geom_rect(color = "grey52", fill = "grey52", ymax=max_val) +
        geom_rect(aes(fill = color_des, ymax=people_fully_vaccinated_per_hundred), color = "#6d7069", size = .75) +
        geom_rect(xmin=2, xmax=3, ymin=39.5, ymax=40.5) +
        geom_text(aes(x = 0, y = 0, label = pct),
                  color="black", alpha = .6, fontface = "bold", size = 9) +
        geom_text(aes(x = 0, y = 0.5, label = income),
                  color="black", fontface = "bold", size = 4,
                  lineheight = .8, vjust = -9) +
        facet_grid(~ income, switch = "y") +
        coord_polar(theta = "y", clip="off") +
        scale_x_continuous(limits = c(0, 3)) +
        scale_fill_manual(values=group.colors) +
        guides(fill="none", alpha="none", color="none") +
        theme(
          plot.caption = element_text(size=6),
          panel.background = element_rect(fill = "transparent", color = "transparent"),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y.left = element_text(size = 25,
                                           color = "grey70",
                                           face = "bold",
                                           vjust = .85,
                                           hjust = 0,
                                           angle = 0),
          plot.margin = margin(30, 60, 30, 60),
          panel.spacing.y = unit(0, "pt"))
    })
    
    if (!country_input == blank_text1) {
      
      dat_country <- data_owid %>%
        filter(location == country_input) %>%
        mutate(income = country_input) %>%
        select(income, date, people_fully_vaccinated_per_hundred) %>%
        drop_na() %>%
        filter(date == max(date)) %>%
        select(-date) %>%
        mutate(pct = people_fully_vaccinated_per_hundred,
               pct = round(pct, digits=1),
               tidy_vax = round(people_fully_vaccinated_per_hundred, digits=1),
               color_des = ifelse(pct < 40, "#88d8b0", 
                                  ifelse(pct > 70, "#ff6f69", "#E1AD01")))
      
      if(nrow(dat_country) < 1) {
        
        dat_country <- data.frame(
          income = country_input,
          people_fully_vaccinated_per_hundred = 0,
          pct = "No data \navailable",
          tidy_vax= 0,
          color_des = "#88d8b0")
      }
      
      dat_extra <- rbind(dat_extra, dat_country) 
      
    } 
    
    if (!region_input == blank_text2) {
      
      dat_region <- data_owid %>%
        filter(location == region_input) %>%
        mutate(income = region_input) %>%
        select(income, date, people_fully_vaccinated_per_hundred) %>%
        drop_na() %>%
        filter(date == max(date)) %>%
        select(-date) %>%
        mutate(pct = people_fully_vaccinated_per_hundred,
               pct = round(pct, digits=1),
               tidy_vax = round(people_fully_vaccinated_per_hundred, digits=1),
               color_des = ifelse(pct < 40, "#88d8b0", 
                                  ifelse(pct > 70, "#ff6f69", "#E1AD01")))
      
      if(nrow(dat_region) < 1) {
        
        dat_region <- data.frame(
          income = region_input,
          people_fully_vaccinated_per_hundred = 0,
          pct = "No data \navailable",
          tidy_vax= 0,
          color_des = "#88d8b0")
      }
      
      dat_extra <- rbind(dat_extra, dat_region)
      
    } 
    
    if (!region_input == blank_text2 & country_input == blank_text1) {
      
      empty_df2 <- data.frame(
        income = blank_text1,
        people_fully_vaccinated_per_hundred = 0,
        pct = ("0"),
        tidy_vax= 0,
        color_des = "#88d8b0")
      
      dat_extra <- rbind(dat_extra, empty_df2)
      
    } else if (region_input == blank_text2 & !country_input == blank_text1) {
      
      empty_df2 <- data.frame(
        income = blank_text2,
        people_fully_vaccinated_per_hundred = 0,
        pct = ("0"),
        tidy_vax= 0,
        color_des = "#88d8b0")
      
      dat_extra <- rbind(dat_extra, empty_df2)
      
    } else if (!region_input == blank_text2 & !country_input == blank_text1) {
      dat_extra <- rbind(dat_region, dat_country)
    } else {
      dat_extra <- data.frame(
        income = c(blank_text1, blank_text2),
        people_fully_vaccinated_per_hundred = 0,
        pct = "0",
        tidy_vax= 0,
        color_des = "#88d8b0")
    }
    
    dat_extra <- dat_extra %>%
      mutate(color_des = ifelse(people_fully_vaccinated_per_hundred < 40, "low", 
                                ifelse(people_fully_vaccinated_per_hundred > 70, "high", "medium"))) %>%
      arrange(income) %>%
      mutate(people_fully_vaccinated_per_hundred = ifelse(people_fully_vaccinated_per_hundred > 101, 101, people_fully_vaccinated_per_hundred))
    
    
    if(!country_input == region_input) {
      dat_extra <- dat_extra %>% mutate(income = factor(income, levels=c(country_input, region_input)))
    }
    
    group.colors <- c(low = "#ff6f69", medium = "#ffcc5c", high ="#88d8b0")
    
    output$choicePlots <- renderPlot({
      
      dat_extra %>%
        ggplot(aes(xmax = 3, xmin = 2, ymax = 70, ymin = 0)) +
        geom_rect(color = "grey52", fill = "grey52", ymax=max_val) +
        geom_rect(aes(fill = color_des, ymax=people_fully_vaccinated_per_hundred),
                  color = "#6d7069", size = .75) +
        geom_rect(xmin=2, xmax=3, ymin=39.5, ymax=40.5) +
        geom_text(aes(x = 0, y = 0, label = pct),
                  color="black", alpha = .6, fontface = "bold", size = 12) +
        geom_text(aes(x = 0, y = 0.5, label = income),
                  color = "black", fontface = "bold", size = 5,
                  lineheight = .8, vjust = -9) +
        facet_grid(~ income, switch = "y") +
        coord_polar(theta = "y", clip="off") +
        scale_x_continuous(limits = c(0, 3)) +
        scale_fill_manual(values = group.colors) +
        guides(fill="none", alpha="none", color="none") +
        theme(
          panel.background = element_rect(fill = "transparent", color = "transparent"),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y.left = element_text(size = 25,
                                           color = "grey70",
                                           face = "bold",
                                           vjust = .85,
                                           hjust = 0,
                                           angle = 0),
          plot.margin = margin(30, 60, 30, 60),
          panel.spacing.y = unit(0, "pt"))
    })
    
    
  })
  
    # Date choice ----------------
    observeEvent(c(input$countryInput, input$regionInput, input$dateInput), {
        
        country_input <- input$countryInput 
        region_input <- input$regionInput
        date_input <- as.character(input$dateInput)
        dat_extra <- data.frame()
        
        dat <- data_tidy_raw %>%
            mutate(date = format(date, "%B %Y")) %>%
            filter(date == date_input) %>%
            group_by(income) %>%
            mutate(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
            slice(1) %>%
            ungroup() %>%
            select(income, people_fully_vaccinated_per_hundred) %>%
            mutate(people_fully_vaccinated_per_hundred = round(people_fully_vaccinated_per_hundred, digits=0))
        
        dat_circle <- dat %>%
          mutate(pct = people_fully_vaccinated_per_hundred,
                 pct = round(pct, digits=0),
                 tidy_vax = round(people_fully_vaccinated_per_hundred, digits=0),
                 color_des = ifelse(people_fully_vaccinated_per_hundred < 40, "low", 
                                    ifelse(people_fully_vaccinated_per_hundred > 70, "high", "medium"))) %>%
          mutate(income = factor(income, levels = c("World", "Low income", "Lower middle income",
                                                    "Upper middle income", "High income"))) %>%
          arrange(income) %>%
          mutate(people_fully_vaccinated_per_hundred = ifelse(people_fully_vaccinated_per_hundred > 101, 101, people_fully_vaccinated_per_hundred))
        
        group.colors <- c(low = "#ff6f69", medium = "#ffcc5c", high ="#88d8b0")
        
        output$incomePlots2 <- renderPlot({
            
            dat_circle %>%
                ggplot(aes(xmax = 3, xmin = 2, ymax = 70, ymin = 0)) +
                geom_rect(color = "grey52", fill = "grey52", ymax=max_val) +
                geom_rect(aes(fill = color_des, ymax=people_fully_vaccinated_per_hundred),
                          color = "#6d7069", size = .75) +
                geom_rect(xmin=2, xmax=3, ymin=39.5, ymax=40.5) +
                geom_text(aes(x = 0, y = 0, label = pct),
                          color="black", alpha = .6, fontface = "bold", size = 9) +
                geom_text(aes(x = 0, y = 0.5, label = income),
                          color="black", fontface = "bold", size = 4, 
                          lineheight = .8, vjust = -9) +
                facet_grid(~ income, switch = "y") +
                coord_polar(theta = "y", clip="off") +
                scale_x_continuous(limits = c(0, 3)) +
                scale_fill_manual(values=group.colors) +
                guides(fill="none", alpha="none", color="none") +
                theme(
                    panel.background = element_rect(fill = "transparent", color = "transparent"),
                    panel.grid = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    strip.text.x = element_blank(),
                    strip.text.y.left = element_text(size = 25,
                                                     color = "grey70",
                                                     face = "bold",
                                                     vjust = .85,
                                                     hjust = 0,
                                                     angle = 0),
                    plot.margin = margin(30, 60, 30, 60),
                    panel.spacing.y = unit(0, "pt"))
            
        })
        
        empty_df <- data.frame(
            income = c(blank_text1, blank_text2),
            people_fully_vaccinated_per_hundred = 0,
            pct = ("0"),
            tidy_vax= 0,
            color_des = "#88d8b0")
        
        if (!country_input == blank_text1) {
            
            dat_country <- data_owid %>%
                filter(location == country_input) %>%
                mutate(income = country_input) %>%
                select(income, date, people_fully_vaccinated_per_hundred) %>%
                drop_na() %>%
                mutate(date = format(date, "%B %Y")) %>%
                filter(date == date_input) %>%
                mutate(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
                slice(1) %>%
                select(-date) %>%
                mutate(pct = people_fully_vaccinated_per_hundred,
                       pct = round(pct, digits=0),
                       tidy_vax = round(people_fully_vaccinated_per_hundred, digits=0),
                       color_des = ifelse(pct < 40, "#88d8b0", 
                                          ifelse(pct > 70, "#ff6f69", "#E1AD01")))
            
            if(nrow(dat_country) < 1) {
                
                dat_country <- data.frame(
                    income = country_input,
                    people_fully_vaccinated_per_hundred = 0,
                    pct = "No data \navailable",
                    tidy_vax= 0,
                    color_des = "#88d8b0")
            }
            
            dat_extra <- rbind(dat_extra, dat_country) 
            
        } 
        
        if (!region_input == blank_text2) {
          
          dat_region <- data_owid %>%
            filter(location == region_input) %>%
            mutate(income = region_input) %>%
            select(income, date, people_fully_vaccinated_per_hundred) %>%
            drop_na() %>%
            mutate(date = format(date, "%B %Y")) %>%
            filter(date == date_input) %>%
            mutate(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
            slice(1) %>%
            select(-date) %>%
            mutate(pct = people_fully_vaccinated_per_hundred,
                   pct = round(pct, digits=0),
                   tidy_vax = round(people_fully_vaccinated_per_hundred, digits=0),
                   color_des = ifelse(pct < 40, "#88d8b0", 
                                      ifelse(pct > 70, "#ff6f69", "#E1AD01")))
          
          if(nrow(dat_region) < 1) {
            
            dat_region <- data.frame(
              income = region_input,
              people_fully_vaccinated_per_hundred = 0,
              pct = "No data \navailable",
              tidy_vax= 0,
              color_des = "#88d8b0")
          }
          
          dat_extra <- rbind(dat_extra, dat_region) 
          
        } 
        
        
        if (!region_input == blank_text2 & country_input == blank_text1) {
            
            empty_df2 <- data.frame(
                income = blank_text1,
                people_fully_vaccinated_per_hundred = 0,
                pct = ("0"),
                tidy_vax= 0,
                color_des = "#88d8b0")
            
            dat_extra <- rbind(dat_extra, empty_df2)
            
        } else if (region_input == blank_text2 & !country_input == blank_text1) {
            
            empty_df2 <- data.frame(
                income = blank_text2,
                people_fully_vaccinated_per_hundred = 0,
                pct = ("0"),
                tidy_vax= 0,
                color_des = "#88d8b0")
            
            dat_extra <- rbind(dat_extra, empty_df2)
            
        } else if (!region_input == blank_text2 & !country_input == blank_text1) {
            dat_extra <- rbind(dat_region, dat_country)
        } else {
            dat_extra <- empty_df
        }
        
        dat_extra <- dat_extra %>%
          mutate(color_des = ifelse(people_fully_vaccinated_per_hundred < 40, "low", 
                                    ifelse(people_fully_vaccinated_per_hundred > 70, "high", "medium"))) %>%
          arrange(income) %>%
          mutate(people_fully_vaccinated_per_hundred = ifelse(people_fully_vaccinated_per_hundred > 101, 101, people_fully_vaccinated_per_hundred))

        if(!country_input == region_input) {
            dat_extra <- dat_extra %>% mutate(income = factor(income, levels=c(country_input, region_input)))
        }
        
        output$choicePlots2 <- renderPlot({
            
            dat_extra %>%
                ggplot(aes(xmax = 3, xmin = 2, ymax = 70, ymin = 0)) +
                geom_rect(color = "grey52", fill = "grey52", ymax=max_val) +
                geom_rect(aes(fill = color_des, ymax=people_fully_vaccinated_per_hundred),
                          color = "#6d7069", size = .75) +
                geom_rect(xmin=2, xmax=3, ymin=39.5, ymax=40.5) +
                geom_text(aes(x = 0, y = 0, label = pct),
                          color="black", alpha = .6, fontface = "bold", size = 12) +
                geom_text(aes(x = 0, y = 0.5, label = income),
                          color = "black", fontface = "bold", size = 5,
                          lineheight = .8, vjust = -9) +
                facet_grid(~ income, switch = "y") +
                coord_polar(theta = "y", clip="off") +
                scale_x_continuous(limits = c(0, 3)) +
                scale_fill_manual(values = group.colors) +
                guides(fill="none", alpha="none", color="none") +
                theme(
                    panel.background = element_rect(fill = "transparent", color = "transparent"),
                    panel.grid = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    strip.text.x = element_blank(),
                    strip.text.y.left = element_text(size = 25,
                                                     color = "grey70",
                                                     face = "bold",
                                                     vjust = .85,
                                                     hjust = 0,
                                                     angle = 0),
                    plot.margin = margin(30, 60, 30, 60),
                    panel.spacing.y = unit(0, "pt"))
            
        })
    })
        

}

# Run the application 
shinyApp(ui = ui, server = server)
