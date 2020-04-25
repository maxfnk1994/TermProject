# =================================================================================================================================================
# Course: CIS-546 DATA VISUALIZATION
# Term: Spring/T2
# FINAL PROJECT: IMF
# Name: Maximilian Franke
# ID: 0867094
# =================================================================================================================================================

# INSTALL AND LOAD PACKAGES
# -------------------------------------------------------------------------------------------------------------------------------------------------

my_packages <- c("plotly", "shiny", "shinydashboard", "tidyverse", "dplyr", "reshape", "gganimate", "gifski", "png", "scales", "maps")
#install.packages(my_packages, repos = "http://cran.rstudio.com")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(readxl)
library(reshape)
library(gganimate)
library(gifski)
library(png)
library(plotly)
library(scales)
library(maps)

# LOAD DATA
# -------------------------------------------------------------------------------------------------------------------------------------------------
gdp <- read.csv("data/WEOOct2019all.csv")
names(gdp)[10:54] <- substring(names(gdp)[10:54], 2)
weo <- read.csv("data/WEO_Data.csv")

# =================================================================================================================================================
# DATA PREPARATION
# =================================================================================================================================================

# National Account
# -------------------------------------------------------------------------------------------------------------------------------------------------

# First animation
names(weo)[4:44] <- substring(names(weo)[4:44], 2)
for (i in 4:19) {
    weo[,i] <- as.numeric(levels(weo[,i])[weo[,i]])  
}
weo_table <- melt(subset(weo, Subject.Descriptor == "Gross domestic product, constant prices" & Units == "Percent change")[,c(1,2,4:44)], id = c("Country.Group.Name", "Subject.Descriptor"))
weo_table$variable <- round(as.numeric(levels(weo_table$variable)[weo_table$variable]),0)
weo_table$hi_lo <- if_else(weo_table$value > 0, "Above", "Below")

# Second animation
weo_table2 <- melt(subset(weo, Subject.Descriptor == "Inflation, average consumer prices" & Units == "Percent change")[,c(1,2,4:44)], id = c("Country.Group.Name", "Subject.Descriptor"))
weo_table2$variable <- round(as.numeric(levels(weo_table2$variable)[weo_table2$variable]),0)
weo_table2$hi_lo <- if_else(weo_table2$value > 0, "Above", "Below")

# First graph
GDP_development <- melt(subset(gdp, WEO.Subject.Code == "NGDP_RPCH")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
GDP_development$value <- as.numeric(levels(GDP_development$value)[GDP_development$value])
GDP_development$variable <- round(as.numeric(levels(GDP_development$variable)[GDP_development$variable]),0)
names(GDP_development) <- c("WEO.Subject.Code", "Country", "Year", "GDP")

# First table
GDP_development_table <- subset(gdp, WEO.Subject.Code == "NGDP_RPCH")[,c(4,45:50)]
rownames(GDP_development_table) <- NULL

# Boxplot of total investments
totalInvestment <- melt(subset(gdp, WEO.Subject.Code == "NID_NGDP")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
totalInvestment$value <- as.numeric(levels(totalInvestment$value)[totalInvestment$value])
totalInvestment$variable <- round(as.numeric(levels(totalInvestment$variable)[totalInvestment$variable]),0)
names(totalInvestment) <- c("WEO.Subject.Code", "Country", "Year", "TotalInvestment")

# Trade
# -------------------------------------------------------------------------------------------------------------------------------------------------

# Volume imports goods and services
IM_goods_services <- melt(subset(gdp, WEO.Subject.Code == "TM_RPCH")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
IM_goods_services$value <- as.numeric(levels(IM_goods_services$value)[IM_goods_services$value])
IM_goods_services$variable <- round(as.numeric(levels(IM_goods_services$variable)[IM_goods_services$variable]),0)
names(IM_goods_services) <- c("WEO.Subject.Code", "Country", "Year", "IM")

# Volume exports goods and services
EX_goods_services <- melt(subset(gdp, WEO.Subject.Code == "TX_RPCH")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
EX_goods_services$value <- as.numeric(levels(EX_goods_services$value)[EX_goods_services$value])
EX_goods_services$variable <- round(as.numeric(levels(EX_goods_services$variable)[EX_goods_services$variable]),0)
names(EX_goods_services) <- c("WEO.Subject.Code", "Country", "Year", "EX")

# Volume imports goods
IM_goods <- melt(subset(gdp, WEO.Subject.Code == "TMG_RPCH")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
IM_goods$value <- as.numeric(levels(IM_goods$value)[IM_goods$value])
IM_goods$variable <- round(as.numeric(levels(IM_goods$variable)[IM_goods$variable]),0)
names(IM_goods) <- c("WEO.Subject.Code", "Country", "Year", "IM")

# Volume exports goods
EX_goods <- melt(subset(gdp, WEO.Subject.Code == "TXG_RPCH")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
EX_goods$value <- as.numeric(levels(EX_goods$value)[EX_goods$value])
EX_goods$variable <- round(as.numeric(levels(EX_goods$variable)[EX_goods$variable]),0)
names(EX_goods) <- c("WEO.Subject.Code", "Country", "Year", "EX")

# People
# -------------------------------------------------------------------------------------------------------------------------------------------------

# Unemployment rate country
Unemployment <- melt(subset(gdp, WEO.Subject.Code == "LUR")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
Unemployment$value <- as.numeric(levels(Unemployment$value)[Unemployment$value])
Unemployment <- Unemployment %>% drop_na()
Unemployment$variable <- round(as.numeric(levels(Unemployment$variable)[Unemployment$variable]),0)
names(Unemployment) <- c("WEO.Subject.Code", "Country", "Year", "Empl")

# Unemployment rate country group
Unemployment_group <- melt(subset(weo, 
                                  Country.Group.Name %in% c("Advanced economies", "Major advanced economies (G7)", "Other advanced economies (Advanced economies excluding G7 and euro area)") & 
                                      Subject.Descriptor %in% "Unemployment rate" & Units %in% "Percent of total labor force")[,c(1,2,4:44)], id = c("Country.Group.Name", "Subject.Descriptor"))
Unemployment_group$variable <- round(as.numeric(levels(Unemployment_group$variable)[Unemployment_group$variable]),0)
names(Unemployment_group) <- c("Country", "Subject.Descriptor", "Year", "Empl")

# Employment country
Employment <- melt(subset(gdp, WEO.Subject.Code == "LE")[,c(3,4,10:50)], id = c("WEO.Subject.Code", "Country"))
Employment$value <- as.numeric(levels(Employment$value)[Employment$value])
Employment <- Employment %>% drop_na()
Employment$variable <- round(as.numeric(levels(Employment$variable)[Employment$variable]),0)
names(Employment) <- c("WEO.Subject.Code", "Country", "Year", "Empl")


# Employment country group
Employment_group <- melt(subset(weo, 
                                Country.Group.Name %in% c("Advanced economies", "Major advanced economies (G7)", "Other advanced economies (Advanced economies excluding G7 and euro area)") & 
                                    Subject.Descriptor %in% "Employment" & Units %in% "Index, 2000=100")[,c(1,2,4:44)], id = c("Country.Group.Name", "Subject.Descriptor"))
Employment_group$variable <- round(as.numeric(levels(Employment_group$variable)[Employment_group$variable]),0)
names(Employment_group) <- c("Country", "Subject.Descriptor", "Year", "Empl")

# Map Population
world <- map_data("world")
names(world)[5] <- "Country"
# Population
Population <- subset(gdp, WEO.Subject.Code == "LP")[,c(3,4,10:50)]
Population$Country <- as.character(Population$Country)
Population[which(Population$Country == "United States"),2] <- c("USA")
for (i in 3:43) {
    Population[,i] <- as.numeric(levels(Population[,i])[Population[,i]])
}
Population
Population_Table <- melt(Population, id = c("WEO.Subject.Code", "Country"))
names(Population_Table) <- c("WEO.Subject.Code", "Country", "Year", "Population")
Population_Table$Year <- round(as.numeric(levels(Population_Table$Year)[Population_Table$Year]),0)


# Merge
world_poppulation <- merge(world, Population, by = "Country", sort = FALSE)
# Define map
map <- map_data("world")

# =================================================================================================================================================
# UI
# =================================================================================================================================================
ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "IMF Data Analysis"),
    dashboardSidebar(
        sidebarMenu(
            sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
            menuItem(text = "National Account", tabName = "nationalAcc", icon = icon("dashboard")),
            menuItem(text = "Trade", tabName = "trade", icon = icon("route")),
            menuItem(text = "People", tabName = "people", icon = icon("user-friends"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "nationalAcc",
                    fluidRow(
                        box(imageOutput(outputId = "gdp", height = 550)),
                        box(imageOutput(outputId = "inflation", height = 550))
                    ),
                    fluidRow(
                        box(title = "GDP development", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "plot1", height = 260)),
                        box(title = "Gross domestic product, constant prices", status = "warning", solidHeader = TRUE, "Expressed in billions of national currency units", br(), collapsible = TRUE,
                            selectInput(inputId = "country", label = "Select countries", multiple = TRUE, choices = unique(GDP_development$Country), selected = "Germany")),
                        box(title = "Percentage change last 5 years", status = "primary", solidHeader = TRUE, collapsible = TRUE, tableOutput(outputId = "table1"))
                    ),
                    fluidRow(
                        box(title = "Total Investments Distribution", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "totalInvestments", height = 260)),
                        box(title = "Total Investments", status = "warning", solidHeader = TRUE, "Expressed as a ratio of total investment in current local currency and GDP in current local currency", br(), collapsible = TRUE,
                            selectInput(inputId = "country2", label = "Select countries", multiple = TRUE, choices = unique(totalInvestment$Country), selected = "Germany"),
                            sliderInput(inputId = "Year", label = "Select a Year range", min = min(totalInvestment$Year), max = max(totalInvestment$Year), value = c(1980,2020), ticks = FALSE, dragRange = TRUE, sep = ""))
                    )
                ),
            tabItem(tabName = "trade",
                    fluidRow(
                        box(title = "Volume of imports / exports of goods (and services)", status = "warning", solidHeader = TRUE, "Percent change of volume of imports / exports refers to the aggregate change in the quantities of total imports / exports", br(), collapsible = TRUE,
                            selectInput(inputId = "country3", label = "Select countries", multiple = TRUE, choices = unique(IM_goods_services$Country), selected = "Germany"),
                            sliderInput(inputId = "Year2", label = "Select a Year range", min = min(IM_goods_services$Year), max = max(IM_goods_services$Year), value = c(1980,2020), ticks = FALSE, dragRange = TRUE, sep = ""), 
                            h4("Chose between Volume of Goods and Volume of Goods and Services"),
                            hr(actionButton(inputId = "goods", label = "Goods"), actionButton(inputId = "goods_services", label = "Goods & Services")),
                            width = "100%")
                    ),
                    fluidRow(
                        box(title = "Volume of imports of goods (and services)", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "imports", height = 300)),
                        box(title = "Volume of exports of goods (and services)", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "exports", height = 300))
                    )
                    ),
            tabItem(tabName = "people",
                    fluidRow(
                        box(title = "Unemployment rate & Employment development", status = "warning", solidHeader = TRUE, "Unemployment rate can be defined by either the national definition, the ILO harmonized definition, or the OECD harmonized definition. <br> Employment can be defined by either the national definition, the ILO harmonized definition, or the OECD harmonized definition.", br(), collapsible = TRUE, width = "100%",
                        tabsetPanel(id = "tabset",
                                    tabPanel(title = "Country",
                                             selectInput(inputId = "country4", label = "Select countries", multiple = TRUE, choices = unique(Unemployment$Country), selected = "Germany"),
                                             sliderInput(inputId = "Year3", label = "Select a Year range", min = min(Unemployment$Year), max = max(Unemployment$Year), value = c(1980,2020), ticks = FALSE, dragRange = TRUE, sep = ""),
                                             ),
                                    tabPanel(title = "Country Group",
                                             selectInput(inputId = "country5", label = "Select country groups", multiple = TRUE, choices = unique(Unemployment_group$Country), selected = "Advanced economies"),
                                             sliderInput(inputId = "Year4", label = "Select a Year range", min = min(Unemployment_group$Year), max = max(Unemployment_group$Year), value = c(1980,2020), ticks = FALSE, dragRange = TRUE, sep = ""))
                                    ),
                        actionButton("go", "Plot it")
                        )
                    ),
                    fluidRow(
                        box(title = "Unemployment rate", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "unemployment", height = 300)),
                        box(title = "Employment (Persons in Million)", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotlyOutput(outputId = "employment", height = 300))
                    ),
                    fluidRow(
                        box(title = "Population development by country", status = "warning", solidHeader = TRUE, "For census purposes, the total population of the country consists of all persons falling within the scope of the census.", collapsible = TRUE, width = "100%",
                        selectInput(inputId = "Year5", label = "Select a Year", multiple = FALSE, choices = c(1980:2020), selected = 2000))
                    ),
                    fluidRow(
                        box(
                        plotOutput(outputId = "map")),
                        box(title = "Select your choise of countries by Population (in Million)", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                            selectInput(inputId = "country6", label = "Select countries", multiple = TRUE, choices = unique(Population_Table$Country), selected = "Germany"),
                            tableOutput(outputId = "table2"))
                    )
                )
        )
    )
)


# =================================================================================================================================================
# SERVER
# =================================================================================================================================================
server <- function(input, output) {
    
    # National Account
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    
    output$gdp <- renderImage({
        outfile <- tempfile(fileext = ".gif")
        # now make the animation
        p = ggplot(data = subset(weo_table, Country.Group.Name %in% c("World", "European Union", "Middle East and Central Asia")), 
                  mapping = aes(x = variable, y = value, fill = hi_lo)) +
            geom_col(position = "dodge2") +
            facet_wrap(~Country.Group.Name, nrow = 1, ncol = 3) +
            guides(fill = FALSE) +
            scale_fill_manual(values = c( "#009E73", "#D55E00")) +
            labs(title = "Development of GDP for EU, Asia, and World",
                 x = NULL,
                 y = "Percent change",
                 subtitle = "Date: {closest_state} (Min: 1980, Max: 2020)") +
            transition_states(variable, wrap = FALSE) +
            shadow_mark() +
            enter_grow() +
            enter_fade()
        
        anim_save("outfile.gif", animate(p, fps = 2))
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = "image/gif",
             width = "100%",
             height = 500
        )}, deleteFile = TRUE)
    
    output$inflation <- renderImage({
        outfile <- tempfile(fileext = ".gif")
        # now make the animation
        p2 = ggplot(data = subset(weo_table2, Country.Group.Name %in% c("World", "European Union", "Middle East and Central Asia")), 
                   mapping = aes(x = variable, y = value, fill = hi_lo)) +
            geom_col(position = "dodge2") +
            facet_wrap(~Country.Group.Name, nrow = 1, ncol = 3) +
            guides(fill = FALSE) +
            scale_fill_manual(values = c( "#009E73", "#D55E00")) +
            labs(title = "Development of Inflation for EU, Asia, and World",
                 x = NULL,
                 y = "Percent change",
                 subtitle = "Date: {closest_state} (Min: 1980, Max: 2020)") +
            transition_states(variable, wrap = FALSE) +
            shadow_mark() +
            enter_grow() +
            enter_fade()
        
        anim_save("outfile.gif", animate(p2, fps = 2))
        
        # Return a list containing the filename
        list(src = "outfile.gif",
             contentType = "image/gif",
             width = "100%",
             height = 500
        )}, deleteFile = TRUE)
    
    output$plot1 <- renderPlotly({
        ggplotly(ggplot(data = subset(GDP_development, Country %in% c(input$country)), mapping = aes(x = Year, y = GDP, color = Country)) +
                     geom_line() +
                     scale_y_continuous(labels = scales::comma) +
                     guides(color = FALSE) +
                     labs(title = "GDP development",
                          y = NULL,
                          x = NULL))
    })
    
    output$table1 <- renderTable(subset(GDP_development_table, Country %in% c(input$country)))
    
    output$totalInvestments <- renderPlotly({
        ggplotly(ggplot(data = subset(totalInvestment, Country %in% c(input$country2) & Year >= input$Year[1] & Year <= input$Year[2]), 
                        mapping = aes(x = Country, y = TotalInvestment)) +
                     geom_violin(outlier.size = 0, alpha = .3, size = 0.2) +
                     geom_point(mapping = aes(color = Country), alpha = .2, position = "jitter") +
                     coord_flip() +
                     guides(color = FALSE) +
                     labs(title = "Distribution of the Total Investments by country",
                          y = "Total Investments",
                          x = NULL))
    })
    
    # Trade
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    
    v <- reactiveValues(data = IM_goods)
    observeEvent(input$goods_services, {
        v$data <- IM_goods_services
    })
    observeEvent(input$goods, {
        v$data <- IM_goods
    })
    
    output$imports <- renderPlotly({
        if (is.null(v$data)) return()
        ggplotly(ggplot(data = subset(v$data, Country %in% c(input$country3) & Year >= input$Year2[1] & Year <= input$Year2[2]),  
                        mapping = aes(x = IM, y = Year, fill = Country)) +
                     geom_col(alpha = .3, position = "dodge2") +
                     coord_flip() +
                     guides(fill = FALSE) +
                     labs(title = "Volume of imports of goods (and services)",
                          x = "Percent change of volume of imports",
                          y = NULL))
    })
    ex <- reactiveValues(data = EX_goods)
    observeEvent(input$goods_services, {
        ex$data <- EX_goods_services
    })
    observeEvent(input$goods, {
        ex$data <- EX_goods
    })
    output$exports <- renderPlotly({
        if (is.null(v$data)) return()
        ggplotly(ggplot(data = subset(ex$data, Country %in% c(input$country3) & Year >= input$Year2[1] & Year <= input$Year2[2]),  
                        mapping = aes(x = EX, y = Year, fill = Country)) +
                     geom_col(alpha = .3, position = "dodge2") +
                     coord_flip() +
                     guides(fill = FALSE) +
                     labs(title = "Volume of exports of goods and services",
                          x = "Percent change of volume of exports",
                          y = NULL))
    })
    
    
    
    # People
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    v <- reactiveValues(doPlot = FALSE)
    
    observeEvent(input$go, {
        v$doPlot <- input$go
    })
    
    observeEvent(input$tabset, {
        v$doPlot <- FALSE
    })  
    
    output$unemployment <- renderPlotly({
        if (v$doPlot == FALSE) return()
        
        isolate({
            data <- if (input$tabset == "Country") {
                ggplot(data = subset(Unemployment, Country %in% c(input$country4) & Year >= input$Year3[1] & Year <= input$Year3[2]),  
                       mapping = aes(x = Year, y = Empl, color = Country)) +
                    geom_line() +
                    guides(color = FALSE) +
                    labs(title = "Unemployment development by country",
                         y = "Unemployment rate")
            } else {
                ggplot(data = subset(Unemployment_group, Country %in% c(input$country5) & Year >= input$Year4[1] & Year <= input$Year4[2]),  
                       mapping = aes(x = Year, y = Empl, color = Country)) +
                    geom_line() +
                    guides(color = FALSE) +
                    labs(title = "Unemployment development by country group",
                         y = "Unemployment rate")
            }
            ggplotly(data)
        })
    })
    output$employment <- renderPlotly({
        if (v$doPlot == FALSE) return()
        
        isolate({
            data <- if (input$tabset == "Country") {
                ggplot(data = subset(Employment, Country %in% c(input$country4) & Year >= input$Year3[1] & Year <= input$Year3[2]),  
                       mapping = aes(x = Year, y = Empl, color = Country)) +
                    geom_line() +
                    guides(color = FALSE) +
                    labs(title = "Employment development by country",
                         y = "Employment (Persons in Million)")
            } else {
                ggplot(data = subset(Employment_group, Country %in% c(input$country5) & Year >= input$Year4[1] & Year <= input$Year4[2]),  
                       mapping = aes(x = Year, y = Empl, color = Country)) +
                    geom_line() +
                    guides(color = FALSE) +
                    labs(title = "Employment development by country group",
                         y = "Employment (Persons in Million)")
            }
            ggplotly(data)
        })
    })
    
    
    df <-reactive({
        
        for (i in 1980:2020) {
            if (input$Year5 == i) {
                df <- world_poppulation[, c(1:7, which(names(world_poppulation) == i))]
                names(df) <- c("Country", "long", "lat", "group", "order", "subregion", "WEO.Subject.Code", "Population")
                }
            else {
            df <- world_poppulation[, c(1:7,10)]
            names(df) <- c("Country", "long", "lat", "group", "order", "subregion", "WEO.Subject.Code", "Population")
            }
        return(df)
        }
    })
    
    
    output$map <- renderPlot({
        ggplot(df(), aes(fill = Population)) + 
            borders("world") +
            geom_map(aes(map_id = Country), map = map,) +
            expand_limits(x = map$long, y = map$lat) + 
            theme_void() +
            scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = scales::comma) +
            labs(fill = "Population in Million")
    })
    
    
    output$table2 <- renderTable(head(subset(Population_Table, Country %in% c(input$country6) & Year == input$Year5)[,c(2,4)], 10))
}

# =================================================================================================================================================
# RUN APPLICATION
# =================================================================================================================================================

shinyApp(ui = ui, server = server)



















