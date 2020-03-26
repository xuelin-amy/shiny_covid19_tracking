library(shiny)
library(shinydashboard)
library(glue)
library(googleVis)
source('functions.R')
url1 <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url2 <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
url3 <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'

province.map.cn <- fread('./china_province_name_map.csv', encoding = 'UTF-8')
pieces.china = list(
    list(min = 10000),
    list(min = 1000, max = 10000),
    list(min = 100, max = 1000),
    list(min = 10, max = 100),
    list(max = 10)
)

pieces.world = list(
    list(min = 10000),
    list(min = 1000, max = 10000),
    list(min = 100, max = 1000),
    list(min = 10, max = 100),
    list(max = 10)
)

df1 <- get.data(url1,'confirmed')
df2 <- get.data(url2, 'recovered')
df3 <- get.data(url3, 'death')
df <- df1 %>%
    left_join(df2) %>%
    left_join(df3) %>%
    group_by(Country.Region, Province.State) %>%
    do(arrange(.,date)) %>%
    mutate(confirmed_new = confirmed - lag(confirmed, 1, 0),
           recovered_new = recovered - lag(recovered, 1, 0),
           death_new = death - lag(death, 1, 0)) %>%
    ungroup()

df.country <- df %>%
    group_by(Country.Region, date) %>%
    summarise_at(vars(starts_with('confirmed'), 
                      starts_with('recovered'),
                      starts_with('death')), sum)

kpi.values <- get.kpi(df.country)

country.sel <- selectInput("country", 
                           "",
                           df.country %>% 
                               arrange(desc(confirmed)) %>%
                               distinct(Country.Region) %>%
                               pull(Country.Region),
                           selected = 'Mainland China')
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = 'styles.css',
    # Application title
    title = "COVID-19 Status Tracking",
    titlePanel(title = "COVID-19 Status Tracking"),
    # data source -----        
    fluidRow(
        column(width = 12,
               HTML('<i>data source: <a href="https://github.com/CSSEGISandData/COVID-19">Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE</a></i>'),
               tags$br(),
               tags$i(glue('latest update on {kpi.values$updated.date}')),
               tags$hr()  
        )
    ),
    
    # KPI world -----
    fluidRow(
        column(width = 12,
               tags$div(class = 'valueBoxTitle',
                        'Worldwide')
        )
        
    ),
    
    fluidRow(
        tags$div(class = 'valueBox',
                 id = 'confirmed',
                 'Total Confirmed',
                 tags$br(),
                 format(kpi.values$confirmed, big.mark = ','),
                 tags$sup(glue("({format(kpi.values$confirmed_new, big.mark = ',')} new)"))
        ),
        tags$div(class = 'valueBox',
                 id = 'recovered',
                 'Total Recovered',
                 tags$br(),
                 format(kpi.values$recovered, big.mark = ','),
                 tags$sup(glue("({format(kpi.values$recovered_new, big.mark = ',')} new)"))
        ),
        tags$div(class = 'valueBox',
                 id = 'death',
                 'Total Death',
                 tags$br(),
                 format(kpi.values$death, big.mark = ','),
                 tags$sup(glue("({format(kpi.values$death_new, big.mark = ',')} new)"))
        )
    ),
    tags$hr(),
    # map -----
    fluidRow(
        tabBox(width = 12,
               tabPanel('Table',
                        htmlOutput('world.table')),
               tabPanel('Map',
                        checkboxInput('map.show.anim', 'show animation'),
                        echarts4rOutput('world.map'))
               )
        ),
    
    tags$hr(),
    # KPI country ------
    fluidRow(
        column(width = 12, 
               uiOutput('country.valueBoxTitle'))
    ),
    fluidRow(
        uiOutput('country.valueBox')
    ),
    
    fluidRow(column(width = 2, country.sel)),

    # trend -----
    fluidRow(
        tabBox(width = 12,
               tabPanel('New cases - Country',
                        echarts4rOutput('country.trend.chart.new')),
               tabPanel('Total cases - Country',
                        echarts4rOutput('country.trend.chart')))
    ),
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rvs <- reactive({
        list(country = input$country,
             map.show.anim = input$map.show.anim)
    })
    
    kpi.country <- reactive({
        get.kpi(df.country, country = input$country)
    })
    
    output$country.trend.chart <- renderEcharts4r({
        plot.country.trend(df.country, 
                           rvs()$country,F)
    })
    
    output$country.trend.chart.new <- renderEcharts4r({
        plot.country.trend(df.country, 
                           rvs()$country,T)
    })
    
    # output$country.map <- renderEcharts4r({
    #     plot.country.map(df, 'China', pieces = pieces.china, 
    #                      province.map.cn = province.map.cn, 
    #                      show.anim = rvs()$map.show.anim)
    # })
    
    output$world.table <- renderGvis({
        plot.world.table(df.country, pageSize = 20, width = 1400)
    })
    
    output$world.map <- renderEcharts4r({
        plot.world.map(df.country, pieces.world, 
                       show.anim = rvs()$map.show.anim)
    })
    
    output$country.valueBoxTitle <- renderUI({
        tags$div(class = 'valueBoxTitle',
                 rvs()$country)
    })
    
    output$country.valueBox <- renderUI({
        list(
            tags$div(class = 'valueBox',
                     id = 'confirmed',
                     'Total Confirmed',
                     tags$br(),
                     format(kpi.country()$confirmed, big.mark = ','),
                     tags$sup(glue("({format(kpi.country()$confirmed_new, big.mark = ',')} new)"))),
            tags$div(class = 'valueBox',
                     id = 'recovered',
                     'Total Recovered',
                     tags$br(),
                     format(kpi.country()$recovered, big.mark = ','),
                     tags$sup(glue("({format(kpi.country()$recovered_new, big.mark = ',')} new)"))),
            tags$div(class = 'valueBox',
                     id = 'death',
                     'Total Death',
                     tags$br(),
                     format(kpi.country()$death, big.mark = ','),
                     tags$sup(glue("({format(kpi.country()$death_new, big.mark = ',')} new)")))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
