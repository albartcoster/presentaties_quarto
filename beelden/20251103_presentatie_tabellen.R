## data voor presentatie 20251003
packages <- c("RODBC",
              "lubridate",
              "RPostgres",
              "dplyr",
              "dotenv",
              "stringr",
              "tools",
              "tidyr",
              "openxlsx",
              "RMySQL",
              "echarts4r",
              "lubridate",
              "shiny",
              "ggplot2")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE))
  install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))

path <- 'C:/Users/Eigenaar/OneDrive - Dairyconsult/bedrijf/Dairyconsult_Albart'
path2 <- "punt_envs"
load_dot_env(file.path(path,path2,".env_postgres"))
load_dot_env(file.path(path,path2,".env_voerkosten"))

for(VAR in c(
  'PG_HOST',
  'PG_DB',
  'PG_USER',
  'PG_PWD',
  'MS_HOST',
  'MS_DB',
  'MS_USER',
  'MS_PWD'
  )){
  assign(VAR, Sys.getenv(VAR))
  if(get(VAR) == '') stop(paste0('Missing ', VAR))
}

if(!exists('pgdb') || !dbIsValid(pgdb)){
  pgdb <- dbConnect(Postgres(), host = PG_HOST, dbname = PG_DB, user = PG_USER, pass = PG_PWD)
  quer_pg <- function(...) dbGetQuery(pgdb, str_glue(paste0(...)))
  pgex <- function(...) dbExecute(pgdb, paste0(...))
}

if(!exists('msdb')){
  msdb <- dbConnect(MySQL(), host = MS_HOST, dbname = MS_DB, user = MS_USER, pass = MS_PWD)
  quer_ms <- function(...) dbGetQuery(msdb, str_glue(paste0(...)))
  msex <- function(...) dbExecute(msdb, paste0(...))
}

lbls <- c('0-30 DIL',
          '31-60 DIL',
          '61-200 DIL',
          '201-300 DIL',
          '300-400 DIL')

stat <- paste("select farms_id,lactation,dim,date_time,pedigree_id,production,ureum,fat_pct,cells from view_melkcontrole 
              where farms_id in (293,244,46)
              and date_time > '2021-01-01' 
              and lactation < 6
              and cells >0
              and dim < 401") # Friesian,Spiker,pot, KI Samen,milsana
df0 <- quer_pg(stat)
df <- df0 |> 
  filter(!is.na(dim)&!is.na(production)) |> 
  filter(production>0) |> 
  mutate(nlactcat = cut(lactation,
                        breaks = c(0,1,2,6),
                        labels = c("1e kalfs","2e kalfs","3e kalfs")),
         jaar = year(date_time),
         dt = format.Date(date_time,"%m-%d"),
         dilcat = cut(dim,breaks = c(-1,30,60,200,300,401),labels = lbls),
         increased = ifelse(lactation==1,cells>150,cells>200)) |> 
  arrange(farms_id,pedigree_id,date_time) |> 
  group_by(pedigree_id) |> 
  mutate(pincreased = lag(increased)) |> 
  mutate(pincreased = ifelse(is.na(pincreased),0,pincreased))|> 
  mutate(cstat = ifelse(increased == 0,
                        ifelse(pincreased == 0,'goed','genezen'),
                        ifelse(pincreased ==0,'nieuw','chronisch'))) 

## Shiny voor verloop ureum

grafiek_ureum <- function(df = NULL){
  library(shiny)
  
  shinyApp(
  ui = fluidPage(
    selectInput('farm','Bedrijf:',
                choices = unique(df$farms_id)),
    sliderInput(
      'lactations',"Laktaties:",
      min = 1,max = max(df$lactation),
      step = 1,
      value = c(1,3)
    ),
    echarts4rOutput("plot")
  ),
  
  server = function(input, output) {
    output$plot = renderEcharts4r({
      df |> filter(farms_id==input$farm&
                     ureum>0&
                     lactation>=input$lactations[1]&
                     lactation<=input$lactations[2]) |> 
        group_by(farms_id,date_time,dilcat) |> 
        summarise(ureum = sum(production*ureum,na.rm = T)/sum(production,na.rm = T)) |> 
        group_by(dilcat) |> 
        e_charts(date_time) |> 
        e_line(ureum,symbol ="none") |> 
             e_title(input$lactation) |> 
        e_datazoom(y_index = 0, type = "slider") |> 
        e_datazoom(x_index = c(0,1), type = "slider") 
    })
  },
  options = list(height = 500)
)
}

## grafiek met celclasses en celgetallen
grafiek_celclasses <- function(df=NULL,classes=FALSE){
  library(shiny)
  shinyApp(
  ui = fluidPage(
    selectInput('farm','Bedrijf:',
                choices = unique(df$farms_id),
                selectize = T,
                multiple = T),
    sliderInput(
     'lactations',"Laktaties:",
     min = 1,max = max(df$lactation),
     step = 1,
     value = c(1,3)
    ),
    sliderInput( 
      "dils", "Laktatiedagen", 
      min = 0, max = max(df$dim)+1, 
      value = c(30, 60) 
    ), 
    echarts4rOutput("plot0"),
    # if(classes)
    #   echarts4rOutput("plot1")
  ),
  
  server = function(input, output) {
    dt <- reactive({
      df |> 
        filter(cells > 0&
                 !is.na(cells)&
                 farms_id%in%input$farm&
                 lactation>=input$lactations[1]&
                 lactation<=input$lactations[2]&
                 input$dils[1]<=dim&dim<input$dils[2]) |>
        ungroup()
    })
    output$plot0 = renderEcharts4r({
      dt() |> 
        group_by(date_time,farms_id) |> 
        summarise(cells = sum(production*cells,na.rm = T)/sum(production,na.rm = T)) |> 
        group_by(farms_id) |> 
        e_charts(date_time) |> 
        e_line(cells,symbol="none") |> 
        e_datazoom(y_index = 0, type = "slider") |> 
        e_datazoom(x_index = c(0,1), type = "slider") 
      
    })})}
    
  
    
    
    # if(classes){
    # output$plot1 = renderEcharts4r({
    #   dt() |>
    #     count(date_time,farms_id,cstat) |>
    #     group_by(csat,date_time,farms_id) |>
    #     mutate(prop = prop.table(n)) |>
    #     ungroup() |> 
    #     group_by(farms_id,cstat) |> 
    #      e_charts(date_time) |>
    #     e_line(prop,symbol="none") |>
    #     e_datazoom(y_index = 0, type = "slider")


## shiny voor verloop productie
stat <- paste("select farms_id,lactation,dim,date_time,pedigree_id,production from view_productions 
              where farms_id in (293,244,220,334)
              and lactation < 4
              and production>0
              and date_time > '2020-01-01'
              and dim < 401") # Friesian,Karp,KI
df2 <- quer_pg(stat)
df2 <- df2 |> 
  filter(!is.na(dim)&!is.na(production)) |> 
  filter(production>0) |> 
  mutate(nlactcat = factor(lactation,labels = c("1e kalfs","2e kalfs","3e kalfs")),
         jaar = year(date_time),
         dt = format.Date(date_time,"%m-%d"),
         dilcat = cut(dim,breaks = c(-1,30,60,200,300,401),labels = lbls))


## app productie
grafiek_productie <- function(df2=NULL){
  library(shiny)
  
shinyApp(
  ui = fluidPage(
    selectInput('farm','Bedrijf:',
                choices = unique(df2$farms_id),
                selectize = T,
                multiple = T),
    sliderInput(
      'lactations',"Laktaties:",
      min = 1,max = max(df2$lactation),
      step = 1,
      value = c(1,3)
    ),
    sliderInput( 
      "dils", "Laktatiedagen", 
      min = 0, max = max(df2$dim)+1, 
      value = c(30, 60) 
    ), 
    echarts4rOutput("plot")
  ),
  server = function(input, output) {
    output$plot = renderEcharts4r({
      df2 |> 
        filter(farms_id%in%input$farm&
                     lactation>=input$lactations[1]&
                      lactation<=input$lactations[2]&
                 dim>=input$dils[1]&
               dim<=input$dils[2]) |>
        group_by(farms_id,date_time) |> 
        summarise(production = mean(production,na.rm = T)) |> 
        arrange(farms_id,date_time)|> 
        mutate(production = as.numeric(smooth(production,kind = "3RS3R"))) |> 
        group_by(farms_id) |> 
        e_charts(date_time) |> 
        e_line(production,symbol="none",smooth='true') |> 
        e_title(input$lactation) |> 
        e_datazoom(y_index = 0, type = "slider") |> 
      e_datazoom(x_index = c(0,1), type = "slider") 
    })
  },
  options = list(height = 500)
)
}

save(
  grafiek_celclasses,
  grafiek_productie,
  grafiek_ureum,
  df,df2,file = '20251103.Rdata')






























