## data voor presentatie 20260609
packages <- c("RODBC",
              "lubridate",
              "RPostgres",
              "dplyr",
              "dotenv",
              "stringr",
              "tools",
              "dairyconsult",
              "tidyr",
              "readxl",
              "RMySQL",
              "ggplot2",
              "stringr",
              "patchwork",
              "gt",
              "ggiraph")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE))
  install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))
load_dot_env()

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

nms <- c("Lugtenberg",
         "Rops",
         "VOFOskam",
         "mtsspiker",
         "famvanleeuwen",
         'meervee',
         "compagner",
         'bloemert'
         )

klanten <- paste('("',
                 paste(nms,collapse = '","'),
                 '")',sep = "")
kids <- gsub("c","",paste(quer_ms("select ID 
                from tbl_999_users
                where USER in {klanten}"),collapse = ","))


mindat <- "2024-12-31"
maxdat <- "2026-06-01"

## eerst grafiek van % percentage krachtvoer en productie
stat <- str_glue('select USER_ID,
                  DATE,
                 FPCM,
                 AMOUNT_CONC_100_MILK AS KV100
                 FROM view_production_cost
                 where DATE BETWEEN "{mindat}" AND "{maxdat}"
                 and user_id IN {kids}')

kvv <- quer_ms(stat)

kvvplot <- ggplot(data = kvv,
                  mapping=aes(x= KV100,
                              y = FPCM,
                              color = factor(USER_ID),
                              tooltip=USER_ID,
                              data_id=DATE))+
  geom_point_interactive(size = 3,hover_nearest = T) + 
  labs(
    title = "Verband krachtvoerverbruik - Productie",
    x = "Krachtvoer 100 kg melk", y = "FPCM",
    color = "Bedrijf"
  ) +
  theme_bw()

## data voor grafiek gezondheidsalarmen
fls <- c("friesian.xlsx",
         "friesian_2.xlsx",
          "staebelow_1.xlsx",
          "staebelow_2.xlsx")


for(f in fls){
  if(f == fls[1])
    d <- data.frame()
  bedrijf <- str_extract(str_extract(f,"^[^.]+"),
                         "^[^_]+")
print(bedrijf)
  d <- rbind(d,read_xlsx(path = f) |> 
  select(1:8) |> 
  dplyr::rename(dier = 1,
         status = 2,
         groep =3,
         laktatie = 4,
         dil = 5,
         datum = 6,
         level = 7,
         tijd = 8) |> 
  mutate(datum = parse_date_time(datum,orders = "%d-%m-%Y, %H:%M")) |> 
  mutate(dil = ifelse(is.na(dil),-10,dil),
         laktatie = ifelse(is.na(laktatie),0,laktatie)) |> 
  mutate(bedrijf=str_sub(bedrijf,1,1))) |> 
    distinct() 
}

d1 <- d |> 
  filter(laktatie!=0) |> 
  group_by(bedrijf,laktatie) |> 
  summarise(n = length(dil)) |> 
  ungroup() |> 
  mutate(laktatie = factor(as.numeric(laktatie),ordered = TRUE))

lactplot <- ggplot(d1, aes(
  x = laktatie,
  y = n,
  tooltip = bedrijf,
  data_id = bedrijf,
  fill = bedrijf)) +
  geom_col_interactive(position='dodge')+
  labs(
    title = "Aantal gezondheidalarmen",
    x = "Laktatie", y = "Aantal",
    color = "Bedrijf"
  ) +
  theme_bw() +   theme(legend.position = "none")

d2 <- d |> 
  filter(laktatie>0,dil<500) |> 
  mutate(dilc = dil%/%50*50) |> 
  group_by(bedrijf,dilc) |> 
  summarise(n=length(dil)) |> 
  ungroup() |> 
  mutate(dilc = factor(dilc,ordered = TRUE))

dilplot <- ggplot(d2, aes(
  x = dilc,
  y = n,
  tooltip = bedrijf,
  data_id = bedrijf,
  fill = bedrijf)) +
  geom_col_interactive(position='dodge')+
  labs(
    title = NULL,
    x = "DIL",y= NULL,
    color = "Bedrijf"
  ) +
  theme_bw() +   theme(legend.position = "none")



d3 <- d |> 
  filter(laktatie>0,dil<500) |> 
  mutate(week = floor_date(datum,unit = "week")) |> 
  group_by(bedrijf,week) |> 
  summarise(n=length(dil))

tplot <- ggplot(d3,aes(
  x=week,y=n,tooltip=bedrijf,data_id=bedrijf,color = bedrijf)) + 
  geom_line_interactive(linewidth = 1)+
  theme_bw() +   theme(legend.position = "none")

combined_plot <- (lactplot + dilplot)/ tplot

## celgetalplot
bedrijf <- 'Milchhof Friesian'
farms_id <- quer_pg("select id from farms where name = '{bedrijf}'")

mdf <- quer_pg('select pedigree_id,
                date_time,
                lactation,
                dim,
                production,
                cells
                from view_melkcontrole 
                where farms_id = {farms_id}')

mmdf <- mdf |> 
  filter(date_time>"2020-01-01")|> 
  group_by(date_time) |> 
  filter(n()>1000) |> 
  ungroup() |> 
  mutate(nlactcat = ifelse(lactation>3,"4+",lactation),
         dilcat = cut(dim,breaks = c(0,30,60,120,200,2000),
                      labels= c("0-30 DIL","31-60 DIL","61-120 DIL","121-200 DIL","200+"))) |> 
  filter(!is.na(dilcat))



clactplot <- mmdf |> 
  group_by(nlactcat) |> 
  summarise(mn = mean(cells,na.rm = T)) |> 
  ungroup() |> 
  ggplot(aes(
  x = nlactcat,
  y = mn,
  tooltip = nlactcat)) +
  geom_col_interactive(position='dodge')+
  labs(
    title = "Celgetal",
    x = "Laktatie", y = "Gemiddelde",
  ) +
  theme_bw() +   theme(legend.position = "none")

cdilplot <- mmdf |> 
  group_by(dilcat) |> 
  summarise(mn = mean(cells,na.rm = T)) |> 
  ungroup() |> 
  ggplot(aes(
    x = dilcat,
    y = mn,
    tooltip = dilcat)) +
  geom_col_interactive(position='dodge')+
  labs(
    title = NULL,
    x = "Laktatiedagen", y = NULL,
  ) +
  theme_bw() +   theme(legend.position = "none")

ctplot <- mmdf |> 
  mutate(week = floor_date(date_time, unit = "weeks", week_start = 1)) |> 
  group_by(week,nlactcat) |> 
  summarise(mn = mean(cells,na.rm = T)) |> 
  ungroup() |> 
  ggplot(aes(
    x = week,
    y = mn,
    data_id=nlactcat,color = nlactcat,
    tooltip = week)) +
  geom_line_interactive(linewidth = 1)+
  labs(
    title = NULL ,
    x = "Datum", y = "Gemiddelde",
  ) +
  theme_bw() +   theme(legend.position = "none")

combined_plot_c <- (clactplot + cdilplot)/ ctplot

## data MPR-en Friesian
FARM <- "Milchhof Friesian"
farms_id <- quer_pg("select id from farms where name = '{FARM}'")


mlp <- quer_pg("select * from view_melkcontrole 
               where farms_id = {farms_id} 
               and cells is not null
               and date_time > '2024-01-01'")

mlp$date_time <- format.Date(mlp$date_time,"%Y-%m-%d")

ord <- unique(mlp$date_time)
ord <- ord[order(ord,decreasing = T)]
ids <- unique(mlp$pedigree_id[mlp$date_time%in%ord[1:3]])

tab <- mlp |> 
  filter(pedigree_id%in%ids) |> 
  group_by(reg_id,date_time) |>
  summarise(cells = cells) |>
  arrange(match(date_time,ord)) |>
  pivot_wider(names_from = c(date_time),values_from = cells)

tab2 <- mlp |> 
  filter(pedigree_id%in%ids&date_time%in%ord[1]) |> 
  mutate(contrib = cells*production/sum(cells*production,na.rm = T)*100,
         acidose = ifelse(protein_pct>fat_pct&fat_pct<4,1,0),
         ketose = ifelse(((fat_pct - protein_pct)>1.5&protein_pct < 3.25),1,0))|> 
  group_by(reg_id,internal_farm_id) |>
  summarise(dim = dim,
            lactation = lactation,
            production = production,
            fat_pct = fat_pct,
            protein_pct = protein_pct,
            acidose,
            ketose,
            contrib = round(contrib,2))

tab3 <- inner_join(tab2,tab,by = c(
  "reg_id" = "reg_id")) |> 
  arrange(desc(contrib)) |> 
  ungroup() |> 
  select(-reg_id,-ketose,-acidose) |> 
  dplyr::rename(id = "internal_farm_id") |> 
  top_n(100)

cns <- grep("[[:digit:]]{4}",names(tab3))
target_cols <- names(tab3[cns])

# 3. Build the table using all_of()
tabcells <- tab3 %>%
  gt() %>%
  data_color(
    columns = all_of(target_cols),
    fn = function(x) ifelse(!is.na(x) & x > 100, "red", "transparent")
  ) |> 
  sub_missing(columns = all_of(target_cols), missing_text = "—") |> 
  opt_interactive(
    use_sorting = TRUE,
    use_search = FALSE
  )


## Gewichten
wfile <- "customized-dairy-heifer-growth-chart-metric-spreadsheet.xlsx"
wf <-   read_xlsx(wfile,sheet = "Input Heifer Data") |> 
  mutate(birth_date = parse_date_time(gsub("[' ] ","",birth_date),c("%d/%m/y","%d/%m/%Y")),
         date_time = parse_date_time (date,"%Y-%m-%d"),
         farm = 'Fr') |> 
  select(farm,id,birth_date,date_time,weight_kg)


FARM <- "Hof Karp"
farms_id <- quer_pg("select id from farms where name = '{FARM}'")

wk <-   data <- quer_pg("select 'Hk' as farm,p.reg_id id,p.birth_date,w.date_time,w.weight weight_kg
                from weights w 
                inner join pedigree p on p.id = w.pedigree_id
                where p.farms_id = {farms_id}
                and breed = 'HF'
                and sex = 'f'")

rbind(wf,wk)

doel <- read_xlsx(wfile,sheet = "Custom Table",
                  range = "B5:C34") |>  
  dplyr::rename(age = 1,
         weight_kg = 2)


wplot <- rbind(wf,wk) |> 
  mutate(age = as.numeric(difftime(date_time,birth_date,units = 'days'))) |> 
  filter(!(farm == "Hk"&age > 200)) |> 
  ggplot(
    mapping=aes(x= age,
                y = weight_kg))+
  geom_point_interactive(size = 1,
                         hover_nearest = T,
                         aes(x=age,
                             data_id = id,
                             y = weight_kg,
                             color = farm,
                             tooltip=id))+ 
  geom_line_interactive(data = doel,
                         linewidth=2, alpha=0.4,color = 'cadetblue2',
                        aes(x=age,y= weight_kg)) + 
  labs(
    title = "Gewicht kalveren en jongvee",
    x = "Leeftijd", y = "Gewicht (kg)",
    color = "Bedrijf"
  ) +
  theme_bw()

wtplot <- wk |> 
  mutate(age = as.numeric(difftime(date_time,birth_date,units = 'days'))) |> 
  filter(!(age > 200)) |> 
  mutate(cage = cut(age,breaks = c(-1,10,100,200),
                    labels = c('geboorte','10-100 dagen','100-200 dagen'))) |> 
  mutate(month = floor_date(date_time, unit = "quarter")) |> 
  ggplot(
    mapping=aes(x = date_time,y = weight_kg,color = cage)) +
  geom_point_interactive(size = 1,
                         hover_nearest = T,
                         aes(data_id = id,
                             tooltip=id)) +
  stat_summary(aes(x=month,y = weight_kg,group=cage), fun.y=median, geom="line", colour="green") + 
  labs(
    title = "Gewicht bedrijf Hk over tijd",
    x = "Datum", y = "Gewicht (kg)",
    color = "Leeftijd"
  ) +
  theme_bw()

combined_plot_w <- wplot  /  wtplot


girafe(
  ggobj = combined_plot_w,
  options = list(
    opts_sizing(rescale = TRUE, width = 1),
    opts_hover(css = "r: 5pt; stroke: #000; transition: all 0.2s ease-in-out;"),
    # Optioneel: Maak niet-geactiveerde groepen een beetje transparant
    opts_hover_inv(css = "opacity: 0.5;")
  )
)

kvvplot <- ggplot(data = kvv,
                  mapping=aes(x= KV100,
                              y = FPCM,
                              color = factor(USER_ID),
                              tooltip=USER_ID,
                              data_id=DATE))+
  geom_point_interactive(size = 3,hover_nearest = T) + 
  labs(
    title = "Verband krachtvoerverbruik - Productie",
    x = "Krachtvoer 100 kg melk", y = "FPCM",
    color = "Bedrijf"
  ) +
  theme_bw()

save(kvvplot,combined_plot,combined_plot_c,tabcells,combined_plot_w,file = '20260609plots.Rdata')

  


