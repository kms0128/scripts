## DEPENDENCY GRAPH ===================================================
# http://datastorm-open.github.io/DependenciesGraphs/

devtools::install_github("DataKnowledge/DependenciesGraphs")
require(DependenciesGraphs)

# you mus first loaded the target package using library
library(plyr,quietly = TRUE)
dep <- funDependencies("package:plyr","count")
plot(dep)
plot(dep,block=TRUE)


dep2 <- Pck.load.to.vis("plyr")
plot(dep2)

dep3 <- Pck.load.to.vis(c("htmlwidgets", "shiny"))
plot(dep3)

launch.app()

## SANKEY DIAGRAM ======================================================
# https://backingup.tistory.com/15

## Let's practice!
testData <- read_excel("input/testData_samsung.xlsx",
                       sheet = "testData", 
                       col_names = TRUE,
                       col_types = c("date", "text", "text", "text", "numeric", "text", "text", "text")) %>%
  filter(DATETIME >= as.Date("2020-07-01 00:00:00") &
           DATETIME <= as.Date("2020-07-31 23:59:59") &
           str_length(USERID) == 36 &
           str_length(message) > 1) %>% 
  mutate(date = strftime(DATETIME, tz="UTC",format = "%Y-%m-%d")) %>%
  group_by(USERID, date) %>% #mean session (daily calculation)
  arrange(USERID, DATETIME) %>% 
  mutate(total_laps = as.numeric(DATETIME-lag(DATETIME), units = 'secs'),
         is.session = ifelse(is.na(total_laps) | total_laps > 1*10*60, 1,0), # first or over 10 min
         session_ord = cumsum(is.session), 
         laps = ifelse(is.session == 0, as.numeric(DATETIME-lag(DATETIME), units = 'secs'), NA),
         is.handled = case_when(str_detect(INTENT, 'fallback') ~ 0,
                                !str_detect(INTENT, 'fallback') ~ 1),
         daily_session_out = ifelse(DATETIME == last(DATETIME), 1, 0),
         is.turnover = case_when(DATETIME == first(DATETIME) ~ 0,
                                 DATETIME == last(DATETIME) ~ 1,
                                 TRUE ~ daily_session_out),
         msg_length = str_length(message), 
         intent_length = str_length(INTENT),
         entity_length = str_length(ENTITY),
         entityValue_length = str_length(entity_value)) %>% ungroup()

df <- testData %>% 
  select(DATETIME, date, USERID, INTENT, session_ord) %>% 
  group_by(USERID, date, session_ord) %>% 
  arrange(USERID, DATETIME) %>% 
  mutate(flowOrder = row_number(DATETIME),
         node = paste0(flowOrder,"/",INTENT)) %>% ungroup()

df2 <- df %>% 
  ungroup() %>% 
  filter(IsOdd(flowOrder)== TRUE) %>% 
  select(USERID, date, flowOrder, INTENT) 

df3 <- df %>% 
  ungroup() %>% 
  filter(IsOdd(flowOrder)== FALSE ) %>% 
  select(USERID, date, flowOrder, INTENT) 

df4 <- left_join(df2, df3, by = c("USERID", "date"))
df5 <- left_join(df3, df2, by = c("USERID", "date"))

links_df <- rbind(df4, df5) %>% 
  arrange(USERID, date) %>% 
  filter( flowOrder.y == flowOrder.x+1 ) %>% 
  mutate(flowOrder = paste(flowOrder.x, flowOrder.y),
         INTENT = paste(INTENT.x, INTENT.y)) %>% 
  ungroup() %>% 
  group_by(flowOrder, INTENT) %>% 
  summarise(source = INTENT.x,
            target = INTENT.y,
            value = n(USERID)) %>% 
  ungroup() %>% 
  select(source, target, value) %>% 
  unique() 


nodes_df <- data.frame(name=c(as.character(links_df$source), 
                              as.character(links_df$target)) %>% 
                         unique() ) 

links_df$IDsource <- match(links_df$source, nodes_df$name)-1 
links_df$IDtarget <- match(links_df$target, nodes_df$name)-1 

links_df1 <- links_df %>% filter(IDsource >10 & IDtarget >10 & value > 100)

# Make the Network 
p <- sankeyNetwork(Links = links_df, 
                   Nodes = nodes_df, 
                   Source = "IDsource", 
                   Target = "IDtarget", 
                   Value = "value", 
                   NodeID = "name", 
                   sinksRight=FALSE,
                   NodeGroup = NodeID,
                   LinkGroup = NULL, units = "",
                   colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
                   fontSize = 7,
                   fontFamily = NULL, nodeWidth = 15, nodePadding = 10, margin = NULL,
                   height = NULL, width = NULL, iterations = 32, sinksRight = TRUE)

p


  