
rm(list = ls())
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





## TEST =====

devtools::install_github("briatte/ggnet", force = TRUE)
library(ggnet)
library(plotly)
library(network)
n <- data.frame(event1 = c(-0.2,0.8,0.4,0),
                event2 = c(0.34,-0.17,0.3,0),
                event3 = c(0.2,0.1,-0.73,0),
                row.names = letters[1:4])
net <- network(n,
               matrix.type = "bipartite",
               ignore.eval = FALSE,
               names.eval = "weights")
net
plot(net)

col = c("actor" = "grey", "event" = "gold")

set.edge.attribute(net, "color", ifelse(net %e% "weights" > 0, "green", "red"))

g <- ggnet2(net, color = "mode", 
            palette = col, 
            label = TRUE, 
            edge.color = "color",
            edge.label="weights")

gp <- ggplotly(g)

plot(g)

htmlwidgets::saveWidget(as_widget(gp), "ggplot.html")
