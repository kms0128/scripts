## Conversationi flow and pre-processing  ======================================

rm(list=ls())

## IMPORT data  ================================================================
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


for (a in 1:length(testData$is.turnover)) {
  testData$is.turnover[a] = case_when(testData$total_laps[a+1] > 1*10*60 ~ 1,
                                      TRUE ~ testData$is.turnover[a])
  
}

## Create TEXT data set  =======================================================

getwd()
setwd("C:/Users/r_admin/Desktop/git_script/scripts") 

library(arules)


df_intent <- testData %>% 
  select(date, USERID, INTENT, DATETIME) %>% 
  arrange(USERID, DATETIME) %>% 
  group_by(USERID, date) %>% 
  transmute(DATETIME = DATETIME,
            transID = paste0(USERID, "_", date),
            num_intent = n(INTENT),
            itemList = list(INTENT)) %>% 
  ungroup() %>% 
  unique() 

intent_list <- df_intent %>% select(DATETIME, transID, itemList)

tr_intent <- intent_list$itemList
b <- c(df_intent$transID)

## All Intent Analysis =========================================================

names(tr_intent) <- paste("tr", c(1:length(tr_intent)), sep = "_")


tr_intent <- as(tr_intent, "transactions")
tr_intent
summary(tr_intent) 

inspect(tr_intent[1:10])
itemFrequency(tr_intent[,1:10])

# control support value ----------
support_value = 0.01
itemFrequencyPlot(tr_intent, 
                  support = support_value,
                  main = paste0("item frequency plot above support ", 
                                support_value*100, "%"),
                  xlab = "Intent Name")

# select top N in support value ----------

top_num_value = 30
itemFrequencyPlot(tr_intent, topN = top_num_value,
                  main = paste0("support top ", top_num_value, "items"))

# sampling N data ---------- 
random_sample = 500
image(sample(tr_intent, random_sample, replace = FALSE),
      main = paste0("matrix diagram with ", random_sample, " random samples"))


# Create Apriori Rule -----------
 
arule_all_intent <- apriori(data = tr_intent,
                            parameter = list(support = 0.10, 
                                             confidence = 0.20, 
                                             minlen = 2))
 
arule_all_intent
summary(arule_all_intent)
 
 inspect(sort(arule_all_intent)[1:20])
 inspect(sort(arule_all_intent, by = "support")[1:20])
 
 # EXPORT arule result into .csv files
  write(arule_all_intent,
        file = "output/arule_all_intent.csv",
        sep = ",", 
        quote = TRUE,
        row.names = FALSE)
 
# Select Intent in interest with Apriori Rule -----------
 
 rule_interest <- subset(arule_all_intent, 
                         items %in% c("StartOver", "StartOver_Welcome") 
                         & confidence > 0.25)
 inspect(rule_interest)
 
 
# Visualization Apriori Rule -----------
 
 # install.packages("arulesViz")
 # library(arulesViz)
 
 # plot arule ---------
   plot(arule_all_intent)
 
 
 # plot arule: sort by "support" ---------  
   plot(sort(arule_all_intent, by = "support")[1:30], method = "grouped")
   
 # plot arule: control graphic index ---------
   plot(arule_all_intent, method = "graph",
        control = list(type="itemsets"), 
        vertex.label.cex = 0.7, 
        edge.arrow.size = 0.3, 
        edge.arrow.width = 2)
   
## WO "StartIver..." Intent Analysis ===========================================
 df_intent2 <- testData %>% 
   select(date, USERID, INTENT, DATETIME) %>% 
   filter(!grepl("StartOver", testData$INTENT)) %>% 
   arrange(USERID, DATETIME) %>% 
   group_by(USERID, date) %>% 
   transmute(DATETIME = DATETIME,
             transID = paste0(USERID, "_", date),
             num_intent = n(INTENT),
             itemList = list(INTENT)) %>% 
   ungroup() %>% 
   unique() 
 
 
 intent_list2 <- df_intent2 %>% select(DATETIME, transID, itemList)
 
 tr_intent2 <- intent_list2$itemList
 b <- c(df_intent2$transID)
 
 names(tr_intent2) <- paste("tr", c(1:length(tr_intent2)), sep = "_")
 
 
 tr_intent2 <- as(tr_intent2, "transactions")
 tr_intent2
 summary(tr_intent2) 
 
 inspect(tr_intent2[1:10])
 itemFrequency(tr_intent2[,1:10])
 
 # control support value ----------
 support_value = 0.01
 itemFrequencyPlot(tr_intent2, 
                   support = support_value,
                   main = paste0("item frequency plot above support ", 
                                 support_value*100, "%"),
                   xlab = "Intent Name")
 
 # select top N in support value ----------
 
 top_num_value = 30
 itemFrequencyPlot(tr_intent2, topN = top_num_value,
                   main = paste0("support top ", top_num_value, "items"))
 
 # sampling N data ---------- 
 random_sample = 500
 image(sample(tr_intent2, random_sample, replace = FALSE),
       main = paste0("matrix diagram with ", random_sample, " random samples"))
 
 
 # Create Apriori Rule -----------
 
 arule_intent_wo_start <- apriori(data = tr_intent2,
                                  parameter = list(support = 0.10,
                                                   confidence = 0.20,
                                                   minlen = 2))
 
 arule_intent_wo_start
 summary(arule_intent_wo_start)
 
 inspect(sort(arule_intent_wo_start)[1:20])
 inspect(sort(arule_intent_wo_start, by = "support")[1:20])
 
 # EXPORT arule result into .csv files
 write(arule_intent_wo_start,
       file = "output/arule_intent_wo_start.csv",
       sep = ",", 
       quote = TRUE,
       row.names = FALSE)
 
 # Select Intent in interest with Apriori Rule -----------
 
 rule_interest2 <- subset(arule_intent_wo_start, 
                         items %in% c("StartOver", "StartOver_Welcome") 
                         & confidence > 0.25)
 inspect(rule_interest2)
 
 
 # Visualization Apriori Rule -----------
 
 # install.packages("arulesViz")
 # library(arulesViz)
 
 # plot arule ---------
 plot(arule_intent_wo_start)
 
 
 # plot arule: sort by "support" ---------  
 plot(sort(arule_intent_wo_start, by = "support")[1:30], method = "grouped")
 
 # plot arule: control graphic index ---------
 plot(arule_intent_wo_start, method = "graph",
      control = list(type="items"), 
      vertex.label.cex = 0.7, 
      edge.arrow.size = 0.3, 
      edge.arrow.width = 2)
 
 # plot arule: control graphic index ---------
 plot(arule_intent_wo_start, method = "graph",
      control = list(type="itemsets"), 
      vertex.label.cex = 0.7, 
      edge.arrow.size = 0.3, 
      edge.arrow.width = 2)
 