## Association Rule  ===========================================================

# install.packages("arules") 
library(arules)

getwd()
setwd("C:/Users/r_admin/Desktop/git_script/scripts") 

tran <- arules::read.transactions("input/practice_arule/tran.txt", 
                                  format = "basket", 
                                  sep=",")
tran 

inspect(tran) 

rule <- apriori(tran, parameter = list(supp=0.3, conf=0.1)) # 16 rule 
rule <- apriori(tran, parameter = list(supp=0.1, conf=0.1)) # 35 rule  
inspect(rule) 


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

df <- testData %>% select(message)
df_intent <- testData %>% 
  select(date, USERID, INTENT, DATETIME, session_ord) %>% 
  arrange(USERID, DATETIME, session_ord) %>% 
  group_by(USERID, date, session_ord) %>% 
  transmute(transactionID = paste0(date, "_", session_ord, "_",USERID),
            item = list(INTENT)) %>% 
  ungroup() %>% 
  select(transactionID, item) %>% 
  unique() 
  

str(df_intent)

tran <- arules::read.transactions(df_intent, 
                                  format = "basket")


df_arule <- df_intent %>% ungroup() %>% 
  select(list_daily_intent) %>% as.data.table()

df_arule$list_daily_intent[3]

lapply(df_arule, function(x) write.table( data.frame(x), 'test.txt'  , append= T, sep=',' ))
capture.output(summary(df_arule), file = "df_arule.txt")


write.table(df_arule, file="df_arule.txt")


tran <- arules::read.transactions(df_arule, 
                                  format = "basket", 
                                  sep=",")
tran 

inspect(tran) 

rule <- apriori(tran, parameter = list(supp=0.3, conf=0.1)) # 16 rule 
rule <- apriori(tran, parameter = list(supp=0.1, conf=0.1)) # 35 rule  
inspect(rule) 


