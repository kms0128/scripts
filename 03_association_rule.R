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

df <- testData %>% select(message)

df_intent <- testData %>% 
  select(date, USERID, INTENT, DATETIME) %>% 
  arrange(USERID, DATETIME) %>% 
  group_by(USERID, date) %>% 
  transmute(date = date,
            USERID = USERID,
            num_intent = n(INTENT),
            item = list(INTENT)) %>% 
  ungroup() %>% 
  unique() 

df_intent_list <- df_intent %>% select(date, USERID, item)
b <- c(df_intent$USERID)
names(df_intent_list) <- b


testset <- testData %>% select(date,USERID, INTENT, session_ord)

testset1 <- testset %>%
  group_by(date, USERID) %>% 
  mutate(rn = paste0("C",row_number())) %>% 
  select(date, USERID, INTENT, rn)

testset_1 <- testset1 %>%
  group_by(USERID) %>% 
  ungroup %>%
  pivot_wider(names_from = rn, values_from = INTENT) %>% ungroup()

length(df_intent$USERID)
length(testset_1$USERID)
length(df_intent_list$USERID)


conversation_flow <- merge(testset_1,df_intent_list, by=c("date", "USERID"))


cf_subset <- conversation_flow %>% select(starts_with("C"))
wb = createWorkbook()

for (k in colnames(cf_subset)){
  
  tbl = data.frame(table(cf_subset[k]))
  colnames(tbl)[1] <- c("INTENT")
  tbl <- tbl %>% 
    mutate(prop =  round(100*(Freq / sum(Freq)), 2))
  
  
  sheet_name = paste('node_', k)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, assign(paste0("node_", k), tbl))
  
  
}

saveWorkbook(wb, 'output/intent_by_node.xlsx')


## Association Rule  ===========================================================

# install.packages("arules") 
library(arules)

getwd()
setwd("C:/Users/r_admin/Desktop/git_script/scripts") 

cf_intent <- conversation_flow %>% select(date, starts_with("C"))

# practice code
tran <- arules::read.transactions("input/practice_arule/tran.txt", 
                                  format = "basket", 
                                  sep=",")
tran 

inspect(tran) 

rule <- apriori(tran, parameter = list(supp=0.3, conf=0.1)) # 16 rule 
rule <- apriori(tran, parameter = list(supp=0.1, conf=0.1)) # 35 rule  
inspect(rule) 

str(df_intent)




# My code

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


