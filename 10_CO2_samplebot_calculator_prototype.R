
rm(list=ls())

## IMPORT dataset ============================================================== 

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

  export_data <- testData 

## Preparing Saving Workbook     ================================================================
    
    wb <- createWorkbook("table")
    update <- Sys.Date()
    saveWorkbook(wb, paste0("samplebot",update,".xlsx"), overwrite = TRUE)
    

#### Looping the CO2 Calculator =================================================================

d07 <- c("2020-07-24 00:00:00")
d14 <- c("2020-07-17 00:00:00")
d30 <- c("2020-07-01 00:00:00")



for (i in 1:3){
  daterange = case_when(i == 1 ~ d07,
                        i == 2 ~ d14,
                        i == 3 ~ d30)
  
  df_name = case_when(i == 1 ~ "7days",
                      i == 2 ~ "14days",
                      i == 3 ~ "30days")

#### 1. DASHBOARD =================================================================
## [1] Visitor --------------------

dashboard_visitor <- export_data %>%
  filter(date >= as.Date(daterange)) %>% 
  select(date, USERID) %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(id = unique(USERID),
            is.visit = 1) %>% ungroup() %>% 
  group_by(id) %>% 
  spread(key = "date", value = is.visit, fill = 0) %>% 
  gather(matches("2020-"), key = "date", value = "is.visit") %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(cum.visit = cumsum(is.visit)) %>% ungroup() %>% 
  group_by(date) %>% 
  summarize(newvisitor = sum(is.visit == 1 & cum.visit == 1),
            revisitor = sum(is.visit == 1 & cum.visit > 1),
            totalvisit = newvisitor+revisitor) %>% 
  mutate(cumvisitors = cumsum(totalvisit)) 

print("dashboard_visitor: done!")

## [2] Session --------------------

dashboard_session <- export_data %>%
  filter(date >= as.Date(daterange)) %>% 
  group_by(date) %>% 
  mutate(laps_min = laps/60) %>% 
  summarize(num_session = sum(is.session),
            num_sessionT = sum(laps_min),
            mean_sessionT =mean(laps_min, na.rm = TRUE)) %>% 
  mutate(cum_session = cumsum(num_session),
         cumsum_sessionT = cumsum(num_sessionT),
         cummean_sessionT = cumsum(mean_sessionT)) %>% 
  ungroup() %>% 
  summarize(total_num = max(cum_session),
            daily_mean_num = mean(num_session, na.rm = TRUE),
            daily_mean_time = mean(mean_sessionT, na.rm = TRUE))

print("dashboard_session: done!")  


## [3] Intent --------------------

dashboard_intent10 <- export_data %>% 
  filter(date >= as.Date(daterange)) %>% 
  group_by(INTENT) %>% 
  # group_by(date) %>% 
  summarize(num_intent = n(INTENT)) %>% 
  # ungroup() %>% 
  # group_by(date) %>% 
  mutate(p_intent = round(100*(num_intent/sum(num_intent)),2),
         rank = dense_rank(desc(p_intent))) %>% 
  arrange(rank) %>% 
  filter(rank <= 10) %>% 
  select(INTENT, p_intent, rank)

print("dashboard_intent10: done!")  

## [4] Message Handling  --------------------

dashboard_message <- export_data %>% 
  ungroup() %>% 
  filter(date >= as.Date(daterange)) %>%  
  summarize(total = n(is.handled),
            handled = sum(is.handled == 1),
            Nothandled = sum(is.handled == 0)) %>% 
  gather(handled, Nothandled, key = "type", value = "n") %>% 
  mutate(p = round(100*(n/total))) %>% 
  select(-total)
  
print("dashboard_message: done!")  


## [5] Segment --------------------

dashboard_segment <- dashboard_visitor %>% ungroup() %>% 
  summarise(seg1 = sum(newvisitor),
            seg2 = sum(revisitor)) %>% 
  gather(seg1, seg2,  key = "type", value = "n") %>% 
  mutate(p =  round(100*(n/sum(n))),
         fluc = c(-5.72, 6.34) * (i/i+1),
         seg_name = c("newbee", "revisitor"))

print("dashboard_segment: done!")  



#### 2. CONVERSATION =================================================================
## [1] Conversation Flow --------------------
df_intent <- testData %>% 
  filter(date >= as.Date(daterange)) %>% 
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

testset <- testData %>% 
  filter(date >= as.Date(daterange)) %>% 
  select(date,USERID, INTENT, session_ord) %>% 
  group_by(date, USERID) %>% 
  mutate(rn = paste0("C_",row_number())) %>% 
  select(date, USERID, INTENT, rn)

testset2 <- testset %>%
  group_by(USERID) %>% 
  ungroup %>%
  pivot_wider(names_from = rn, values_from = INTENT) %>% ungroup()


conversation_flow <- merge(testset2,df_intent_list, by=c("date", "USERID"))

print("conversation_flow: done!")  


## [2] Intent Rank --------------------
conversation_intentRank <- export_data %>% 
  filter(date >= as.Date(daterange)) %>% 
  mutate(CONFIDENCE = replace(CONFIDENCE, is.na(CONFIDENCE), 0)) %>% 
  group_by(date, INTENT) %>% 
  summarize(n_call = n(INTENT),
            m_confidence = round(100*mean(CONFIDENCE, na.rm = TRUE),2),
            p_turnover = round(100*(sum(is.turnover == 1)/n(is.turnover)),2)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(p_call = round(100*(n_call/sum(n_call)),2),
         rank = dense_rank(desc(p_call))) %>% 
  arrange(date, rank)

print("conversation_intentRank: done!")


## [3] Entity Rank --------------------

    # create fake data set on excel for the poor numbers of Entity values from Samsung DB

#### 3. SEGMENT ======================================================================
# [0] Segment Table --------------------
daily_total <- sum(dashboard_visitor$totalvisit)
                  
segment_intentRank <- export_data %>% 
  ungroup() %>% 
  filter(date >= as.Date(daterange)) %>% 
  mutate(CONFIDENCE = replace(CONFIDENCE, is.na(CONFIDENCE), 0)) %>% 
  group_by(INTENT) %>% 
  summarize(n_user = n_distinct(USERID),
            n_call = n(INTENT),
            m_confidence = round(100*mean(CONFIDENCE, na.rm = TRUE),2),
            n_turnover = sum(is.turnover == 1)) %>% 
  mutate(p_user =  round(100*(n_user/daily_total),2),
         p_call = round(100*(n_call/sum(n_call)),2),
         p_turnover = round(100*(n_turnover/sum(n_turnover)),2),
         rank = dense_rank(desc(p_call))) %>% # ranked by the number of calls, not the number of users.
  arrange(rank)

# # check the result
# sum(segment_intentRank$n_user)
# sum(segment_intentRank$p_turnover)
# sum(segment_intentRank$n_call)
# sum(segment_intentRank$p_user) # over 100%, cuz there's duplicated userid values as one user can call several intents.
# sum(segment_intentRank$p_call)


# [1] summary  --------------------
segment_summary <- dashboard_visitor %>% 
  filter(date >= as.Date(daterange)) %>% 
  summarise(n_seg1 = sum(newvisitor),
            n_seg2 = sum(revisitor)) %>% 
  gather(n_seg1, n_seg2,  key = "type", value = "n") %>% 
  mutate(p =  round(100*(n/sum(n))),
         fluc = c(-5.72, 6.34) * (i/i+1),
         seg_name = c("newbee", "revisitor"))


segment_intentRank1 <- segment_intentRank %>% filter(rank == 1)
segment_summary[3,] <- data.frame(type = "n_seg3",
                                 n = segment_intentRank1$n_user,
                                 p = segment_intentRank1$p_user,
                                 fluc = c(15.89),
                                 seg_name = "1st_intent_call")

print("segment_summary: done!")  

#### 4. INDEX ========================================================================
## [1] Session  --------------------
df_session <- export_data %>% 
  filter(date >= as.Date(daterange)) %>% 
  select(date, USERID, DATETIME, matches("laps"), matches("session"), matches("turnover")) %>% 
  arrange(date, USERID,DATETIME)

cal_session <- df_session %>% 
  group_by(date, USERID) %>% 
  summarize(n_session = max(session_ord),
            n_sessionT = sum(laps/60)) %>% # second 2 minute transformation
  ungroup() %>% 
  group_by(date) %>% 
  summarize(num = sum(n_session),
            num_T = sum(n_sessionT),
            mean = mean(n_session, na.rm = TRUE),
            mean_T = mean(n_sessionT, na.rm=TRUE),
            max = max(n_session),
            min = min(n_session),
            median = median(n_session, na.rm = TRUE),
            max_T = max(n_sessionT),
            min_T = min(n_sessionT),
            median_T = median(n_sessionT, na.rm = TRUE),
            sd_session = sd(n_session, na.rm = TRUE),
            se_session = sd_session/sqrt(n(n_session)),
            sd_session_T = sd(n_sessionT, na.rm = TRUE),
            se_session_T = sd_session_T/sqrt(n(n_sessionT))) %>% 
  mutate( cumsum = cumsum(num),
          cumsum_T = cumsum(num_T))

index_session <- cal_session %>% 
  select(date, num, num_T, cumsum, cumsum_T)

print("index_session: done!")  

## [2] Input Type  --------------------

index_inputType <- export_data %>%
  filter(date >= as.Date(daterange)) %>% 
  group_by(date, TYPE) %>% 
  summarize(n = n(INTENT)) %>% spread(TYPE, n) %>% 
  mutate(p_button = round(100*(button/(button+question)),2),
         p_question = round(100*(question/(button+question)),2),
         fake_button_n1 = round(button * 0.33,2),
         fake_button_n2 = round(button * 0.51,2),
         fake_button_n3 = round(button * 0.16,2),
         fake_button_p1 = round(p_button * 0.33,2),
         fake_button_p2 = round(p_button * 0.51,2),
         fake_button_p3 = round(p_button * 0.16, 2) )
  
print("index_inputType: done!")  


#### 5. INSIGHT ======================================================================
## [1] Messages  --------------------
# message
insight_message <- export_data %>% 
  filter(date >= as.Date(daterange)) %>%  
  group_by(date) %>% 
  summarize(num_handled = sum(is.handled == 1),
            num_Nothandled = sum(is.handled == 0),
            mena_confidence =  round(100*(mean(CONFIDENCE, na.rm = TRUE)),2))

print("insight_message: done!")  

# message_tbl
insight_msg_tb <- export_data %>% 
  ungroup() %>% 
  group_by(date) %>% 
  filter(date >= as.Date(daterange) &
           !is.na(CONFIDENCE) &
           msg_length > 3 &
           msg_length <= mean(msg_length, na.rm = TRUE) + 1*sd(msg_length, na.rm = TRUE) &
           msg_length >= mean(msg_length, na.rm = TRUE) - 1*sd(msg_length, na.rm = TRUE)) %>%  
  arrange(DATETIME, message) %>% 
  summarise(DATETIME = DATETIME,
            confidence_rate = round(100*(CONFIDENCE),2),
            msg_handled = case_when(is.handled == 1 ~ "yes",
                                    is.handled == 0 ~ "no")) %>% 
  ungroup() %>% 
  select(date, DATETIME, confidence_rate, msg_handled)

sheet_list[15]

## result: openxlsx - chart and table ========================================== 
sheet_list <- 
  list("testData" = testData,
       "dashboard_visitor" = dashboard_visitor,
       "dashboard_session" = dashboard_session,
       "dashboard_intent10" = dashboard_intent10,
       "dashboard_message" = dashboard_message,
       "dashboard_segment" = dashboard_segment,
       "conversation_intentRank" = conversation_intentRank,
       "segment_intentRank" = segment_intentRank,
       "segment_summary" = segment_summary,
       "index_session" = index_session,
       "index_inputType" = index_inputType,
       "insight_message" = insight_message,
       "insight_msg_tb" = insight_msg_tb
       )
  
  
  write.xlsx(sheet_list,
             file = paste0("output/sampleBot_", df_name, "_", update, ".xlsx"),
             col.names = TRUE,
             append = TRUE)
  
  write.xlsx(conversation_flow,
             file = paste0("output/sampleBot_conversation_flow_", df_name, "_", update, ".xlsx"),
             col.names = TRUE,
             append = TRUE)

print(paste0("xlsx save Success! i : ", i))

}

