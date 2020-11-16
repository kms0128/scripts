## TEXT Preprocessing  =========================================================

rm(list=ls())


# System environments setting

options(scipen = 2) 
options(encoding = "UTF-8")


# "KoNLP" Setting

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-15.0.1")
Sys.getenv("JAVA_HOME")

library(rJava)
library(KoNLP)


#"KoSpacing" setting  ================================

library(KoSpacing)
set_env()


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

library(KoNLP)
SimplePos09("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.")

library(RmecabKo)
pos(iconv("롯데마트가 판매하고 있는 흑마늘 양념 치킨이 논란이 되고 있다.", to = "utf8"))


library(N2H4)

library(tidytext)


# crawling comments from the news article
tar <- "https://news.naver.com/main/read.nhn?mode=LSD&mid=shm&sid1=102&oid=001&aid=0012017374"

getAllComment(tar) %>% 
  select(userName, contents) %>% 
  # unnest_tokens(ws, contents, "words")
unnest_tokens(input = body,
              output = word)

str_detect(
  string = 글자 데이터, 
  pattern = 찾고자 하는 글자,  
  negate = FALSE # 조건에 맞는 경우 or 그 반대의 결과를 받을 것을 지정
)

str_replace_all(
  string = 글자 데이터,
  pattern = 찾고자 하는 글자,
  replacement = 찾은 글자가 바뀌게 될 글자
)
