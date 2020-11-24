## CO2 Test

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


intent_set <- testData$INTENT
msg_set <- testData$message


df_intent <- testData %>% 
  select(date, USERID, INTENT, DATETIME) %>% 
  arrange(USERID, DATETIME) %>% 
  group_by(USERID, date) %>% 
  transmute(date = date,
            USERID = USERID,
            num_intent = n(INTENT),
            item = list(INTENT),
            itemList = as.character(item)) %>% 
  ungroup() %>% 
  unique() 


  testset <- testData %>% 
  select(date,USERID, INTENT, is.handled) %>% 
  group_by(date, USERID) %>% 
  mutate(rn = case_when(row_number() < 10 ~ paste0("C_00",row_number()),
                        row_number() < 100 ~ paste0("C_0",row_number()),
                        row_number() < 1000 ~ paste0("C_",row_number()))) %>% 
  select(date, USERID, INTENT, rn, is.handled)
  
  test_freq <- testset %>% 
    group_by(is.handled, INTENT, rn) %>% 
    summarise(freq = n(INTENT))
  

  ggplot(testset2,
         aes(y = Freq,
             axis1 = Class, axis2 = Sex, axis3 = Age,
             fill = is.handled)) +
    geom_alluvium() +
    scale_x_discrete(limits = rn)
  
  
  # alluvial diagram with ggplot2 -------------
  
  testDF <- as.data.frame(Titanic)
  ggplot(testDF,
         aes(y = Freq,
             axis1 = Class, axis2 = Sex, axis3 = Age,
             fill = is.handled)) +
    geom_alluvium() +
    scale_x_discrete(limits = rn)
  
  library(ggplot2)
  library(ggalluvial)
  
  a <- titanic_table
  
  ggplot(titanic_table,
         aes(axis1 = Class,
             axis2 = Survived,
             y = n)) +
    geom_alluvium(aes(fill = Sex)) +
    geom_stratum() +
    geom_text(stat = "stratum", 
              label.strata = TRUE) +
    scale_x_discrete(limits = c("Class", "Survived"),
                     expand = c(.1, .1)) +
    labs(title = "Titanic data",
         subtitle = "stratified by class, sex, and survival",
         y = "Frequency") +
    theme_minimal()
  
  
  # NOT RUN {
  # basic
  testDF <- as.data.frame(Titanic)
  ggplot(as.data.frame(Titanic),
         aes(y = Freq,
             axis1 = Class, axis2 = Sex, axis3 = Age,
             fill = Survived)) +
    geom_alluvium() +
    scale_x_discrete(limits = c("Class", "Sex", "Age"))
  
## spread list -----------------------
  
  FreqTbl <- spread(data = test_freq,
                   value = freq,
                   fill = NA,
                   key = rn)
  
  FreqTbl_tidy <- FreqTbl %>% 
    group_by(is.handled, INTENT) %>% 
    gather( key = "order", value = "freq")
  
  colset <- colnames(FreqTbl)[-c(1:2)]
  
  FreqTbl_tidy <- FreqTbl %>% 
    group_by(is.handled, INTENT) %>% 
    pivot_longer(cols = colset,
                 names_to = "order",
                 values_to = "freq",
                 values_drop_na = FALSE) %>% 
    arrange(is.handled, INTENT, order)
  
  FreqTbl_tidy[is.na(FreqTbl_tidy)] <- 0
  
  
  IntentTbl <- spread(data = testset,
                       value = INTENT,
                       fill = NA,
                       key = rn)
  
  write.xlsx(list(FreqTbl, IntentTbl),
             file = paste0("output/sampleBot_conversation_flow_Tbl_", update, ".xlsx"),
             col.names = TRUE,
             append = TRUE)
  
  write.xlsx(FreqTbl_tidy,
             file = paste0("output/sampleBot_conversation_flow_Tbl_", update, ".xlsx"),
             col.names = TRUE,
             append = TRUE)
  


  # testset2 <- testset %>%
  #   group_by(USERID) %>% 
  #   ungroup %>%
  #   pivot_wider(names_from = rn, values_from = INTENT) %>% ungroup()
  


alluvial_df <- merge(df_intent, testset2, by = c("USERID", "date"))
 
alluvial_df1 <- alluvial_df %>% 
  select(-date, -USERID, -num_intent, -item) %>% 
  group_by(item) %>% summarise(Freq = n_distinct(item))


  # intent word cloud ----

    
    # sankey diagram with sankey package ----

    library(networkD3)
    
    # Make a connection data frame
    links <- data.frame(
      source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
      target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
      value=c(2,3, 2, 3, 1, 3)
    )
    
    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(
      name=c(as.character(links$source), as.character(links$target)) %>% 
        unique()
    )
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(links$source, nodes$name)-1 
    links$IDtarget <- match(links$target, nodes$name)-1
    
    # Add a 'group' column to the nodes data frame:
    nodes$group <- as.factor(c("a","a","a","a","a","b","b","b"))
    
    # Give a color for each group:
    my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'
    
    
    # Make the Network
    p <- sankeyNetwork(Links = links, 
                       Nodes = nodes, 
                       Source = "IDsource", 
                       Target = "IDtarget", 
                       Value = "value", 
                       NodeID = "name", 
                       colourScale = my_color, 
                       LinkGroup="group",
                       NodeGroup="group")
    p
    
   
    # Node: intent types
    # source 는 시작하는 그룹, 
    # target 은 바뀐 그룹을 의미하고, 
    # value 는 몇 명이나 이동했는지를 의미합니다.
    
    nodes_co2 <- testData %>% 
      select(INTENT) %>% 
      summarise(name = unique(INTENT))
    
    
    
    
    
    

## Conversation flow chart Practice ============================================

  
    

# Prepare data
dep <- funDependencies("package:ibr","iterchoiceS1")

# visualization
plot(dep)


## test

plot.new()
par(mar=c(0,0,0,0)+.1)
plot.window(xlim=c(0,3), ylim=c(0,8))
xspline( c(1,1.25,1.75,2), c(7,7,4,4), s=1, lwd=32.8/4.5, border="#0000ff88", lend=1)
xspline( c(1,1.25,1.75,2), c(6,6,4,4), s=1, lwd=19.7/4.5, border="#0000ff88", lend=1 )
xspline( c(1,1.25,1.75,2), c(5,5,4,4), s=1, lwd=16.5/4.5, border="#0000ff88", lend=1 )
xspline( c(1,1.25,1.75,2), c(4,4,4,4), s=1, lwd=13.8/4.5, border="#0000ff88", lend=1 )
xspline( c(1,1.25,1.75,2), c(3,3,4,4), s=1, lwd= 7.9/4.5, border="#0000ff88", lend=1 )
xspline( c(1,1.25,1.75,2), c(2,2,4,4), s=1, lwd= 4.8/4.5, border="#0000ff88", lend=1 )
xspline( c(1,1.25,1.75,2), c(1,1,4,4), s=1, lwd= 4.5/4.5, border="#0000ff88", lend=1 )

text( rep(0.75, 7), 7:1, LETTERS[1:7] )
text( 2.25, 4, 'Tie strength')



## ggalluvial
install.packages("ggalluvial", dependencies = TRUE)
library(alluvial)
library(ggalluvial)

df <- as.data.frame(UCBAdmissions)
head(as.data.frame(UCBAdmissions), n = 12)
is_alluvia_form(as.data.frame(UCBAdmissions), axes = 1:3, silent = TRUE)

ggplot(as.data.frame(UCBAdmissions),
       aes(y = Freq, axis1 = Gender, axis2 = Dept)) +
  geom_alluvium(aes(fill = Admit), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")

