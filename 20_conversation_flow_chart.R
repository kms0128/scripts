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


  # intent word cloud
    install.packages(c("tm", "SnowballC", 
                       "wordcloud", "RColorBrewer", 
                       "RCurl", "XML"))
    
    script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
    source(script)
    
    res<-rquery.wordcloud("JFKspeech.txt", 
                          type ="file", 
                          lang = "english")
    
    
    
    # sankey diagram with sankey package ----
    
    install.packages("networkD3", dependencies = TRUE)
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
    
    # prepare color scale: I give one specific color for each node.
    my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'
    
    # Make the Network. I call my colour scale with the colourScale argument
    p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                       Value = "value", NodeID = "name", colourScale=my_color)
    p
    
    # save the widget
    # library(htmlwidgets)
    # saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor1.html"))
    
    
    
    # alluvial diagram with ggplot2 -------------
    library(ggplot2)
    library(ggalluvial)
    
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


## Conversation flow chart Practice ============================================

install.packages("mvbutils", dependencies = TRUE)
library(mvbutils)

foodweb(where = environment())


install.packages("DependenciesGraph", dependencies = TRUE)
install.packages("QualtricsTools", dependencies = TRUE)

devtools::install_github("datastorm-open/DependenciesGraphs")
devtools::install_github("ctesta01/QualtricsTools")

library(DependenciesGraphs)
library(QualtricsTools) # A package I'm developing

deps <- funDependencies("package:QualtricsTools", "generate_split_coded_comments")
plot(deps)

library(mvbutils)
library(QualtricsTools) 
deps <- foodweb(where="package:QualtricsTools", 
                prune='make_split_coded_comments')
plot(deps)


# Prepare data
install.packages("ibr", dependencies = TRUE)
library("ibr")
dep <- envirDependencies("package:ibr")
plot(dep)

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

