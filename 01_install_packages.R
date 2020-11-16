## install packages## R Packages ==================================================================
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, type = "binary", force = TRUE)
  sapply(pkg, require, character.only = TRUE)
}



pak_KoNLP <- c( "devtools", "RSQLite", "rJava", "remotes", "memoise",
                "multilinguer", "stringr", "hash", "tau", "Sejong",
                "installr")

ipak(pak_KoNLP)


pak <- c("broom", "data.table", "reshape", "reshape2", "plyr", "dplyr", "tidyverse", "purrr", 
         "tidyr", "tibble", "tidytext",
         "bindrcpp", "foreach", "doBy", "gdata", "lubridate","psych", "scales", "DescTools","caret", "NbClust", "tidyLPA", 
         "PerformanceAnalytics", "moonBook", "lattice", "latticeExtra", "bpa", 
         "MANOVA.RM","agricolae", "laercio", "mvnormtest", "mvoutlier", "outliers","chemometrics", "tseries",
         "ggplot2", "gridExtra", "corrplot","gplots", "ggpubr", "ggdendro", "GGally","latticeExtra",
         "extrafont", "showtext", "Cairo","ggiraph","ggiraphExtra", 
         "readxl", "writexl", "xlsx", "openxlsx", "rstan", "RcppEigen","rapportools", "wordcloud")

ipak(pak)


Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-15.0.1")
Sys.getenv("JAVA_HOME")



# INSTALL TEXT MINING PACKAGE: "KoNLP" =========================================

remotes::install_github("haven-jeon/KoNLP", 
                        upgrade = "never", 
                        INSTALL_opts=c("--no-multiarch"))

# INSTALL TEXT MINING PACKAGE: "KoSpacing" =====================================

install.packages("installr")
install.packages("Rtools")
install.packages("reticulate")

remotes::install_gitlab("mrchypark/multilinguer", force = TRUE)

reticulate::install_miniconda()

remotes::install_github("https://github.com/nathan-russell/hashmap")

remotes::install_github("haven-jeon/KoSpacing", force = TRUE)



library(KoSpacing)
packageVersion("KoSpacing")

set_env()


sessionInfo()

# INSTALL TEXT MINING PACKAGE: "RmecabKo" ======================================

install.packages("RmecabKo")
RmecabKo::install_mecab("c:/Rlib/mecab")

# INSTALL TEXT MINING PACKAGE: "N2H4" ==========================================

install.packages("N2H4")

# INSTALL TEXT MINING PACKAGE: "TensorFlow" & others ===========================

tensorflow::install_tensorflow() 

reticulate::py_config()

Sys.setenv(TENSORFLOW_PYTHON="C:/ProgramData/Anaconda3/python.exe")

devtools::install_github("rstudio/keras", force = TRUE)

library(keras)
keras::install_tensorflow()
install.packages("keras", type = "source")

library("tensorflow")
tensorflow::use_condaenv("r-tensorflow")


reticulate::create_conda_env(condaenv = "r-kospacing",
                             version = "~/anaconda3/bin/python",
                             verbose = TRUE)


# install.packages("httr")
# httr::content(threatfeedsj,"parsed")
# httr::content(series_data, as = "text")
# series_data <- c()
# series_data <- httr::content(series_data, as = "text", encoding = "UTF-8")


## Association Rule  ===========================================================
install.packages("arules") 