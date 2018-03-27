library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plyr)
library(dplyr)
library(tidyr)
library(digest)
library(openxlsx)
library(mongolite)







num_fails_to_lockout<-10


header <- dashboardHeader(
  title = "区域管理模拟平台",
  tags$li(class = "dropdown",
          tags$li(class = "dropdown-toggle", actionLink(inputId = "save","保存")),
          tags$li(class = "dropdown-toggle", actionLink(inputId = "exit","退出")))
)

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))


body <- dashboardBody(uiOutput("body"))

ui <- dashboardPage(header, sidebar, body)