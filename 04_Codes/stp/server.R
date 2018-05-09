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


mainbody <- div(
  
  
  tabItems(
    # tabItem(tabName = "introduction",
    #         br(),
    #         box(
    #           title = "介绍",
    #           solidHeader = TRUE, status = "primary"
    #           
    #         )),
    # First tab content
    tabItem(tabName = "news_and_WAS",
            tabsetPanel(
              
              tabPanel("新闻",
                       br(),
                       fluidRow(
                         column(width = 2,
                             title = "周期列表",
                             solidHeader = TRUE, status = "primary",
                             
                             actionButton(inputId="phase1_WAS_info", label="周期1:  新闻快报",
                                          style="width:140px"),
                             br(),br(),
                             disabled(
                               actionButton(inputId="phase2_WAS_info", label="周期2:  新闻快报",
                                            style="width:140px"))),
                         
                         div(id="phase1_WAS_info_box",
                             box(width = 10,
                                 title = "新闻快报",
                                 solidHeader = TRUE, status = "primary",
                                 dataTableOutput("p1_was_plan"))),
                         hidden(
                           div(id="phase2_WAS_info_box",
                               box(width = 10,
                                   title = "新闻快报",
                                   solidHeader = TRUE, status = "primary",
                                   dataTableOutput("p2_was_plan")
                                   )
                           ))
                         )
              ),
              tabPanel("客户信息",
                       br(),
                       fluidRow(
                         
                         column(width = 2,
                             title = "周期列表",
                             solidHeader = TRUE, status = "primary",
                             
                             actionButton(inputId="phase1_hospital_info", label="周期1:  客户信息",
                                          style="width:140px"),
                             br(),br(),
                             disabled(actionButton(inputId="phase2_hospital_info", label="周期2:  客户信息",
                                                   style="width:140px"))),
                         
                         div(id="phase1_hospital_info_box",
                             box(width = 10,
                                 title = "客户潜力信息",
                                 solidHeader = TRUE, status = "primary",
                                 dataTableOutput("p1_hospital_info"))),
                         hidden(
                           div(id="phase2_hospital_info_box",
                               box(width = 10,
                                   title = "客户潜力信息",
                                   solidHeader = TRUE, status = "primary",
                                   dataTableOutput("p2_hospital_info")
                               )
                           ))
                       ))
            )),
    
    # First tab content
    tabItem(tabName = "sr",
            fluidRow(
              box(title = "业务代表介绍",
                  solidHeader = TRUE, status = "primary",
                  width = 12,
                  h4(strong("团队能力指数：59")),
                  dataTableOutput("sales_rep_info")))
    ),
    
    # First tab content
    tabItem(tabName = "products",
            fluidRow(
              box(title = "销售产品介绍",
                  solidHeader = TRUE, status = "primary",
                  width = 12,
                  dataTableOutput("products_info")))
    ),
    
    # First tab content
    tabItem(
      tabName = "decision1",
      
      tabsetPanel(
        id = "tab1",
        
        
        
        
        tabPanel("周期1",
                 value="phase1",
                 # actionButton('save_inputs', '保存输入'),
                 # actionButton("load_inputs", "加载输入"),
                 # selectInput(inputId = "select_file",label = "filename",choices = list.files(pattern="\\.RDS$"),selected=NULL),
                 br(),
                 
                 #fluidRow(
                 # h3("总推广预算(元)"),
                 box(title = "总推广预算",
                     status = "primary",
                     solidHeader = TRUE,
                     width="100%",
                     
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                "经理指标(元)"),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_flm_sales_target"))
                     ),
                     
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                "总推广预算(元)"),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_total_promotional_budget"))
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                "已分配的推广预算(%)"),
                       tags$div(
                         style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                         verbatimTextOutput("p1_arranged_promotional_budget")
                       )
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                paste("已分配时间 ",sr_info$业务代表[1],"(%)",sep="")),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_arranged_time_of_sr1"))
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                paste("已分配时间 ",sr_info$业务代表[2],"(%)",sep="")),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_arranged_time_of_sr2"))
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                paste("已分配时间 ",sr_info$业务代表[3],"(%)",sep="")),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_arranged_time_of_sr3"))
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                paste("已分配时间 ",sr_info$业务代表[4],"(%)",sep="")),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_arranged_time_of_sr4"))
                     ),
                     tags$div(
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                paste("已分配时间 ",sr_info$业务代表[5],"(%)",sep="")),
                       tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p1_arranged_time_of_sr5"))
                     )
                 ),
                 
                 br(),
                 box(
                   title=paste("1.",unique(hospital_info$名称)[1]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   
                   #fluidRow(  
                   
                   #h3("决策-医院1"),
                   #h3(hospital_info$hospital[1]),
                   #htmlOutput(),
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp1_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp1_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp1_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp1_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp1", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp1_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp1_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp1_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp1_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp1_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp1_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp1_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp1_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp1",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp1_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp1_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp1_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp1_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院2"),
                 box(
                   title=paste("2.",unique(hospital_info$名称)[2]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp2_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp2_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp2_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp2_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp2", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp2_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp2_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp2_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp2_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp2_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp2_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp2_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp2_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp2",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp2_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp2_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp2_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp2_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院3"),
                 box(
                   title=paste("3.",unique(hospital_info$名称)[3]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp3_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp3_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp3_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp3_4"))
                   ),
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp3", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp3_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp3_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp3_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp3_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp3_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp3_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp3_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp3_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp3",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp3_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp3_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp3_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp3_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                   
                   
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院4"),
                 box(
                   title=paste("4.",unique(hospital_info$名称)[4]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp4_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp4_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp4_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp4_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp4", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp4_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp4_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp4_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp4_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp4_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp4_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp4_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp4_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp4",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp4_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp4_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp4_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp4_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院5"),
                 box(
                   title=paste("5.",unique(hospital_info$名称)[5]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp5_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp5_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp5_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp5_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp5", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp5_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp5_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp5_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp5_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp5_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp5_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp5_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp5_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp5",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp5_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp5_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp5_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp5_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 
                 # fluidRow(
                 #   h3("决策-医院6"),
                 box(
                   title=paste("6.",unique(hospital_info$名称)[6]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp6_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp6_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp6_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp6_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp6", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp6_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp6_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp6_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp6_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp6_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp6_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp6_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp6_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp6",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp6_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp6_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp6_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp6_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院7"),
                 box(
                   title=paste("7.",unique(hospital_info$名称)[7]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp7_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp7_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp7_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp7_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp7", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp7_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp7_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp7_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp7_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp7_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp7_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp7_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp7_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp7",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp7_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp7_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp7_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp7_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 
                 # fluidRow(
                 #   h3("决策-医院8"),
                 box(
                   title=paste("8.",unique(hospital_info$名称)[8]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp8_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp8_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp8_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp8_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp8", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp8_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp8_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp8_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp8_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp8_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp8_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp8_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp8_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp8",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp8_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp8_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp8_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp8_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 # fluidRow(
                 #   h3("决策-医院9"),
                 box(
                   title=paste("9.",unique(hospital_info$名称)[9]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp9_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp9_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp9_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp9_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp9", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp9_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp9_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp9_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp9_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp9_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp9_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp9_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp9_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp9",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp9_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp9_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp9_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp9_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 ),
                 br(),
                 
                 # fluidRow(
                 #   h3("决策-医院10"),
                 box(
                   title=paste("10.",unique(hospital_info$名称)[10]),
                   status = "primary",
                   solidHeader = TRUE,
                   width="100%",
                   tags$div(
                     tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                              product_info$类别[1]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[2]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[3]),
                     tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                              product_info$类别[4])
                   ),
                   br(),
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "潜力(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp10_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp10_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp10_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_potential_sales_hosp10_4"))
                   ),
                   
                   
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的总推广预算(%)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_promotional_budget_hosp10", 
                                        label = NULL,
                                        value=""))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "上期销售额(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp10_1")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp10_2")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp10_3")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                              verbatimTextOutput("p1_current_sales_hosp10_4"))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "销售指标设定(元)"),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp10_sales_target_1",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp10_sales_target_2",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput("p1_hosp10_sales_target_3",label = NULL, value="")),
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput("p1_hosp10_sales_target_4",label = NULL, value="")))
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "分派业务代表"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       selectizeInput(
                         "p1_sr_hosp10",
                         label = NULL,
                         choices = sr_info$业务代表,
                         selected = NULL
                       )
                     )
                   ),
                   
                   tags$div(
                     tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                              "批准的医院拜访时间(%)"),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp10_worktime_1",
                         label = NULL,
                         value = ""
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp10_worktime_2",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       textInput(
                         "p1_hosp10_worktime_3",
                         label = NULL,
                         value = ""
                         
                       )
                     ),
                     tags$div(
                       style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                       disabled(textInput(
                         "p1_hosp10_worktime_4",
                         label = NULL,
                         value = ""
                         
                       )
                       ))
                   )
                 )
        ),
        
        
        
        tabPanel(
          "周期2",
          value="phase2",
          
          # fluidRow(
          #   h3("总推广预算(元)"),
          br(),
          hidden(
            div(id="decision1_phase2",
                box(
                  title="总推广预算",
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "经理指标(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_flm_sales_target"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "总推广预算(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_total_promotional_budget"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "已分配的推广预算(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                      verbatimTextOutput("p2_arranged_promotional_budget")
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             paste("已分配时间 ",sr_info$业务代表[1],"(%)",sep="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_arranged_time_of_sr1"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             paste("已分配时间 ",sr_info$业务代表[2],"(%)",sep="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_arranged_time_of_sr2"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             paste("已分配时间 ",sr_info$业务代表[3],"(%)",sep="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_arranged_time_of_sr3"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             paste("已分配时间 ",sr_info$业务代表[4],"(%)",sep="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_arranged_time_of_sr4"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             paste("已分配时间 ",sr_info$业务代表[5],"(%)",sep="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                             verbatimTextOutput("p2_arranged_time_of_sr5"))
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院1"),
                box(
                  title=paste("1.",unique(hospital_info$名称)[1]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp1_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp1_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp1_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp1_4"))
                  ),
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp1", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp1_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp1_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp1_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp1_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp1_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp1_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp1_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp1_sales_target_4",label = NULL, value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp1",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp1_worktime_1",
                        label = NULL,
                        value = "6"
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp1_worktime_2",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp1_worktime_3",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp1_worktime_4",
                        label = NULL,
                        value = "6"
                        
                      )
                    )
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院2"),
                box(
                  title=paste("2.",unique(hospital_info$名称)[2]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp2_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp2_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp2_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp2_4"))
                  ),
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp2", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp2_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp2_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp2_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp2_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp2_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp2_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp2_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp2_sales_target_4",label = NULL, value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp2",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp2_worktime_1",
                        label = NULL,
                        value = "6"
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp2_worktime_2",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp2_worktime_3",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp2_worktime_4",
                        label = NULL,
                        value = "6"
                        
                      )
                    )
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院3"),
                box(
                  title=paste("3.",unique(hospital_info$名称)[3]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp3_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp3_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp3_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp3_4"))
                  ),
                  
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp3", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp3_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp3_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp3_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp3_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp3_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp3_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp3_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp3_sales_target_4",label = NULL, value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp3",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp3_worktime_1",
                        label = NULL,
                        value = "6"
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp3_worktime_2",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp3_worktime_3",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp3_worktime_4",
                        label = NULL,
                        value = "6"
                        
                      )
                    )
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院4"),
                box(
                  title=paste("4.",unique(hospital_info$名称)[4]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp4_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp4_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp4_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp4_4"))
                  ),
                  
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp4", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp4_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp4_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp4_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp4_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp4_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp4_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp4_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp4_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp4",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp4_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp4_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp4_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp4_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院5"),
                box(
                  title=paste("5.",unique(hospital_info$名称)[5]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp5_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp5_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp5_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp5_4"))
                  ),
                  
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp5", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp5_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp5_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp5_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp5_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp5_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp5_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp5_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp5_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp5",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp5_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp5_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp5_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp5_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                ),
                br(),
                
                # fluidRow(
                #   h3("决策-医院6"),
                box(
                  title=paste("6.",unique(hospital_info$名称)[6]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp6_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp6_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp6_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp6_4"))
                  ),
                  
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp6", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp6_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp6_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp6_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp6_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp6_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp6_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp6_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp6_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp6",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp6_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp6_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp6_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp6_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院7"),
                box(
                  title=paste("7.",unique(hospital_info$名称)[7]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp7_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp7_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp7_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp7_4"))
                  ),
                  
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp7", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp7_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp7_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp7_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp7_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp7_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp7_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp7_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp7_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp7",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp7_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp7_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp7_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp7_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                ),
                br(),
                
                # fluidRow(
                #   h3("决策-医院8"),
                box(
                  title=paste("8.",unique(hospital_info$名称)[8]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp8_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp8_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp8_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp8_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp8", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp8_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp8_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp8_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp8_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp8_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp8_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp8_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp8_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp8",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp8_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp8_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp8_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp8_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                ),
                br(),
                # fluidRow(
                #   h3("决策-医院9"),
                box(
                  title=paste("9.",unique(hospital_info$名称)[9]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp9_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp9_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp9_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp9_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp9", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp9_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp9_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp9_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp9_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp9_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp9_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp9_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp9_sales_target_4",label = NULL, value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp9",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp9_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp9_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp9_worktime_3",
                        label = NULL,
                        value = "6"
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp9_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                    )
                  )
                ),
                br(),
                
                # fluidRow(
                #   h3("决策-医院10"),
                box(
                  title=paste("10.",unique(hospital_info$名称)[10]),
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                             product_info$类别[1]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[2]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[3]),
                    tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                             product_info$类别[4])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "潜力(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp10_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp10_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp10_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_potential_sales_hosp10_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的总推广预算(%)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_promotional_budget_hosp10", 
                                       label = NULL,
                                       value=""))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "上期销售额(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp10_1")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp10_2")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp10_3")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                             verbatimTextOutput("p2_current_sales_hosp10_4"))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "销售指标设定(元)"),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp10_sales_target_1",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp10_sales_target_2",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             textInput("p2_hosp10_sales_target_3",label = NULL, value="")),
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                             disabled(textInput("p2_hosp10_sales_target_4",label = NULL, value="")))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "分派业务代表"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      selectizeInput(
                        "p2_sr_hosp10",
                        label = NULL,
                        choices = sr_info$业务代表,
                        selected = NULL
                      )
                    )
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                             "批准的医院拜访时间(%)"),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp10_worktime_1",
                        label = NULL,
                        value = ""
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp10_worktime_2",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      textInput(
                        "p2_hosp10_worktime_3",
                        label = NULL,
                        value = ""
                        
                      )
                    ),
                    tags$div(
                      style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                      disabled(textInput(
                        "p2_hosp10_worktime_4",
                        label = NULL,
                        value = ""
                        
                      )
                      ))
                  )
                )
            )
          )),
        hidden(
          tabPanel(title="周期0",
                        value="phase0",
                        hidden(checkboxInput("hide1", "Show tab2", FALSE)),
                        br(),
                        #selectInput(inputId = "hide1",chices="yes",selected="yes"),
                        
                        #fluidRow(
                        # h3("总推广预算(元)"),
                        box(title = "总推广预算(元)",
                            status = "primary",
                            solidHeader = TRUE,
                            width="100%",
                            
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       "经理指标(元)"),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_flm_sales_target"))
                            ),
                            
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       "总推广预算(元)"),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_total_promotional_budget"))
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       "已分配的推广预算(%)"),
                              tags$div(
                                style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                verbatimTextOutput("p0_arranged_promotional_budget")
                              )
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       paste("已分配时间 ",sr_info$业务代表[1],"(%)",sep="")),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_arranged_time_of_sr1"))
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       paste("已分配时间 ",sr_info$业务代表[2],"(%)",sep="")),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_arranged_time_of_sr2"))
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       paste("已分配时间 ",sr_info$业务代表[3],"(%)",sep="")),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_arranged_time_of_sr3"))
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       paste("已分配时间 ",sr_info$业务代表[4],"(%)",sep="")),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_arranged_time_of_sr4"))
                            ),
                            tags$div(
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                       paste("已分配时间 ",sr_info$业务代表[5],"(%)",sep="")),
                              tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left",
                                       verbatimTextOutput("p0_arranged_time_of_sr5"))
                            )
                        ),
                        
                        br(),
                        box(
                          title=unique(hospital_info$名称)[1],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          
                          #fluidRow(  
                          
                          #h3("决策-医院1"),
                          #h3(hospital_info$hospital[1]),
                          #htmlOutput(),
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp1_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp1_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp1_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp1_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp1", 
                                               label = NULL,
                                               value="22"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp1_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp1_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp1_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp1_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp1_sales_target_1",label = NULL, value="1500000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp1_sales_target_2",label = NULL, value="200000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp1_sales_target_3",label = NULL, value="270000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     disabled(textInput("p0_hosp1_sales_target_4",label = NULL, value="")))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp1",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小宋"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp1_worktime_1",
                                label = NULL,
                                value = "50"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp1_worktime_2",
                                label = NULL,
                                value = "15"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp1_worktime_3",
                                label = NULL,
                                value = "17"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput(
                                "p0_hosp1_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                              ))
                          )
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院2"),
                        box(
                          title=unique(hospital_info$名称)[2],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp2_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp2_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp2_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp2_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp2", 
                                               label = NULL,
                                               value="7"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp2_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp2_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp2_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp2_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp2_sales_target_1",label = NULL, value="470000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp2_sales_target_2",label = NULL, value="205000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp2_sales_target_3",label = NULL, value="83000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     disabled(textInput("p0_hosp2_sales_target_4",label = NULL, value="")))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp2",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小白"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp2_worktime_1",
                                label = NULL,
                                value = "25"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp2_worktime_2",
                                label = NULL,
                                value = "15"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp2_worktime_3",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput(
                                "p0_hosp2_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                              ))
                          )
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院3"),
                        box(
                          title=unique(hospital_info$名称)[3],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp3_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp3_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp3_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp3_4"))
                          ),
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp3", 
                                               label = NULL,
                                               value="14"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp3_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp3_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp3_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp3_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp3_sales_target_1",label = NULL, value="895000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp3_sales_target_2",label = NULL, value="148000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp3_sales_target_3",label = NULL, value="36800")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     disabled(textInput("p0_hosp3_sales_target_4",label = NULL, value="")))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp3",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = '小青'
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp3_worktime_1",
                                label = NULL,
                                value = "48"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp3_worktime_2",
                                label = NULL,
                                value = "15"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp3_worktime_3",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput(
                                "p0_hosp3_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                              ))
                          )
                          
                          
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院4"),
                        box(
                          title=unique(hospital_info$名称)[4],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp4_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp4_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp4_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp4_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp4", 
                                               label = NULL,
                                               value="11"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp4_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp4_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp4_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp4_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp4_sales_target_1",label = NULL, value="940000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp4_sales_target_2",label = NULL, value="180000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp4_sales_target_3",label = NULL, value="90000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     disabled(textInput("p0_hosp4_sales_target_4",label = NULL, value="")))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp4",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小白"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp4_worktime_1",
                                label = NULL,
                                value = "40"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp4_worktime_2",
                                label = NULL,
                                value = "10"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp4_worktime_3",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              disabled(textInput(
                                "p0_hosp4_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                              ))
                          )
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院5"),
                        box(
                          title=unique(hospital_info$名称)[5],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp5_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp5_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp5_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp5_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp5", 
                                               label = NULL,
                                               value="4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp5_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp5_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp5_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp5_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp5_sales_target_1",label = NULL, value="265000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp5_sales_target_2",label = NULL, value="105000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp5_sales_target_3",label = NULL, value="33000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     disabled(textInput("p0_hosp5_sales_target_4",label = NULL, value="")))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp5",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小青"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp5_worktime_1",
                                label = NULL,
                                value = "16"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp5_worktime_2",
                                label = NULL,
                                value = "10"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp5_worktime_3",
                                label = NULL,
                                value = "6"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp5_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                        ),
                        br(),
                        
                        # fluidRow(
                        #   h3("决策-医院6"),
                        box(
                          title=unique(hospital_info$名称)[6],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp6_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp6_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp6_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp6_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp6", 
                                               label = NULL,
                                               value="7"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp6_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp6_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp6_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp6_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp6_sales_target_1",label = NULL, value="650000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp6_sales_target_2",label = NULL, value="170000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp6_sales_target_3",label = NULL, value="47000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp6_sales_target_4",label = NULL, value=""))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp6",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小木"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp6_worktime_1",
                                label = NULL,
                                value = "35"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp6_worktime_2",
                                label = NULL,
                                value = "15"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp6_worktime_3",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp6_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院7"),
                        box(
                          title=unique(hospital_info$名称)[7],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp7_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp7_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp7_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp7_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp7", 
                                               label = NULL,
                                               value="7"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp7_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp7_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp7_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp7_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp7_sales_target_1",label = NULL, value="500000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp7_sales_target_2",label = NULL, value="160000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp7_sales_target_3",label = NULL, value="30000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp7_sales_target_4",label = NULL, value=""))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp7",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小木"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp7_worktime_1",
                                label = NULL,
                                value = "30"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp7_worktime_2",
                                label = NULL,
                                value = "10"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp7_worktime_3",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp7_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                        ),
                        br(),
                        
                        # fluidRow(
                        #   h3("决策-医院8"),
                        box(
                          title=unique(hospital_info$名称)[8],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp8_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp8_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp8_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp8_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp8", 
                                               label = NULL,
                                               value="2"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp8_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp8_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp8_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp8_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp8_sales_target_1",label = NULL, value="170000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp8_sales_target_2",label = NULL, value="60000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp8_sales_target_3",label = NULL, value="8000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp8_sales_target_4",label = NULL, value=""))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp8",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小宋"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp8_worktime_1",
                                label = NULL,
                                value = "10"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp8_worktime_2",
                                label = NULL,
                                value = "5"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp8_worktime_3",
                                label = NULL,
                                value = "3"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp8_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                        ),
                        br(),
                        # fluidRow(
                        #   h3("决策-医院9"),
                        box(
                          title=unique(hospital_info$名称)[9],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp9_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp9_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp9_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp9_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp9", 
                                               label = NULL,
                                               value="2"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp9_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp9_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp9_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp9_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp9_sales_target_1",label = NULL, value="170000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp9_sales_target_2",label = NULL, value="70000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp9_sales_target_3",label = NULL, value="25000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp9_sales_target_4",label = NULL, value=""))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp9",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小兰"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp9_worktime_1",
                                label = NULL,
                                value = "10"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp9_worktime_2",
                                label = NULL,
                                value = "7"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp9_worktime_3",
                                label = NULL,
                                value = "3"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp9_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                        ),
                        br(),
                        
                        # fluidRow(
                        #   h3("决策-医院10"),
                        box(
                          title=unique(hospital_info$名称)[10],
                          status = "primary",
                          solidHeader = TRUE,
                          width="100%",
                          tags$div(
                            tags$div(style = "display:inline-block;vertical-align:middle;margin-left:18%;width:18%;text-align:center",
                                     product_info$类别[1]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[2]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[3]),
                            tags$div(style = "display:inline-block;width:18%;text-align:center;vertical-align:middle;margin-left:10px",
                                     product_info$类别[4])
                          ),
                          br(),
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "潜力(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp10_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp10_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp10_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_potential_sales_hosp10_4"))
                          ),
                          
                          
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的总推广预算(%)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_promotional_budget_hosp10", 
                                               label = NULL,
                                               value="23"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "上期销售额(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp10_1")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp10_2")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp10_3")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;margin-right:10px;",
                                     verbatimTextOutput("p0_current_sales_hosp10_4"))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "销售指标设定(元)"),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp10_sales_target_1",label = NULL, value="2000000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp10_sales_target_2",label = NULL, value="200000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp10_sales_target_3",label = NULL, value="225000")),
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                                     textInput("p0_hosp10_sales_target_4",label = NULL, value=""))
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "分派业务代表"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              selectizeInput(
                                "p0_sr_hosp10",
                                label = NULL,
                                choices = sr_info$业务代表,
                                selected = "小兰"
                              )
                            )
                          ),
                          
                          tags$div(
                            tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:left;",
                                     "批准的医院拜访时间(%)"),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp10_worktime_1",
                                label = NULL,
                                value = "60"
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp10_worktime_2",
                                label = NULL,
                                value = "10"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp10_worktime_3",
                                label = NULL,
                                value = "10"
                                
                              )
                            ),
                            tags$div(
                              style = "display:inline-block;width:16%;vertical-align:middle;margin-left:20px;text-align:center;margin-right:10px;",
                              textInput(
                                "p0_hosp10_worktime_4",
                                label = NULL,
                                value = ""
                                
                              )
                            )
                          )
                       )
        ))
      )),
    tabItem(
      tabName = "decision2",
      
      tabsetPanel(
        
        id = "tab2",
        
        
        tabPanel(
          "周期1",
          br(),
          
          # fluidRow(
          #   column(
          #      width=2,
          #           br(),
          #           br(),
          #           br(),
          #           br(),
          fluidRow(
            column(width=10),
            column(width=2,
                   actionButton("decision2_phase1_submit", strong(" 提  交"),icon("check-square-o"),width=150))),
          # div(id="decision2_phase1_submit",class = "ui blue button", 
          #     "Color icon button"),
          div(
            id="admin",
            selectInput(inputId="select_file",label="选择文件",choices=list.files(pattern = "\\.RDS$"),selected=NULL),
            actionButton("load_inputs", "加载输入")),
          downloadButton("p0_chk_data","下载周期0中间数据"),
          downloadButton("p1_chk_data","下载周期1中间数据"),
          
          #   column(width=5,textInput(inputId = "filename",label = "保存文件名"),
          #          actionButton('save_inputs', '保存输入')),
          #   column(width=5,selectInput(inputId="select_file",label="选择文件",choices=list.files(pattern = "\\.RDS$"),selected=NULL),
          #          actionButton("load_inputs", "加载输入"))),
          br(),
          #fluidRow(h3("时间分配"),
          box(
            title="时间分配",
            status = "primary",
            solidHeader = TRUE,
            width="100%",
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:15%;margin-left:20px;",
                       "可供时间分配(天)"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_work_time"))
            ),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:15%;margin-left:20px;",
                       "已分配时间 经理(天)"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_arranged_time_of_flm"))
            )),
          hr(),
          # fluidRow(
          #   h3("经理时间分配(天数)"),
          box(
            title="经理时间分配(天数)",
            status = "primary",
            solidHeader = TRUE,
            width="100%",
            tags$div(
              tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                       "总时间的百分比"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                       "经理"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                       sr_info$业务代表[1]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                       sr_info$业务代表[2]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                       sr_info$业务代表[3]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                       sr_info$业务代表[4]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:10px;width:10%",
                       sr_info$业务代表[5])
            ),
            br(),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                       "能力辅导"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_sales_training")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_flm_sales_training")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr1_sales_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr2_sales_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr3_sales_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr4_sales_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr5_sales_training", label =
                                   NULL,value=""))
            ),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                       "实地协访"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_field_work")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_flm_field_work")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr1_field_work", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr2_field_work", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr3_field_work", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr4_field_work", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr5_field_work", label =
                                   NULL,value=""))
            ),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                       "团队例会和团建"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_team_meeting")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                       textInput("p1_flm_team_meeting", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                       verbatimTextOutput("p1_sr1_team_meeting")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                       verbatimTextOutput("p1_sr2_team_meeting")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                       verbatimTextOutput("p1_sr3_team_meeting")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                       verbatimTextOutput("p1_sr4_team_meeting")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                       verbatimTextOutput("p1_sr5_team_meeting"))
            ),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                       "KPI 报告分析"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_kpi_analysis")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                       textInput("p1_flm_kpi_analysis", label =
                                   NULL,value=""))
            ),
            
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                       "行政工作"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_admin_work")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                       textInput("p1_flm_admin_work", label =
                                   NULL,value=""))
            ),
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                       "合计"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_total_management")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                       verbatimTextOutput("p1_flm_management"))
            )
          ),
          br(),
          # fluidRow(
          #   h3("产品培训"),
          box(
            title="批准代表脱岗进行产品培训(天数)",
            status = "primary",
            solidHeader = TRUE,
            width="100%",
            tags$div(
              tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                       sr_info$业务代表[1]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                       sr_info$业务代表[2]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                       sr_info$业务代表[3]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                       sr_info$业务代表[4]),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                       sr_info$业务代表[5])
            ),
            br(),
            
            tags$div(
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                       "产品培训"),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:1%;width:8%;margin-right:1%",
                       textInput("p1_sr1_product_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                       textInput("p1_sr2_product_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr3_product_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr4_product_training", label =
                                   NULL,value="")),
              tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                       textInput("p1_sr5_product_training", label =
                                   NULL,value=""))
            )
          )
        ),
        tabPanel(
          "周期2",
          hidden(
            div(id="decision2_phase2",
                
                actionButton("decision2_phase2_submit","提交"),
                downloadButton("p2_chk_data","下载中间数据"),
                #fluidRow(h3("时间分配"),
                box(
                  title="时间分配",
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;margin-left:20px;width:15%;vertical-align:middle;text-align:left;",
                             "可供时间分配(天)"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_work_time"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:15%;margin-left:20px;",
                             "已分配时间 经理(天)"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_arranged_time_of_flm"))
                  )),
                hr(),
                # fluidRow(
                #   h3("经理时间分配(天数)"),
                box(
                  title="经理时间分配(天数)",
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                             "总时间的百分比"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                             "经理"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                             sr_info$业务代表[1]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                             sr_info$业务代表[2]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                             sr_info$业务代表[3]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                             sr_info$业务代表[4]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:10px;width:10%",
                             sr_info$业务代表[5])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "能力辅导"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_sales_training")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_flm_sales_training")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr1_sales_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr2_sales_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr3_sales_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr4_sales_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr5_sales_training", label =
                                         NULL))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "实地随访"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_field_work")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_flm_field_work")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr1_field_work", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr2_field_work", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr3_field_work", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr4_field_work", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr5_field_work", label =
                                         NULL))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "团队例会和团建"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_team_meeting")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                             textInput("p2_flm_team_meeting", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                             verbatimTextOutput("p2_sr1_team_meeting")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                             verbatimTextOutput("p2_sr2_team_meeting")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                             verbatimTextOutput("p2_sr3_team_meeting")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                             verbatimTextOutput("p2_sr4_team_meeting")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                             verbatimTextOutput("p2_sr5_team_meeting"))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "KPI 报告分析"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_kpi_analysis")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                             textInput("p2_flm_kpi_analysis", label =
                                         NULL))
                  ),
                  
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "行政工作"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_admin_work")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                             textInput("p2_flm_admin_work", label =
                                         NULL))
                  ),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "合计"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_total_management")),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                             verbatimTextOutput("p2_flm_management"))
                  )
                ),
                br(),
                # fluidRow(
                #   h3("产品培训"),
                box(
                  title="批准代表脱岗进行产品培训(天数)",
                  status = "primary",
                  solidHeader = TRUE,
                  width="100%",
                  tags$div(
                    tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                             sr_info$业务代表[1]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                             sr_info$业务代表[2]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                             sr_info$业务代表[3]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                             sr_info$业务代表[4]),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                             sr_info$业务代表[5])
                  ),
                  br(),
                  tags$div(
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                             "产品培训"),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:1%;width:8%;margin-right:1%",
                             textInput("p2_sr1_product_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                             textInput("p2_sr2_product_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr3_product_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr4_product_training", label =
                                         NULL)),
                    tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                             textInput("p2_sr5_product_training", label =
                                         NULL))
                  )
                )))
        ),
        
        hidden(
          tabPanel(
            title="周期0",
            value="phase0",
            hidden(checkboxInput("hide2", "Show tab2", FALSE)),
            
            #fluidRow(h3("时间分配"),
            box(
              title="时间分配",
              status = "primary",
              solidHeader = TRUE,
              width="100%",
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:15%;margin-left:20px;",
                         "可供时间分配(天)"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_work_time"))
              ),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:15%;margin-left:20px;",
                         "已分配时间 经理(天)"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:3%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_arranged_time_of_flm"))
              )),
            hr(),
            # fluidRow(
            #   h3("经理时间分配(天数)"),
            box(
              title="经理时间分配(天数)",
              status = "primary",
              solidHeader = TRUE,
              width="100%",
              tags$div(
                tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                         "总时间的百分比"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                         "经理"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                         sr_info$业务代表[1]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                         sr_info$业务代表[2]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                         sr_info$业务代表[3]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                         sr_info$业务代表[4]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:10px;width:10%",
                         sr_info$业务代表[5])
              ),
              br(),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                         "能力辅导"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_sales_training")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_flm_sales_training")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr1_sales_training", label =
                                     NULL,value=4)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr2_sales_training", label =
                                     NULL,value=4)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr3_sales_training", label =
                                     NULL,value=6)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr4_sales_training", label =
                                     NULL,value=4)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr5_sales_training", label =
                                     NULL,value=5))
              ),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                         "实地随访"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_field_work")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_flm_field_work")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr1_field_work", label =
                                     NULL,value=6)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr2_field_work", label =
                                     NULL,value=8)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr3_field_work", label =
                                     NULL,value=8)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr4_field_work", label =
                                     NULL,value=8)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr5_field_work", label =
                                     NULL,value=10))
              ),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                         "团队例会和团建"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_team_meeting")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                         textInput("p0_flm_team_meeting", label =
                                     NULL,value=10)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                         verbatimTextOutput("p0_sr1_team_meeting")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                         verbatimTextOutput("p0_sr2_team_meeting")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                         verbatimTextOutput("p0_sr3_team_meeting")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                         verbatimTextOutput("p0_sr4_team_meeting")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2.5%;width:8%;margin-right:1.5%",
                         verbatimTextOutput("p0_sr5_team_meeting"))
              ),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                         "KPI 报告分析"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_kpi_analysis")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                         textInput("p0_flm_kpi_analysis", label =
                                     NULL,value=15))
              ),
              
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px;",
                         "行政工作"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_admin_work")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                         textInput("p0_flm_admin_work", label =
                                     NULL,value=10))
              ),
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                         "合计"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:1%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_total_management")),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:2%;width:8%;margin-right:1%",
                         verbatimTextOutput("p0_flm_management"))
              )
            ),
            br(),
            # fluidRow(
            #   h3("产品培训"),
            box(
              title="批准代表脱岗进行产品培训(天数)",
              status = "primary",
              solidHeader = TRUE,
              width="100%",
              tags$div(
                tags$div(style = "display:inline-block;margin-left:15%;vertical-align:middle;text-align:center;width:11.5%",
                         sr_info$业务代表[1]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:30px;width:8%",
                         sr_info$业务代表[2]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:25px;width:10.5%",
                         sr_info$业务代表[3]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:20px;width:10.5%",
                         sr_info$业务代表[4]),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;margin-left:15px;width:10.5%",
                         sr_info$业务代表[5])
              ),
              br(),
              
              tags$div(
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:left;width:13%;margin-left:20px",
                         "产品培训"),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:1%;width:8%;margin-right:1%",
                         textInput("p0_sr1_product_training", label =
                                     NULL,value=5)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2%;width:8%;margin-right:1%",
                         textInput("p0_sr2_product_training", label =
                                     NULL,value=5)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr3_product_training", label =
                                     NULL,value=5)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr4_product_training", label =
                                     NULL,value=5)),
                tags$div(style = "display:inline-block;vertical-align:middle;text-align:center;margin-left:2.5%;width:8%;margin-right:1.5%",
                         textInput("p0_sr5_product_training", label =
                                     NULL,value=5))
              )
            )
          ))
        
      )),
    
    tabItem(tabName = "reports",
            tabsetPanel(
              tabPanel("周期1",
                       
                       br(),
                       fluidRow(column(width = 10),
                                column(width = 2, downloadButton(outputId = "download_phase1",
                                               label = "下载周期1报告"))),
                      
                       br(),
                       box(title="市场销售报告",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report7_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report7_2"))),
                       
                       br(),
                       box(title="代表报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report1_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report1_2")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report1_3")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report1_4")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report1_5"))
                       ),
                       br(),
                       box(title="经理报告",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report2_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;;margin-left:3%",
                                    dataTableOutput("p1_report2_2"))),
                       br(),
                       box(title="分配报告",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           tags$div(style = "margin-left:3%",
                                    selectInput("p1_report3_hosp",
                                                label="选择医院",
                                                choice=c("ALL",unique(hospital_info$名称)),
                                                selected=NULL)),
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report3"))),
                       
                       br(),
                       box(title="利润贡献(总体)",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report4_1")),
                           
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report4_2"))),
                       br(),
                       box(title="利润贡献(每客户)",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           tags$div(style = "margin-left:3%",
                                    selectInput("p1_profit3_hosp",
                                                label="选择医院",
                                                choice=c("ALL",unique(hospital_info$名称)),
                                                selected=NULL)),
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report5_1"))),
                       br(),
                       box(title="销售报告",
                           width="100%",
                           status = "primary", solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report6_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report6_2")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p1_report6_3")))
                       
                       
              ),
              
              tabPanel("周期2",
                       
                       br(),
                       fluidRow(column(width = 10),
                                column(width = 2, downloadButton(outputId = "download_phase2",
                                                                 label = "下载周期2报告"))),
                       
                       br(),
                       box(title="市场销售报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report7_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report7_2"))),
                       
                       br(),
                       box(title="代表报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report1_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report1_2")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report1_3")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report1_4")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report1_5"))
                       ),
                       br(),
                       box(title="经理报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report2_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;;margin-left:3%",
                                    dataTableOutput("p2_report2_2"))),
                       br(),
                       box(title="分配报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           tags$div(style = "margin-left:3%",
                                    selectInput("p2_report3_hosp",
                                                label="选择医院",
                                                choice=c("ALL",unique(hospital_info$名称)),
                                                selected=NULL)),
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report3"))),
                       # br(),
                       # box(title="决策报告",
                       #     width="100%",
                       #     status = "primary", 
                       #     solidHeader = TRUE,
                       #     
                       #     tags$div(style = "text-align:left;margin-left:3%",
                       #              dataTableOutput("p2_report4_3"))),
                       
                       br(),
                       box(title="利润贡献(总体)",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report4_1")),
                           
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report4_2"))),
                       br(),
                       box(title="利润贡献(每客户)",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           tags$div(style = "margin-left:3%",
                                    selectInput("p2_profit3_hosp",
                                                label="选择医院",
                                                choice=c("ALL",unique(hospital_info$名称)),
                                                selected=NULL)),
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report5_1"))),
                       br(),
                       box(title="销售报告",
                           width="100%",
                           status = "primary", 
                           solidHeader = TRUE,
                           
                           
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report6_1")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report6_2")),
                           br(),br(),br(),
                           tags$div(style = "text-align:left;margin-left:3%",
                                    dataTableOutput("p2_report6_3")))
                       
              )
              
            ))
    )
)

mainsidebar <- div(
  useShinyjs(),
  sidebarMenu(
    id = "sidebarmenu",
    
    menuItem("市场信息", tabName = "news_and_WAS", icon = icon("navicon")),
    menuItem("业务代表",
             tabName = "sr",
             icon = icon("address-card-o")
    ),
    menuItem("产品", tabName = "products", icon = icon("plus-square-o")),
    menuItem("决策",
             tabName = "decisions",
             icon = icon("edit"),
             menuSubItem("业务决策", tabName = "decision1"),
             menuSubItem("管理决策", tabName = "decision2")
    ),
    menuItem("报告",
             tabName = "reports",
             icon = icon("list-alt")
    ),
    menuItem("帮助",
             tabName = "help",
             icon = icon("list-alt")
    )
  ))




server=function(input, output, session) {
  
  #hideTab(inputId = "tab1", target = "phase0")
  #### UI code --------------------------------------------------------------
  # output$header <- renderUI({
  #   if (user_input$authenticated == FALSE) {
  #     title = "区域管理培训模拟平台"
  #   } else {
  #     main_head
  #   }
  # })
  
  output$body <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      
      
      
      column(width = 4, offset = 4,
             br(), br(), br(), br(),
             br(), br(), br(), br(),
             uiOutput("uiLogin"),
             uiOutput("pass"))
      
    } else { 
      #### Your app's UI code goes here!
      mainbody
      
      
    }
  })
  
  output$sidebarpanel <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      
      
    } else {mainsidebar}
    
  })
  
  
  
  #### YOUR APP'S SERVER CODE GOES HERE ----------------------------------------
  
  observe({
    if (user_input$authenticated == T) {
      shinyjs::enable("save")
      enable("exit")
    }
  })
  
  observeEvent(input$login_button,{
    if (input$user_name=="admin2"){
      shinyjs::show(id="admin",anim=T)
      
    }
  })
  
  observeEvent(input$exit,{
    stopApp()
  })
  
  
  observeEvent(input$save,{
    if (user_input$authenticated == T) {
      time <- gsub("-","_",Sys.Date())
      saveRDS( reactiveValuesToList(input) ,
               file = paste(input$user_name,"_",time,'.RDS',sep=""))}} )
  
  observeEvent(input$load_inputs,{

    if(!file.exists(input$select_file)) {return(NULL)}

    savedInputs <- readRDS(input$select_file)


    for (i in 1:length(savedInputs)) {
      session$sendInputMessage(names(savedInputs)[i],  list(value=savedInputs[[i]]) )
    }
  })
  
  

  observe({
    toggle(condition = input$hide1, selector = "#tab1 li a[data-value=phase0]")
    toggle(condition = input$hide2, selector = "#tab2 li a[data-value=phase0]")
  })
  
 
  output$p0_total_promotional_budget <- renderText(total_promotional_budget$phase1)
  
  
  p0_calculator_result <- reactive({  calculator(input,0) })
  
  output$p0_arranged_time_of_sr1 <- renderText(p0_calculator_result()[2])
  output$p0_arranged_time_of_sr2 <- renderText(p0_calculator_result()[3])
  output$p0_arranged_time_of_sr3 <- renderText(p0_calculator_result()[4])
  output$p0_arranged_time_of_sr4 <- renderText(p0_calculator_result()[5])
  output$p0_arranged_time_of_sr5 <- renderText(p0_calculator_result()[6])
  
  output$p0_potential_sales_hosp1_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp1_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp1_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp1_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp1_1 <- renderText(
    pp_info[which(pp_info$hosp_code==1&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp1_2 <- renderText(
    pp_info[which(pp_info$hosp_code==1&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp1_3 <- renderText(
    pp_info[which(pp_info$hosp_code==1&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp1_4 <- renderText(
    pp_info[which(pp_info$hosp_code==1&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  
  output$p0_potential_sales_hosp2_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp2_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp2_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp2_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp2_1 <- renderText(
    pp_info[which(pp_info$hosp_code==2&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp2_2 <- renderText(
    pp_info[which(pp_info$hosp_code==2&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp2_3 <- renderText(
    pp_info[which(pp_info$hosp_code==2&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp2_4 <- renderText(
    pp_info[which(pp_info$hosp_code==2&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp3_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp3_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp3_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp3_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp3_1 <- renderText(
    pp_info[which(pp_info$hosp_code==3&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp3_2 <- renderText(
    pp_info[which(pp_info$hosp_code==3&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp3_3 <- renderText(
    pp_info[which(pp_info$hosp_code==3&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp3_4 <- renderText(
    pp_info[which(pp_info$hosp_code==3&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp4_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp4_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp4_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp4_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp4_1 <- renderText(
    pp_info[which(pp_info$hosp_code==4&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp4_2 <- renderText(
    pp_info[which(pp_info$hosp_code==4&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp4_3 <- renderText(
    pp_info[which(pp_info$hosp_code==4&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp4_4 <- renderText(
    pp_info[which(pp_info$hosp_code==4&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp5_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp5_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp5_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp5_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp5_1 <- renderText(
    pp_info[which(pp_info$hosp_code==5&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp5_2 <- renderText(
    pp_info[which(pp_info$hosp_code==5&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp5_3 <- renderText(
    pp_info[which(pp_info$hosp_code==5&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp5_4 <- renderText(
    pp_info[which(pp_info$hosp_code==5&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp6_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp6_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp6_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp6_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp6_1 <- renderText(
    pp_info[which(pp_info$hosp_code==6&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp6_2 <- renderText(
    pp_info[which(pp_info$hosp_code==6&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp6_3 <- renderText(
    pp_info[which(pp_info$hosp_code==6&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp6_4 <- renderText(
    pp_info[which(pp_info$hosp_code==6&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp7_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp7_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp7_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp7_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp7_1 <- renderText(
    pp_info[which(pp_info$hosp_code==7&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp7_2 <- renderText(
    pp_info[which(pp_info$hosp_code==7&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp7_3 <- renderText(
    pp_info[which(pp_info$hosp_code==7&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp7_4 <- renderText(
    pp_info[which(pp_info$hosp_code==7&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp8_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp8_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp8_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp8_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp8_1 <- renderText(
    pp_info[which(pp_info$hosp_code==8&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp8_2 <- renderText(
    pp_info[which(pp_info$hosp_code==8&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp8_3 <- renderText(
    pp_info[which(pp_info$hosp_code==8&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp8_4 <- renderText(
    pp_info[which(pp_info$hosp_code==8&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp9_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp9_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp9_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp9_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp9_1 <- renderText(
    pp_info[which(pp_info$hosp_code==9&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp9_2 <- renderText(
    pp_info[which(pp_info$hosp_code==9&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp9_3 <- renderText(
    pp_info[which(pp_info$hosp_code==9&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp9_4 <- renderText(
    pp_info[which(pp_info$hosp_code==9&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  output$p0_potential_sales_hosp10_1 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==1),]$潜力)
  output$p0_potential_sales_hosp10_2 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==2),]$潜力)
  output$p0_potential_sales_hosp10_3 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==3),]$潜力)
  output$p0_potential_sales_hosp10_4 <- renderText(
    hospital_info[which(hospital_info$phase=="周期0"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==4),]$潜力)
  
  output$p0_current_sales_hosp10_1 <- renderText(
    pp_info[which(pp_info$hosp_code==10&
                    pp_info$prod_code==1),]$pp_real_revenue)
  output$p0_current_sales_hosp10_2 <- renderText(
    pp_info[which(pp_info$hosp_code==10&
                    pp_info$prod_code==2),]$pp_real_revenue)
  output$p0_current_sales_hosp10_3 <- renderText(
    pp_info[which(pp_info$hosp_code==10&
                    pp_info$prod_code==3),]$pp_real_revenue)
  output$p0_current_sales_hosp10_4 <- renderText(
    pp_info[which(pp_info$hosp_code==10&
                    pp_info$prod_code==4),]$pp_real_revenue)
  
  
  
  
  p0_calculator_result <- reactive({  calculator(input,0) })
  
  output$p0_arranged_time_of_sr1 <- renderText(p0_calculator_result()[2])
  output$p0_arranged_time_of_sr2 <- renderText(p0_calculator_result()[3])
  output$p0_arranged_time_of_sr3 <- renderText(p0_calculator_result()[4])
  output$p0_arranged_time_of_sr4 <- renderText(p0_calculator_result()[5])
  output$p0_arranged_time_of_sr5 <- renderText(p0_calculator_result()[6])
  
  
  p0_flm_data <- reactive(get.data3(input,0))
  output$p0_total_sales_training <-renderText(sales_training(input,0))
  output$p0_flm_sales_training <- renderText(sales_training(input,0))
  output$p0_total_field_work <-renderText(field_work(input,0))
  output$p0_flm_field_work <- renderText(field_work(input,0))
  output$p0_sr1_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                  NA,
                                                  input$p0_flm_team_meeting))
  output$p0_sr2_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                  NA,
                                                  input$p0_flm_team_meeting))
  output$p0_sr3_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                  NA,
                                                  input$p0_flm_team_meeting))
  output$p0_sr4_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                  NA,
                                                  input$p0_flm_team_meeting))
  output$p0_sr5_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                  NA,
                                                  input$p0_flm_team_meeting))
  output$p0_total_team_meeting <- renderText(ifelse(input$p0_flm_team_meeting=="",
                                                    NA,
                                                    input$p0_flm_team_meeting))
  output$p0_total_kpi_analysis <- renderText(ifelse(input$p0_flm_kpi_analysis=="",
                                                    NA,
                                                    input$p0_flm_kpi_analysis))
  
  output$p0_total_admin_work <- renderText(ifelse(input$p0_flm_admin_work=="",
                                                  NA,
                                                  input$p0_flm_admin_work))
  output$p0_total_management <- renderText(sum(p0_flm_data(),na.rm=T))
  output$p0_flm_management <- renderText(sum(p0_flm_data(),na.rm=T))
  output$p0_arranged_time_of_flm <- renderText(sum(p0_flm_data(),na.rm=T))
  
  
  tmp0 <- reactive({
    pp_data1 <- pp_info
    pp_data2 <- sr_info_list
    cp_data1 <- get.data1(input,0)
    cp_data2 <- get.data2(input,0)
    tmp <- calculation(pp_data1,
                       pp_data2,
                       cp_data1,
                       cp_data2)})
  
  
  
  
  
  p0_report <- reactive({
    if (
      
      p0_calculator_result()[1] <=100 &
      p0_calculator_result()[2] <=100 &
      p0_calculator_result()[3] <=100 &
      p0_calculator_result()[4] <=100 &
      p0_calculator_result()[5] <=100 &
      p0_calculator_result()[6] <=100 &
      sum(p0_flm_data()) <=worktime
    ) {
      p0_report <- report_data(tmp0(),p0_flm_data(),null_report7)
      return(p0_report)
    }
    
  })
  
  
  output$p0_chk_data <- downloadHandler(
    filename = function() { paste("phase0", '.csv', sep='') },
    content = function(file) {
      write.csv(tmp0(), file)
    }
  )
  
  
  
  
  observeEvent(input$phase1_hospital_info,{
    shinyjs::show(id="phase1_hospital_info_box")
    shinyjs::hide(id="phase2_hospital_info_box")

  })
  
  observeEvent(input$phase2_hospital_info,{
    shinyjs::hide(id="phase1_hospital_info_box")
    shinyjs::show(id="phase2_hospital_info_box")

  })
  

  
  observeEvent(input$phase1_WAS_info,{
    shinyjs::show(id="phase1_WAS_info_box")
    shinyjs::hide(id="phase2_WAS_info_box")

  })
  
  observeEvent(input$phase2_WAS_info,{
    shinyjs::hide(id="phase1_WAS_info_box")
    shinyjs::show(id="phase2_WAS_info_box")

  })
  
  
  
  output$p1_work_time <- renderText(worktime)
  output$p2_work_time <- renderText(worktime)

  
  output$p1_flm_sales_target <- renderText(flm_target[which(flm_target$phase=="周期1"),]$target)
  output$p2_flm_sales_target <- renderText(flm_target[which(flm_target$phase=="周期2"),]$target)

  
  output$p1_was_plan <- renderDataTable(
    datatable(p1_news,
              escape = F,
              rownames = FALSE,
              colnames = c("医院","","快报","上期销售额(元)"),
              caption="",
              options =list(ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "150px", targets = 0),
                                              list(className = 'dt-left', width = "80px", targets = 1),
                                              list(className = 'dt-center', width = "250px", targets = 2:3)),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}")))
  )
  
  output$p2_was_plan <- renderDataTable(
    datatable(p2_news,
              escape = F,
              rownames = FALSE,
              caption="",
              colnames = c("医院","","快报"),
              options =list(ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "150px", targets = 0),
                                              list(className = 'dt-left', width = "80px", targets = 1),
                                              list(className = 'dt-center', width = "250px", targets = 2)),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}")))
  )
  

  output$sales_rep_info <- renderDataTable(
    datatable(sr_info_ui,
              escape = F,
              rownames = FALSE,
              colnames = c("业务代表","","销售技巧","产品知识","动力值"),
              caption="",
              options =list(ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "100px", targets = 0),
                                              list(className = 'dt-left', width = "100px", targets = 1),
                                              list(className = 'dt-center', width = "250px", targets = 2:4)),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}")))
  )
  
  output$products_info <- renderDataTable(
    datatable(product_info_ui,
              escape = F,
              rownames = FALSE,
              caption="",
              options =list(ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "150px", targets = 0),
                                              list(className = 'dt-left', width = "80px", targets = 1),
                                              list(className = 'dt-center', width = "250px", targets = 2:7)),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}"))))
  
  output$p1_hospital_info <- renderDataTable(
    datatable(select(hospital_info_ui[which(hospital_info_ui$phase=="周期1"),],-phase),
              rownames = FALSE,
              caption="",
              options =list(pageLength=50,
                            ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}"))))
  
  output$p2_hospital_info <- renderDataTable(
    datatable(select(hospital_info_ui[which(hospital_info_ui$phase=="周期2"),],-phase),
              rownames = FALSE,
              caption="",
              options =list(pageLength=50,
                            ordering = F, dom = "t",
                            columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                            initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header());",
                              "}"))))
  
 
  
  output$p1_worktime <- renderText(worktime)
  output$p2_worktime <- renderText(worktime)

  
  ##phase1
  
  output$p1_total_promotional_budget <- renderText(prettyNum(
    total_promotional_budget[which(total_promotional_budget$phase=="周期1"),]$budget,big.mark = ","))
  
  
  p1_calculator_result <- reactive({  calculator(input,1) })
  
  output$p1_arranged_promotional_budget <- renderText(p1_calculator_result()[1])
  
  output$p1_arranged_time_of_sr1 <- renderText(p1_calculator_result()[2])
  output$p1_arranged_time_of_sr2 <- renderText(p1_calculator_result()[3])
  output$p1_arranged_time_of_sr3 <- renderText(p1_calculator_result()[4])
  output$p1_arranged_time_of_sr4 <- renderText(p1_calculator_result()[5])
  output$p1_arranged_time_of_sr5 <- renderText(p1_calculator_result()[6])
  
  output$p1_potential_sales_hosp1_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp1_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp1_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp1_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp1_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==1&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp1_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==1&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp1_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==1&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp1_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==1&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  
  output$p1_potential_sales_hosp2_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp2_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp2_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp2_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp2_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==2&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp2_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==2&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp2_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==2&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp2_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==2&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp3_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp3_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp3_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp3_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp3_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==3&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp3_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==3&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp3_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==3&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp3_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==3&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp4_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp4_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp4_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp4_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp4_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==4&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp4_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==4&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp4_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==4&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp4_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==4&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp5_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp5_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp5_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp5_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp5_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==5&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp5_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==5&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp5_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==5&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp5_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==5&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp6_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp6_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp6_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp6_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp6_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==6&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp6_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==6&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp6_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==6&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp6_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==6&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp7_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp7_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp7_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp7_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp7_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==7&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp7_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==7&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp7_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==7&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp7_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==7&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp8_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp8_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp8_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp8_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp8_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==8&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp8_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==8&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp8_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==8&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp8_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==8&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp9_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp9_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp9_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp9_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp9_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==9&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp9_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==9&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp9_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==9&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp9_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==9&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p1_potential_sales_hosp10_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp10_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp10_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p1_potential_sales_hosp10_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期1"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p1_current_sales_hosp10_1 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==10&
                   tmp0()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp10_2 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==10&
                   tmp0()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp10_3 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==10&
                   tmp0()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p1_current_sales_hosp10_4 <- renderText(prettyNum(
    tmp0()[which(tmp0()$phase=="周期0"&
                   tmp0()$hosp_code==10&
                   tmp0()$prod_code==4),]$real_revenue,big.mark = ","))
  
 
  p1_flm_data <- reactive(get.data3(input,1))
  
  
  output$p1_total_sales_training <-renderText(sales_training(input,1))
  output$p1_flm_sales_training <- renderText(sales_training(input,1))
  output$p1_total_field_work <-renderText(field_work(input,1))
  output$p1_flm_field_work <- renderText(field_work(input,1))
  output$p1_sr1_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                  NA,
                                                  input$p1_flm_team_meeting))
  output$p1_sr2_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                  NA,
                                                  input$p1_flm_team_meeting))
  output$p1_sr3_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                  NA,
                                                  input$p1_flm_team_meeting))
  output$p1_sr4_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                  NA,
                                                  input$p1_flm_team_meeting))
  output$p1_sr5_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                  NA,
                                                  input$p1_flm_team_meeting))
  output$p1_total_team_meeting <- renderText(ifelse(input$p1_flm_team_meeting=="",
                                                    NA,
                                                    input$p1_flm_team_meeting))
  output$p1_total_kpi_analysis <- renderText(ifelse(input$p1_flm_kpi_analysis=="",
                                                    NA,
                                                    input$p1_flm_kpi_analysis))
  
  output$p1_total_admin_work <- renderText(ifelse(input$p1_flm_admin_work=="",
                                                  NA,
                                                  input$p1_flm_admin_work))
  output$p1_total_management <- renderText(sum(p1_flm_data(),na.rm=T))
  output$p1_flm_management <- renderText(sum(p1_flm_data(),na.rm=T))
  output$p1_arranged_time_of_flm <- renderText(sum(p1_flm_data(),na.rm=T))
  
  
  tmp <- reactive({
    pp_data1 <- tmp0() %>% select(hospital,
                                  hosp_code,
                                  product,
                                  prod_code,
                                  real_revenue,
                                  real_volume,
                                  sr_sales_performance,
                                  deployment_quality_index,
                                  customer_relationship_index,
                                  promotional_support_index,
                                  sales_performance,
                                  offer_attractiveness,
                                  acc_offer_attractiveness) %>%
      mutate(acc_success_value = ifelse(p0_report()$acc_success_value=="",0,p0_report()$acc_success_value))%>%
      distinct()
    
    colnames(pp_data1)[5:14] <- paste("pp_",colnames(pp_data1)[5:14],sep="")
    
    pp_data2 <- tmp0() %>% select(sales_rep,
                                  sales_level,
                                  real_revenue_by_sr,
                                  real_volume_by_sr,
                                  sr_acc_revenue,
                                  sales_skills_index,
                                  product_knowledge_index,
                                  motivation_index,
                                  sr_acc_field_work,
                                  target_revenue_realization_by_sr) %>%
      distinct()
    
    colnames(pp_data2)[3:10] <- paste("pp_",colnames(pp_data2)[3:10],sep="")
    cp_data1 <- get.data1(input,1)
    cp_data2 <- get.data2(input,1)
    tmp <- calculation(pp_data1,
                       pp_data2,
                       cp_data1,
                       cp_data2)})
  
  
  
  
  
  
  
  
  observeEvent(input$decision2_phase1_submit,{
    if (
      p1_calculator_result()[1] >100 |
      p1_calculator_result()[2] >100 |
      p1_calculator_result()[3] >100 |
      p1_calculator_result()[4] >100 |
      p1_calculator_result()[5] >100 |
      p1_calculator_result()[6] >100 |
      sum(p1_flm_data()) >worktime
    ) {
      shinyjs::alert("推广预算分配或是时间分配超出最大值！！")
      
    } else{
      
      
      
      shinyjs::show(id="decision1_phase2",anim=T)
      shinyjs::show(id="decision2_phase2",anim=T)
      shinyjs::enable(id="phase2_hospital_info")
      shinyjs::enable(id="phase2_WAS_info")
      
      #hosp1
      disable("p1_promotional_budget_hosp1")
      disable("p1_hosp1_sales_target_1")
      disable("p1_hosp1_sales_target_2")
      disable("p1_hosp1_sales_target_3")
      disable("p1_hosp1_sales_target_4")
      disable("p1_sr_hosp1")
      disable("p1_hosp1_worktime_1")
      disable("p1_hosp1_worktime_2")
      disable("p1_hosp1_worktime_3")
      disable("p1_hosp1_worktime_4")

      ##hosp2
      disable("p1_promotional_budget_hosp2")
      disable("p1_hosp2_sales_target_1")
      disable("p1_hosp2_sales_target_2")
      disable("p1_hosp2_sales_target_3")
      disable("p1_hosp2_sales_target_4")
      disable("p1_sr_hosp2")
      disable("p1_hosp2_worktime_1")
      disable("p1_hosp2_worktime_2")
      disable("p1_hosp2_worktime_3")
      disable("p1_hosp2_worktime_4")

      ##hosp3
      disable("p1_promotional_budget_hosp3")
      disable("p1_hosp3_sales_target_1")
      disable("p1_hosp3_sales_target_2")
      disable("p1_hosp3_sales_target_3")
      disable("p1_hosp3_sales_target_4")
      disable("p1_sr_hosp3")
      disable("p1_hosp3_worktime_1")
      disable("p1_hosp3_worktime_2")
      disable("p1_hosp3_worktime_3")
      disable("p1_hosp3_worktime_4")
      
      ##hosp4
      disable("p1_promotional_budget_hosp4")
      disable("p1_hosp4_sales_target_1")
      disable("p1_hosp4_sales_target_2")
      disable("p1_hosp4_sales_target_3")
      disable("p1_hosp4_sales_target_4")
      disable("p1_sr_hosp4")
      disable("p1_hosp4_worktime_1")
      disable("p1_hosp4_worktime_2")
      disable("p1_hosp4_worktime_3")
      disable("p1_hosp4_worktime_4")

      ##hosp5
      disable("p1_promotional_budget_hosp5")
      disable("p1_hosp5_sales_target_1")
      disable("p1_hosp5_sales_target_2")
      disable("p1_hosp5_sales_target_3")
      disable("p1_hosp5_sales_target_4")
      disable("p1_sr_hosp5")
      disable("p1_hosp5_worktime_1")
      disable("p1_hosp5_worktime_2")
      disable("p1_hosp5_worktime_3")
      disable("p1_hosp5_worktime_4")

      ##hosp6
      disable("p1_promotional_budget_hosp6")
      disable("p1_hosp6_sales_target_1")
      disable("p1_hosp6_sales_target_2")
      disable("p1_hosp6_sales_target_3")
      disable("p1_hosp6_sales_target_4")
      disable("p1_sr_hosp6")
      disable("p1_hosp6_worktime_1")
      disable("p1_hosp6_worktime_2")
      disable("p1_hosp6_worktime_3")
      disable("p1_hosp6_worktime_4")

      ##hosp7
      disable("p1_promotional_budget_hosp7")
      disable("p1_hosp7_sales_target_1")
      disable("p1_hosp7_sales_target_2")
      disable("p1_hosp7_sales_target_3")
      disable("p1_hosp7_sales_target_4")
      disable("p1_sr_hosp7")
      disable("p1_hosp7_worktime_1")
      disable("p1_hosp7_worktime_2")
      disable("p1_hosp7_worktime_3")
      disable("p1_hosp7_worktime_4")

      ##hosp8
      disable("p1_promotional_budget_hosp8")
      disable("p1_hosp8_sales_target_1")
      disable("p1_hosp8_sales_target_2")
      disable("p1_hosp8_sales_target_3")
      disable("p1_hosp8_sales_target_4")
      disable("p1_sr_hosp8")
      disable("p1_hosp8_worktime_1")
      disable("p1_hosp8_worktime_2")
      disable("p1_hosp8_worktime_3")
      disable("p1_hosp8_worktime_4")

      ##hosp9
      disable("p1_promotional_budget_hosp9")
      disable("p1_hosp9_sales_target_1")
      disable("p1_hosp9_sales_target_2")
      disable("p1_hosp9_sales_target_3")
      disable("p1_hosp9_sales_target_4")
      disable("p1_sr_hosp9")
      disable("p1_hosp9_worktime_1")
      disable("p1_hosp9_worktime_2")
      disable("p1_hosp9_worktime_3")
      disable("p1_hosp9_worktime_4")

      ##hosp10
      disable("p1_promotional_budget_hosp10")
      disable("p1_hosp10_sales_target_1")
      disable("p1_hosp10_sales_target_2")
      disable("p1_hosp10_sales_target_3")
      disable("p1_hosp10_sales_target_4")
      disable("p1_sr_hosp10")
      disable("p1_hosp10_worktime_1")
      disable("p1_hosp10_worktime_2")
      disable("p1_hosp10_worktime_3")
      disable("p1_hosp10_worktime_4")

      disable("p1_sr1_sales_training")
      disable("p1_sr2_sales_training")
      disable("p1_sr3_sales_training")
      disable("p1_sr4_sales_training")
      disable("p1_sr5_sales_training")
      disable("p1_sr1_field_work")
      disable("p1_sr2_field_work")
      disable("p1_sr3_field_work")
      disable("p1_sr4_field_work")
      disable("p1_sr5_field_work")
      disable("p1_flm_team_meeting")
      disable("p1_flm_kpi_analysis")
      disable("p1_flm_admin_work")
      disable("p1_sr1_product_training")
      disable("p1_sr2_product_training")
      disable("p1_sr3_product_training")
      disable("p1_sr4_product_training")
      disable("p1_sr5_product_training")
      shinyjs::alert("周期1决策已提交，无法再修改。")}
  })
  
  p1_report <- eventReactive(input$decision2_phase1_submit,{
    if (
      
      p1_calculator_result()[1] <=100 &
      p1_calculator_result()[2] <=100 &
      p1_calculator_result()[3] <=100 &
      p1_calculator_result()[4] <=100 &
      p1_calculator_result()[5] <=100 &
      p1_calculator_result()[6] <=100 &
      sum(p1_flm_data()) <=worktime
    ) {
      p1_report <- report_data(tmp(),p1_flm_data(),p0_report()$report7_mod1)
      return(p1_report)
    } else {
      return(NULL)
    }
    
  })
  
  
  
  
  # output$p1_chk_data <- renderDataTable(datatable(tmp()))
  
  output$p1_chk_data <- downloadHandler(
    filename = function() { paste("phase1", '.csv', sep='') },
    content = function(file) {
      write.csv(select(tmp(),
                       product_knowledge_index,
                       sales_skills_index,
                       motivation_index,
                       sr_sales_performance,
                       sales_performance,
                       deployment_quality_index,
                       promotional_support_index,
                       customer_relationship_index,
                       sales_performance,
                       oa_customer_relationship_factor,
                       oa_sales_performance_factor,
                       cp_offer_attractiveness)
                , file)
    }
  )
  
  
  output$p1_report1_1 <- 
    renderDataTable(datatable(p1_report()$report1_mod1,
                              caption="时间分配",
                              options = 
                                list(ordering = F, dom = "t",
                                     # autoWidth = TRUE,
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report1_2 <- 
    renderDataTable(datatable(p1_report()$report1_mod2,
                              caption="产品知识",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report1_3 <- 
    renderDataTable(datatable(p1_report()$report1_mod3,
                              caption="经验",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  output$p1_report1_4 <- 
    renderDataTable(datatable(p1_report()$report1_mod4,
                              caption="销售技巧",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report1_5 <- 
    renderDataTable(datatable(p1_report()$report1_mod5,
                              caption="动力值",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report2_1 <- 
    renderDataTable(datatable(p1_report()$report2_mod1,
                              caption="职员成本",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report2_2 <- 
    renderDataTable(datatable(p1_report()$report2_mod2,
                              caption="时间分配",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  p1_report3_mod1 <- reactive({
    if (input$p1_report3_hosp=="ALL") {
      out <- p1_report()$report3_mod1
    } else{
      data <- p1_report()$report3_mod1
      out <- data[which(data$医院==input$p1_report3_hosp),]
    }
    
    out
  })
  
  
  
  output$p1_report3 <- 
    renderDataTable(datatable(p1_report3_mod1(),
                              rownames = F,
                              caption="时间分配",
                              options = 
                                list(pageLength = 30,
                                     ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  
  output$p1_report4_1 <- 
    renderDataTable(datatable(p1_report()$report4_mod1,
                              caption="利润贡献 每产品(总)",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report4_1 <- 
    renderDataTable(datatable(p1_report()$report4_mod2,
                              caption="利润贡献 (总体)",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  
  p1_report5_mod1 <- reactive({
    if (input$p1_profit3_hosp=="ALL") {
      out <- p1_report()$report5_mod1
    } else {
      data <- p1_report()$report5_mod1
      out <- data[which(data$医院==input$p1_profit3_hosp),]
    }
    out
  })
  
  output$p1_report5_1 <- 
    renderDataTable(datatable(p1_report5_mod1(),
                              rownames = F,
                              caption="利润贡献 每客户",
                              options = 
                                list(pageLength = 120,
                                     ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report6_1 <- 
    renderDataTable(datatable(p1_report()$report6_mod1,
                              caption="销售额和数量/客户",
                              options = 
                                list(pageLength = 120,
                                     ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report6_2 <- 
    renderDataTable(datatable(p1_report()$report6_mod2,
                              caption="销售额和数量/代表",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p1_report6_3 <- 
    renderDataTable(datatable(p1_report()$report6_mod3,
                              caption="销售额和数量/产品",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  p1_report7_mod1 <- reactive({
    if ( is.null(p1_report())) {NULL} else {
    report7_mod1 <- p1_report()$report7_mod1
    report7_mod1 <- report7_mod1 %>% 
      gather(variable,`值`,-phase) %>%
      spread(phase,`值`)
    
    
    report7_mod1 <- report7_mod1 %>%
      left_join(report7_mod1_rank,by="variable") %>%
      arrange(rank) %>%
      select(-variable,-rank)
    
    rownames(report7_mod1) <- report7_mod1$name
    
    report7_mod1 <- report7_mod1 %>%
      select(-name)
    }
    
  })
  
  output$p1_report7_1 <- 
    renderDataTable(datatable(p1_report7_mod1(),
                              caption="商业价值",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  output$p1_report7_2 <- 
    renderDataTable(datatable(p1_report()$report7_mod2,
                              caption="销售业绩",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$download_phase1 <- downloadHandler(
      filename = function() { paste("phase1", '.xlsx', sep='') },
      content = function(file) {
        saveWorkbook(writeDown("phase1",p1_report(),p1_report7_mod1()),
                     file,
                     overwrite = TRUE)
        
        
      }
    )
  
  observe({
    if (!is.null(p1_report7_mod1())) {
      saveWorkbook(writeDown("phase1",p1_report(),p1_report7_mod1()),
                   file = paste("users/",input$user_name,"/",
                                user_input$time,"_",
                                user_input$time_stamp,"/report_phase1.xlsx",sep=""),
                   overwrite = TRUE)
    }
  })
  
  
  
  
  
  
  ##phase2
  
  output$p2_total_promotional_budget <- renderText(prettyNum(
    total_promotional_budget[which(total_promotional_budget$phase=="周期2"),]$budget,big.mark = ","))
  
  
  p2_calculator_result <- reactive(calculator(input,2))
  
  
  
  output$p2_arranged_promotional_budget <- renderText(p2_calculator_result()[1])
  output$p2_arranged_time_of_sr1 <- renderText(p2_calculator_result()[2])
  output$p2_arranged_time_of_sr2 <- renderText(p2_calculator_result()[3])
  output$p2_arranged_time_of_sr3 <- renderText(p2_calculator_result()[4])
  output$p2_arranged_time_of_sr4 <- renderText(p2_calculator_result()[5])
  output$p2_arranged_time_of_sr5 <- renderText(p2_calculator_result()[6])
  
  output$p2_potential_sales_hosp1_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp1_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp1_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp1_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==1&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp1_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==1&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp1_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==1&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp1_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==1&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp1_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==1&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp2_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp2_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp2_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp2_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==2&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp2_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==2&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp2_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==2&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp2_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==2&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp2_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==2&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp3_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp3_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp3_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp3_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==3&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp3_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==3&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp3_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==3&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp3_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==3&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp3_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==3&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp4_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp4_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp4_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp4_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==4&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp4_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==4&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp4_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==4&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp4_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==4&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp4_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==4&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp5_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp5_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp5_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp5_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==5&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp5_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==5&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp5_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==5&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp5_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==5&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp5_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==5&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp6_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp6_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp6_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp6_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==6&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp6_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==6&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp6_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==6&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp6_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==6&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp6_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==6&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp7_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp7_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp7_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp7_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==7&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp7_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==7&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp7_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==7&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp7_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==7&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp7_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==7&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp8_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp8_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp8_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp8_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==8&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp8_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==8&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp8_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==8&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp8_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==8&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp8_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==8&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp9_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp9_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp9_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp9_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==9&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp9_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==9&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp9_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==9&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp9_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==9&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp9_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==9&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  output$p2_potential_sales_hosp10_1 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==1),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp10_2 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==2),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp10_3 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==3),]$`潜力(元)`,big.mark = ","))
  output$p2_potential_sales_hosp10_4 <- renderText(prettyNum(
    hospital_info[which(hospital_info$phase=="周期2"&
                          hospital_info$hosp_code==10&
                          hospital_info$prod_code==4),]$`潜力(元)`,big.mark = ","))
  
  output$p2_current_sales_hosp10_1 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==10&
                  tmp()$prod_code==1),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp10_2 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==10&
                  tmp()$prod_code==2),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp10_3 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==10&
                  tmp()$prod_code==3),]$real_revenue,big.mark = ","))
  output$p2_current_sales_hosp10_4 <- renderText(prettyNum(
    tmp()[which(tmp()$phase=="周期1"&
                  tmp()$hosp_code==10&
                  tmp()$prod_code==4),]$real_revenue,big.mark = ","))
  
  
  
  p2_flm_data <- reactive(get.data3(input,2))
  
  output$p2_total_sales_training <-renderText(sales_training(input,2))
  output$p2_flm_sales_training <- renderText(sales_training(input,2))
  output$p2_total_field_work <-renderText(field_work(input,2))
  output$p2_flm_field_work <- renderText(field_work(input,2))
  output$p2_sr1_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                  NA,
                                                  input$p2_flm_team_meeting))
  output$p2_sr2_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                  NA,
                                                  input$p2_flm_team_meeting))
  output$p2_sr3_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                  NA,
                                                  input$p2_flm_team_meeting))
  output$p2_sr4_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                  NA,
                                                  input$p2_flm_team_meeting))
  output$p2_sr5_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                  NA,
                                                  input$p2_flm_team_meeting))
  output$p2_total_team_meeting <- renderText(ifelse(input$p2_flm_team_meeting=="",
                                                    NA,
                                                    input$p2_flm_team_meeting))
  output$p2_total_kpi_analysis <- renderText(ifelse(input$p2_flm_kpi_analysis=="",
                                                    NA,
                                                    input$p2_flm_kpi_analysis))
  
  output$p2_total_admin_work <- renderText(ifelse(input$p2_flm_admin_work=="",
                                                  NA,
                                                  input$p2_flm_admin_work))
  output$p2_total_management <- renderText(sum(p2_flm_data(),na.rm=T))
  output$p2_flm_management <- renderText(sum(p2_flm_data(),na.rm=T))
  output$p2_arranged_time_of_flm <- renderText(sum(p2_flm_data(),na.rm=T))
  
  
  
  tmp2 <- reactive({
    pp_data1 <- tmp() %>% select(hospital,
                                 hosp_code,
                                 product,
                                 prod_code,
                                 real_revenue,
                                 real_volume,
                                 sr_sales_performance,
                                 deployment_quality_index,
                                 customer_relationship_index,
                                 promotional_support_index,
                                 sales_performance,
                                 offer_attractiveness,
                                 acc_offer_attractiveness) %>%
      mutate(acc_success_value = p1_report()$acc_success_value)%>%
      distinct()
    
    colnames(pp_data1)[5:14] <- paste("pp_",colnames(pp_data1)[5:14],sep="")
    
    pp_data2 <- tmp() %>% select(sales_rep,
                                 sales_level,
                                 real_revenue_by_sr,
                                 real_volume_by_sr,
                                 sr_acc_revenue,
                                 sales_skills_index,
                                 product_knowledge_index,
                                 motivation_index,
                                 sr_acc_field_work,
                                 target_revenue_realization_by_sr) %>%
      distinct()
    
    colnames(pp_data2)[3:10] <- paste("pp_",colnames(pp_data2)[3:10],sep="")
    cp_data1 <- get.data1(input,2)
    cp_data2 <- get.data2(input,2)
    tmp <- calculation(pp_data1,
                       pp_data2,
                       cp_data1,
                       cp_data2)})
  
  
  
  
  
  
  
  observeEvent(input$decision2_phase2_submit, {
    
    if (
      p2_calculator_result()[1] >100 |
      p2_calculator_result()[2] >100 |
      p2_calculator_result()[3] >100 |
      p2_calculator_result()[4] >100 |
      p2_calculator_result()[5] >100 |
      p2_calculator_result()[6] >100 |
      sum(p2_flm_data()) >worktime
    ) {
      shinyjs::alert("推广预算分配或是时间分配超出最大值！！")
      
    } else{
      
      
      shinyjs::show(id="decision1_phase3")
      shinyjs::show(id="decision2_phase3")
      shinyjs::enable(id="phase3_hospital_info")
      shinyjs::enable(id="phase3_WAS_info")
      
      #hosp1
      disable("p2_promotional_budget_hosp1")
      disable("p2_hosp1_sales_target_1")
      disable("p2_hosp1_sales_target_2")
      disable("p2_hosp1_sales_target_3")
      disable("p2_hosp1_sales_target_4")
      disable("p2_sr_hosp1")
      disable("p2_hosp1_worktime_1")
      disable("p2_hosp1_worktime_2")
      disable("p2_hosp1_worktime_3")
      disable("p2_hosp1_worktime_4")

      ##hosp2
      disable("p2_promotional_budget_hosp2")
      disable("p2_hosp2_sales_target_1")
      disable("p2_hosp2_sales_target_2")
      disable("p2_hosp2_sales_target_3")
      disable("p2_hosp2_sales_target_4")
      disable("p2_sr_hosp2")
      disable("p2_hosp2_worktime_1")
      disable("p2_hosp2_worktime_2")
      disable("p2_hosp2_worktime_3")
      disable("p2_hosp2_worktime_4")

      ##hosp3
      disable("p2_promotional_budget_hosp3")
      disable("p2_hosp3_sales_target_1")
      disable("p2_hosp3_sales_target_2")
      disable("p2_hosp3_sales_target_3")
      disable("p2_hosp3_sales_target_4")
      disable("p2_sr_hosp3")
      disable("p2_hosp3_worktime_1")
      disable("p2_hosp3_worktime_2")
      disable("p2_hosp3_worktime_3")
      disable("p2_hosp3_worktime_4")
      
      ##hosp4
      disable("p2_promotional_budget_hosp4")
      disable("p2_hosp4_sales_target_1")
      disable("p2_hosp4_sales_target_2")
      disable("p2_hosp4_sales_target_3")
      disable("p2_hosp4_sales_target_4")
      disable("p2_sr_hosp4")
      disable("p2_hosp4_worktime_1")
      disable("p2_hosp4_worktime_2")
      disable("p2_hosp4_worktime_3")
      disable("p2_hosp4_worktime_4")

      ##hosp5
      disable("p2_promotional_budget_hosp5")
      disable("p2_hosp5_sales_target_1")
      disable("p2_hosp5_sales_target_2")
      disable("p2_hosp5_sales_target_3")
      disable("p2_hosp5_sales_target_4")
      disable("p2_sr_hosp5")
      disable("p2_hosp5_worktime_1")
      disable("p2_hosp5_worktime_2")
      disable("p2_hosp5_worktime_3")
      disable("p2_hosp5_worktime_4")

      ##hosp6
      disable("p2_promotional_budget_hosp6")
      disable("p2_hosp6_sales_target_1")
      disable("p2_hosp6_sales_target_2")
      disable("p2_hosp6_sales_target_3")
      disable("p2_hosp6_sales_target_4")
      disable("p2_sr_hosp6")
      disable("p2_hosp6_worktime_1")
      disable("p2_hosp6_worktime_2")
      disable("p2_hosp6_worktime_3")
      disable("p2_hosp6_worktime_4")

      ##hosp7
      disable("p2_promotional_budget_hosp7")
      disable("p2_hosp7_sales_target_1")
      disable("p2_hosp7_sales_target_2")
      disable("p2_hosp7_sales_target_3")
      disable("p2_hosp7_sales_target_4")
      disable("p2_sr_hosp7")
      disable("p2_hosp7_worktime_1")
      disable("p2_hosp7_worktime_2")
      disable("p2_hosp7_worktime_3")
      disable("p2_hosp7_worktime_4")

      ##hosp8
      disable("p2_promotional_budget_hosp8")
      disable("p2_hosp8_sales_target_1")
      disable("p2_hosp8_sales_target_2")
      disable("p2_hosp8_sales_target_3")
      disable("p2_hosp8_sales_target_4")
      disable("p2_sr_hosp8")
      disable("p2_hosp8_worktime_1")
      disable("p2_hosp8_worktime_2")
      disable("p2_hosp8_worktime_3")
      disable("p2_hosp8_worktime_4")

      ##hosp9
      disable("p2_promotional_budget_hosp9")
      disable("p2_hosp9_sales_target_1")
      disable("p2_hosp9_sales_target_2")
      disable("p2_hosp9_sales_target_3")
      disable("p2_hosp9_sales_target_4")
      disable("p2_sr_hosp9")
      disable("p2_hosp9_worktime_1")
      disable("p2_hosp9_worktime_2")
      disable("p2_hosp9_worktime_3")
      disable("p2_hosp9_worktime_4")

      ##hosp10
      disable("p2_promotional_budget_hosp10")
      disable("p2_hosp10_sales_target_1")
      disable("p2_hosp10_sales_target_2")
      disable("p2_hosp10_sales_target_3")
      disable("p2_hosp10_sales_target_4")
      disable("p2_sr_hosp10")
      disable("p2_hosp10_worktime_1")
      disable("p2_hosp10_worktime_2")
      disable("p2_hosp10_worktime_3")
      disable("p2_hosp10_worktime_4")

      
      disable("p2_sr1_sales_training")
      disable("p2_sr2_sales_training")
      disable("p2_sr3_sales_training")
      disable("p2_sr4_sales_training")
      disable("p2_sr5_sales_training")
      disable("p2_sr1_field_work")
      disable("p2_sr2_field_work")
      disable("p2_sr3_field_work")
      disable("p2_sr4_field_work")
      disable("p2_sr5_field_work")
      disable("p2_flm_team_meeting")
      disable("p2_flm_kpi_analysis")
      disable("p2_flm_admin_work")
      disable("p2_sr1_product_training")
      disable("p2_sr2_product_training")
      disable("p2_sr3_product_training")
      disable("p2_sr4_product_training")
      disable("p2_sr5_product_training")
      shinyjs::alert("周期2决策已提交，无法再修改。")
      time <- gsub("-","_",Sys.Date())
      saveRDS( reactiveValuesToList(input) ,
               file = paste(input$user_name,"_",time,'.RDS',sep=""))
      
      }
  })
  
  p2_report<- eventReactive(input$decision2_phase2_submit,{
    if (
      
      p2_calculator_result()[1] <=100 &
      p2_calculator_result()[2] <=100 &
      p2_calculator_result()[3] <=100 &
      p2_calculator_result()[4] <=100 &
      p2_calculator_result()[5] <=100 &
      p2_calculator_result()[6] <=100 &
      sum(p2_flm_data(),na.rm=T) <=worktime
    ) {
      p2_report <- report_data(tmp2(),p2_flm_data(),p1_report()$report7_mod1)
      return(p2_report)
    } 
    
  })
  
  # output$p2_chk_data <- renderDataTable(tmp2())
  
  output$p2_chk_data <- downloadHandler(
    filename = function() { paste("phase2", '.csv', sep='') },
    content = function(file) {
      write.csv(tmp2(), file)
    }
  )
  
  output$p2_report1_1 <- 
    renderDataTable(datatable(p2_report()$report1_mod1,
                              caption="时间分配",
                              options = 
                                list(ordering = F, dom = "t",
                                     # autoWidth = TRUE,
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report1_2 <- 
    renderDataTable(datatable(p2_report()$report1_mod2,
                              caption="产品知识",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report1_3 <- 
    renderDataTable(datatable(p2_report()$report1_mod3,
                              caption="经验",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  output$p2_report1_4 <- 
    renderDataTable(datatable(p2_report()$report1_mod4,
                              caption="销售技巧",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report1_5 <- 
    renderDataTable(datatable(p2_report()$report1_mod5,
                              caption="动力值",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report2_1 <- 
    renderDataTable(datatable(p2_report()$report2_mod1,
                              caption="职员成本",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report2_2 <- 
    renderDataTable(datatable(p2_report()$report2_mod2,
                              caption="时间分配",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  p2_report3_mod1 <- reactive({
    if (input$p2_report3_hosp=="ALL") {
      out <- p2_report()$report3_mod1
    } else{
      data <- p2_report()$report3_mod1
      out <- data[which(data$医院==input$p2_report3_hosp),]
    }
    
    out
  })
  
  
  
  output$p2_report3 <- 
    renderDataTable(datatable(p2_report3_mod1(),
                              rownames = F,
                              caption="时间分配",
                              options = 
                                list(pageLength = 30,
                                     ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report4_1 <- 
    renderDataTable(datatable(p2_report()$report4_mod1,
                              caption="利润贡献 每产品(总)",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report4_2 <- 
    renderDataTable(datatable(p2_report()$report4_mod2,
                              caption="利润贡献 (总体)",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  
  p2_report5_mod1 <- reactive({
    if (input$p2_profit3_hosp=="ALL") {
      out <- p2_report()$report5_mod1
    } else {
      data <- p2_report()$report5_mod1
      out <- data[which(data$医院==input$p2_profit3_hosp),]
    }
    out
  })
  
  output$p2_report5_1 <- 
    renderDataTable(datatable(p2_report5_mod1(),
                              rownames = F,
                              caption="利润贡献 每客户",
                              options = 
                                list(pageLength = 120,
                                     ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report6_1 <- 
    renderDataTable(datatable(p2_report()$report6_mod1,
                              caption="销售额和数量/客户",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report6_2 <- 
    renderDataTable(datatable(p2_report()$report6_mod2,
                              caption="销售额和数量/代表",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$p2_report6_3 <- 
    renderDataTable(datatable(p2_report()$report6_mod3,
                              caption="销售额和数量/产品",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  p2_report7_mod1 <- reactive({
    if ( is.null(p2_report())) {NULL} else {
    report7_mod1 <- p2_report()$report7_mod1
    report7_mod1 <- report7_mod1 %>% 
      gather(variable,`值`,-phase) %>%
      spread(phase,`值`)
    
    
    report7_mod1 <- report7_mod1 %>%
      left_join(report7_mod1_rank,by="variable") %>%
      arrange(rank) %>%
      select(-variable,-rank)
    
    rownames(report7_mod1) <- report7_mod1$name
    
    report7_mod1 <- report7_mod1 %>%
      select(-name)
    }
    
  })
  
  output$p2_report7_1 <- 
    renderDataTable(datatable(p2_report7_mod1(),
                              caption="商业价值",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  output$p2_report7_2 <- 
    renderDataTable(datatable(p2_report()$report7_mod2,
                              caption="销售业绩",
                              options = 
                                list(ordering = F, dom = "t",
                                     columnDefs = list(list(className = 'dt-center', width = "250px", targets = "_all")),
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#41555D', 'color': '#fff'});",
                                       "}"))))
  
  output$download_phase2 <- downloadHandler(
    filename = function() { paste("phase2", '.xls', sep='') },
    content = function(file) {
      saveWorkbook(writeDown("phase2",p2_report(),p2_report7_mod1()),
                   file,
                   overwrite = TRUE)
    }
  )
  
  
  
  
  
  
  #### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "",
                               time_stamp = NULL,time = NULL)
  
  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    # credentials <- readRDS("credentials/credentials.rds")
    # credentials <-  read.csv("credentials/register_data.csv", sep = ",",
    #                          stringsAsFactors = FALSE)
    user_input$authenticated = T
    user_input$valid_credentials = T
    
    # db <- mongo(collection = collectionName,
    #             url = sprintf(
    #               "mongodb://%s/%s",
    #               # options()$mongodb$username,
    #               # options()$mongodb$password,
    #               options()$mongodb$host,
    #               databaseName))
    # # Read all the entries
    # credentials <- db$find()
    # 
    # 
    # 
    # row_username <- which(credentials$user == input$user_name)
    # row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
    # 
    # # if user name row and password name row are same, credentials are valid
    # #   and retrieve locked out status
    # if (length(row_username) == 1 && 
    #     length(row_password) >= 1 &&  # more than one user may have same pw
    #     (row_username %in% row_password)) {
    #   user_input$valid_credentials <- TRUE
    #   user_input$user_locked_out <- credentials$locked_out[row_username]
    #   user_input$time_stamp <- credentials$timestamp[row_username]
    # }
    # 
    # # if user is not currently locked out but has now failed login too many times:
    # #   1. set current lockout status to TRUE
    # #   2. if username is present in credentials DF, set locked out status in 
    # #     credentials DF to TRUE and save DF
    # if (input$login_button == num_fails_to_lockout & 
    #     user_input$user_locked_out == FALSE) {
    #   
    #   user_input$user_locked_out <- TRUE
    #   
    #   if (length(row_username) == 1) {
    #     credentials$locked_out[row_username] <- TRUE
    #     
    #     write.csv(credentials, "credentials/register_data.csv")
    #   }
    # }
    # 
    # # if a user has valid credentials and is not locked out, he is authenticated      
    # if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
    #   user_input$authenticated <- TRUE
    #   user_input$time <- gsub("-","_",Sys.Date())
    #   system(paste("mkdir users/",input$user_name,"/",
    #                user_input$time,"_",user_input$time_stamp,sep=""))
    # } else {
    #   user_input$authenticated <- FALSE
    # }
    # 
    # # if user is not authenticated, set login status variable for error messages below
    # if (user_input$authenticated == FALSE) {
    #   if (user_input$user_locked_out == TRUE) {
    #     user_input$status <- "locked_out"  
    #   } else if (length(row_username) > 1) {
    #     user_input$status <- "credentials_data_error"  
    #   } else if (input$user_name == "" || length(row_username) == 0) {
    #     user_input$status <- "bad_user"
    #   } else if (input$password == "" || length(row_password) == 0) {
    #     user_input$status <- "bad_password"
    #   }
    # }
  })   
  
  # password entry UI componenets:
  #   username and password text fields, login button
  # output$uiLogin <- renderUI({
  #   wellPanel(
  #     textInput("user_name", "User Name:"),
  #     
  #     passwordInput("password", "Password:"),
  #     
  #     actionButton("login_button", "Log in")
  #   )
  # })
  output$uiLogin <- renderUI({
    fluidRow(
    wellPanel(
      
      textInput("user_name", "用户名"),
      
      passwordInput("password", "密码"),
      
      # actionButton("login_button", "Log in")
      
      # tags$div(
      # 
      # tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
      #          actionButton(inputId = "login_button",
      #                       label = "登录",
      #                       icon = icon("th"))),
      # tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:140px;text-align:center;margin-right:10px;",
      #          actionButton(inputId = "Signup_button",
      #                       label = "注册",
      #                       icon = icon("th"),
      #                       onclick ="location.href='xxxx';"
      #                       # onclick ="window.open('xxxx')"
      #          )))
      fluidRow(
        column(width = 6,
               offset = 1,
               #tags$div(style = "display:inline-block;width:16%;vertical-align:middle;margin-left:10px;text-align:center;margin-right:10px;",
                        actionButton(inputId = "login_button",
                                     label = "登录",
                                     icon = icon("th"))
               ),
        #column(width = 2),
        column(width = 1,
               #offset = 1,
               #tags$div(style = "display:inline-block;width:16%;vertical-align:middle;text-align:left;margin-right:10px;",
                        actionButton(inputId = "Signup_button",
                                     label = "注册",
                                     icon = icon("th"),
                                     onclick ="location.href='http://tm.pharbers.com/register/';"
                                     # onclick ="window.open('xxxx')"
                        )
        )
      )
    ))
  })
  
  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
  
}