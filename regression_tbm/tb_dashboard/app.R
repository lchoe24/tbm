
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)

load('./regression.RData')

show_res_all <- function(fit){
  
  ureal <- as.numeric(df0[,response]) # 실제값
  upred <- as.numeric(predict(fit, newdata=df0)) # 예측값
  mae <- mean(abs(ureal[test.idx]-upred[test.idx]))
  
  colors <- rep('train', nrow(df0)) # 색깔 - 학습데이터는 파란색, 테스트 데이터는 빨간색
  colors[test.idx] <- 'test'
  
  p <- ggplot(data.frame(predict=upred,actual=ureal,set=colors)) + 
    geom_point(aes(actual,predict,color=set)) + 
    scale_x_continuous(limits=range(ureal)) + scale_y_continuous(limits=range(ureal)) + geom_abline(intercept=0,slope=1) +
    labs(title = paste(deparse(substitute(fit)),'MAE',round(mae,3)))
  print(p)
}


ui <- dashboardPage(
  dashboardHeader(title = 'Real Estate Price Model'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Data', tabName='t-data',
               menuSubItem('Load', tabName='t-data-load'),
               menuSubItem('Process', tabName='t-data-proc')
      ),
      menuItem('Summary',
               menuSubItem('Mean', tabName='t-mean'),
               menuSubItem('Median', tabName='t-median')
      ),
      menuItem('Linear Regression',
               menuSubItem('Simple', tabName='t-lm'),
               menuSubItem('Robust', tabName='t-rlm'),
               menuSubItem('Model Pick', tabName='t-lmstep')
      ),
      menuItem('Tree',
               menuSubItem('Tree', tabName='t-tree'),
               menuSubItem('Tree(II)', tabName='t-tree2'),
               menuSubItem('Random Forest', tabName='t-rf')
      ),
      menuItem('Boosting',
               menuSubItem('GBM', tabName='t-gbm'),
               menuSubItem('XGBoost', tabName='t-xgb')
      ),
      menuItem('Regularized',
               menuSubItem('Ridge', tabName='t-ridge'),
               menuSubItem('Lasso', tabName='t-lasso')
      ),
      menuItem('Neural Network',
               menuSubItem('Keras', tabName='t-keras')
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName='t-data-load',
              DT::dataTableOutput('text_data_load')
      ),
      tabItem(tabName='t-data-proc',
              DT::dataTableOutput('text_data_proc')
      ),
      tabItem(tabName='t-lm',
              plotOutput('plot_lm'),
              verbatimTextOutput('text_lm')
      ),
      tabItem(tabName='t-rlm',
              plotOutput('plot_rlm'),
              verbatimTextOutput('text_rlm')
      ),
      tabItem(tabName='t-lmstep',
              plotOutput('plot_lmstep'),
              verbatimTextOutput('text_lmstep')
      ),
      tabItem(tabName='t-mean',
              DT::dataTableOutput('text_mean')
      ),
      tabItem(tabName='t-median',
              DT::dataTableOutput('text_median')
      ),
      tabItem(tabName='t-tree',
              plotOutput('plot_tree'),
              plotOutput('plot_tree_shape'),
              verbatimTextOutput('text_tree')
      ),
      tabItem(tabName='t-tree2',
              plotOutput('plot_tree2'),
              plotOutput('plot_tree2_shape'),
              verbatimTextOutput('text_tree2')
      ),
      tabItem(tabName='t-rf',
              plotOutput('plot_rf'),
              verbatimTextOutput('text_rf')
      ),
      tabItem(tabName='t-gbm',
              plotOutput('plot_gbm'),
              verbatimTextOutput('text_gbm')
      ),
      tabItem(tabName='t-xgb',
              plotOutput('plot_xgb'),
              verbatimTextOutput('text_xgb')
      ),
      tabItem(tabName='t-ridge',
              plotOutput('plot_ridge'),
              verbatimTextOutput('text_ridge')
      ),
      tabItem(tabName='t-lasso',
              plotOutput('plot_lasso'),
              verbatimTextOutput('text_lasso')
      )
    )
  )
)

server <- function(input, output) {
  output$text_data_load <- renderDataTable({
    read.csv('./list.csv')
  })
  
  output$text_data_proc <- renderDataTable({
    df0
  })
  
  output$text_mean <- renderDataTable({
    round(fit.result.means,3)
  })
  
  output$text_median <- renderDataTable({
    round(fit.result.medians,3)
  })
  
  output$text_lm <- renderPrint({ summary(fit.lm) })
  output$plot_lm <- renderPlot({ show_res_all(fit.lm) })
  
  output$text_rlm <- renderPrint({ summary(fit.rlm)})
  output$plot_rlm <- renderPlot({ show_res_all(fit.rlm)})
  
  output$plot_lmstep <- renderPlot({show_res_all(fit.reg)})
  output$text_lmstep <- renderPrint({summary(fit.reg)})
  
  output$plot_tree <- renderPlot({ show_res_all(fit.tree) })
  output$plot_tree_shape <- renderPlot({ plot(fit.tree$finalModel); text(fit.tree$finalModel)  })
  output$text_tree <- renderPrint({ fit.tree })
  
  output$plot_tree2 <- renderPlot({ show_res_all(fit.tree2) })
  output$plot_tree2_shape <- renderPlot({ plot(fit.tree2$finalModel); text(fit.tree2$finalModel)  })
  output$text_tree2 <- renderPrint({ fit.tree2 })
  
  
        
  output$plot_rf <- renderPlot({show_res_all(fit.rf)})
  output$text_rf <- renderPrint({summary(fit.rf)})
  
  output$plot_gbm <- renderPlot({show_res_all(fit.gbm)})
  output$text_gbm <- renderPrint({summary(fit.gbm)})
  
  output$plot_ridge <- renderPlot({show_res_all(fit.ridge)})
  output$text_ridge <- renderPrint({summary(fit.ridge)})
  
  output$plot_xgb <- renderPlot({show_res_all(fit.xgb)})
  output$text_xgb <- renderPrint({summary(fit.xgb)})
  
  output$plot_lasso <- renderPlot({show_res_all(fit.lasso)})
  output$text_lasso <- renderPrint({summary(fit.lasso)})
  
  }

shinyApp(ui, server)

