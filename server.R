library(shiny)
library(shinyjs)
library(googlesheets)
library(magrittr)

if(is.null(gs_ls("asthma_app"))){
  asthma_gs = gs_new("asthma_app", trim = TRUE, verbose = FALSE)
  asthma_gs = asthma_gs %>% 
    gs_edit_cells(ws = "Sheet1", 
                  input = data.frame(date = "test",email = "test", 
                                     numeric_day = 1, numeric_d_albuterol = 1, 
                                     numeric_night = 1, numeric_n_albuterol = 1, CL = 1),
                  trim = TRUE)
} else {
  asthma_gs = gs_title("asthma_app", verbose = T)
}

shinyServer(function(input, output){
  
  classification = function(day_max, night_max){#}, steroid_max){
    #CONTROL GROUP CLASSIFICATION
    CL_day_bins = c(0, 4, 10, 13)
    CL_night_bins = c(0, 2, 3, 5)
    # CL_steroid_bins = c(0, 1, 2, 3)
    
    #Control level for each category of questions 
    day_CL = findInterval(day_max, CL_day_bins)
    night_CL = findInterval(night_max, CL_night_bins)
    # steroid_CL = findInterval(steroid_max, CL_steroid_bins)
    
    #Control Level of the Patient
    CL = max(day_CL, night_CL)#, steroid_CL)
    return(CL)
  }
  
  monitoring = function(CL){
    if(CL == 1){
      return("Your child's asthma is under good control now.  
             Remember to keep taking your controller medication every day.")
    }
    else if(CL == 2){
      return("Your child's asthma is not under good control now.  
             Give albuterol every four hours for symptoms  
             and call your child’s asthma doctor to talk about 
             your child’s asthma symptoms.")
    }
    else if(CL == 3){
      return("Your child’s asthma is poorly controlled. 
             Give albuterol every four hours for symptoms, 
             and call your doctor today to talk about whether your child 
             needs an increase in his or her controller medication dose.")
    }
    else {
      return("Your child’s asthma is very poorly controlled and your 
             child may be having an exacerbation of his/her asthma.  
             You should take your child to see his or her doctor today.  
             He/she may need an increase in his/her controller 
             medication or a steroid burst.")
    }
  }
  
  day_max = reactive({ max(input$numeric_day, input$numeric_d_albuterol) })
  night_max = reactive({ max(input$numeric_night, input$numeric_n_albuterol) })
  # steroid_max = input$numeric_steroid
  
  observeEvent(input$submit, {
    toggle("header1")
    toggle("report")
    toggle("header2")
    toggle("plan")
    output$report = renderText({
      classification(day_max(), night_max())#, steroid_max)
    })
    
    gs = reactive({ 
      asthma_gs %>% gs_add_row(ws = "Sheet1", 
                               input = data.frame(input$date, input$email, 
                                                  input$numeric_day, 
                                                  input$numeric_d_albuterol, 
                                                  input$numeric_night, 
                                                  input$numeric_n_albuterol, 
                                                  classification(day_max(), night_max())))
      monitoring(classification(day_max(), night_max()))
    })
    
    output$plan = renderText({ gs() })
    output$header1 = renderText({ "Asthma Control Level" })
    output$header2 = renderText({ "Future Monitoring Plan" })
    
  })
}) 