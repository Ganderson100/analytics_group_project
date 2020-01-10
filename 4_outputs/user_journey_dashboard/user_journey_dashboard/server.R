

library(shiny)


server <- function(input, output) {

    output$user_flow <- DT::renderDataTable({
        filtered_behaviour_flow <- behaviour_flow %>%
        filter(
            channel == input$channel,
            device == input$device,
        )
    
       if_not_set <- if(input$not_set == TRUE) {
            filter(filtered_behaviour_flow, second_page_path != "(not set)")
        } else{
            filtered_behaviour_flow
        }
        
        DT::datatable(if_not_set,
                      extensions = c("Buttons"),
                      rownames = FALSE,
                      options = list(dom = "Bfrtip", buttons = I("colvis"))
        )
        
    })
    
  output$entry_page <- DT::renderDataTable({
    entry_page <- entry_page_user_flow %>%
      filter(
        channel == input$channel,
        device == input$device,
      )

    DT::datatable(entry_page,
      extensions = c("Buttons"),
      rownames = FALSE,
      options = list(dom = "Bfrtip", buttons = I("colvis"))
    )

  })
  
  output$next_page <- DT::renderDataTable({
      next_page <- next_page_user_flow %>%
          filter(
              channel == input$channel,
              device == input$device,
              
          )
      
      DT::datatable(next_page,
                    extensions = c("Buttons"),
                    rownames = FALSE,
                    options = list(dom = "Bfrtip", buttons = I("colvis"))
      )
      
  })
  
}
