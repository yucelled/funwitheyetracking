#Eye Tracking Data Processing by Dilan Yucel

#Load libraries & read source file
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(magick)
library(tidyverse)
source('CodeFile2-2.R')


#Create ui with one tab and a drop down menu
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("Fun with Eye-Tracking", fluid = TRUE,
                  tabPanel("Heatmaps", 
                     pageWithSidebar(headerPanel(h2("Image List")),
                       sidebarPanel(width = 3,
                                    selectInput("myImages", "Let's select an image",
                                               choices = image_list)),
                         mainPanel(withSpinner(imageOutput(outputId = "myImage")))
                             
            ))
            ))


#Create server functions
server <- function(input, output) {
  
#Use ObserveEvent to control the changes in dropdown menu. If user changes the image name, bring new image on main menu.
  dropdown_change <- observeEvent(input$myImages, {
    filename <- input$myImages
    
    fixations <- find_fixations(fixations_all, filename)
    
    plot_file_path <- create_and_save_plot(fixations, filename)
    
    picture <- get_image(filename)
    plot <- get_plot_as_image(filepath = plot_file_path)
    
    image_bw <- image_normalize(image_modulate(picture, saturation = 0)) #black and white image
    image <- image_composite(image_bw, plot, 'Multiply') #blending mode
    
    #Write the rendered image to directory in jpg format
    output$myImage <- renderImage({
      tmpfile <- image %>%
        
      image_write(tempfile(fileext='jpg'), format = 'jpg')
      
      # Return a list
      list(src = tmpfile, contentType = "image/jpeg")
    })  
  })
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

