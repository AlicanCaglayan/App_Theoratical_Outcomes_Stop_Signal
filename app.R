#

library(shiny)
library(ggplot2)


ui <- fluidPage(
    
    ### Application title
    titlePanel("Theoretical Outcomes for Stop Signal Trials"),
    
    ### Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mean",
                        "Mean",
                        min = 700,
                        max = 1400,
                        value = 1200),
            
            sliderInput("sd",
                        "Standard Deviation",
                        min = 50,
                        max = 250,
                        value = 150),
            
            sliderInput("SSRT",
                        "Stop Signal Reaction Time",
                        min = 50,
                        max = 300,
                        value = 90),
            
            sliderInput("ssd_before",
                        " Stop signal delay set relative to mean reaction time (How many milliseconds in advance does the 'stop signal' come, relative to mean reaction time of primary response?)" ,
                        
                        min = 50,
                        max = 300,
                        value = 150)
            
            
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            p("In stop signal task, the subjects are first trained to perform a 'go' primary response (e.g. pressing a key) upon a 'go signal'. In our experiment, intrinsic signal of initiating a trial by first nose poke was 'go signal'. After a 'go' response (which is fast) is established, a 'stop signal' is introduced at some of the trials (e.g ~ %20). Stop signal is given with a delay set relative to estimated mean reaction time from the previous primary responses (e.g 150 ms shorter than mean reaction time of primary responses). There are three possible outcomes of a stop signal trial:", style = "font-size:13px"),
            
            tags$ul(
                tags$li("A subject can respond before the stop signal is given (early response)"),
                tags$li("A subject can respond after the stop signal is given (response after stop signal)"),
                tags$li("A subject can inhibit its primary response upon hearing stop signal (inhibited response) by reacting to stop signal (the duration required to stop is sum of stop signal delay and stop signal reaction time [time needed for reacting to the stop signal] )" ) 
            ),
            
            p("The two horse race model accounts for the relationship between the probability of the outcomes from stop signal trials and relevant parameters (e.g. stop signal reaction time). The model assumes that completing the 'go' response and inhibiting the response are in a race and these two processes are independent (e.g. hearing the stop signal does not slow down the primary response). While the 'go' response reaction times have a distribution (assumed as normal, Mean +/- SD), stop signal reaction time is assumed as constant. In the graph below, the theoretical outcomes of stop signal trials according to the model are shown. Using sliders at the left, the parameters can be adjusted (mean and standard deviation of primary response times, stop signal reaction time and stop signal delay).", style = "font-size:13px"),
          
            
            hr(),
            
            plotOutput("outcome_distribution"),
            
            
            p( "Probability density curve subdivided into the different outcomes from stop signal trials.The model assumes that probability density curve of reaction times for go and stop signal curve will be identical except the inhibited responses of stop signal trials would not be observed. The different categories are: early responses (responding before the stop signal, red), responses after the stop signal (green), and inhibited responses (blue). The dashed line shows the time of the stop signal (after stop signal delay) and the solid black line shows the time of reacting to the stop signal (thus inhibiting the response). The horizontal dark green line connecting two horizontal lines shows the stop signal reaction time (from presenting the stop signal until inhibiting the response). The frequencies of each outcome (as cumulative probability) are also in the upper left (early responses (red), at responses after the stop signal (green), and inhibited responses (blue).", style = "font-size:12px"),
           
            hr(),
            
            p( "MAIN TAKEAWAYS: ", style = "font-size:20px"),
            
            tags$ul(
              tags$li("When the delay is long (near the subject's mean reaction time) and the standard deviation of reaction times is high, a considerable proportion of the outcomes is expected to be early responses (response before the stop signal). Therefore, especially under these conditions, exclusion of early responses from the analysis will affect the construction of the inhibition curve and stop-signal reaction time calculations."),
              
              tags$li("Keeping all the other parameters (e.g standard deviation of reaction times), differences in stop-signal reaction time (increase or decrease) would not affect the proportion of early responses (as early responses are before the stop signal and processing speed of stop signal does not affect the proportion of early responses).") ) ,
            
            hr(),
            
            p( "The app is coded using R 4.0.2 (R Core Team, 2020) and 'Shiny' package. Using 'Shiny' package and interactive user interphase is created where parameters (e.g. standard deviation of reaction times) can be enter via the slider bars and the results (as graph) are displayed as output.The code is uploaded under GNU license (open source) to 'https://github.com/AlicanCaglayan/App_Theoratical_Outcomes_Stop_Signal_Task' . ", style = "font-size:11px") ,
            
            
            
            hr(),
            
            p( "For further questions, e-mail to alicancaglayanneuro@gmail.com", style = "font-size:11px")
          
            
            
            
        )
    )
)

### Make necessary calculations for probability density curve and draw the graphs 
server <- function(input, output) {
    
    output$outcome_distribution <- renderPlot({
        
        
        ### Define X values for probability density curve
        X_axis_values<- 0:2300
        
        
        
        ### calculate the probabilities for probability density curve
        probility_density_values <- 
            dnorm(X_axis_values, mean = input$mean, sd = input$sd)
        
        
        ### create a data frame to with values for X axis and probability         densities
        
        df_for_graph<- data.frame(probility_density_values,X_axis_values)
        
        ### calculate the absolute value of stop signal delay
        
        stop_signal_delay_abs <- input$mean - input$ssd_before
        
        ### calculate the duration required to respond the stop signal            (summation of stop signal reaction time and stop signal delay),           responses would be inhibited after this point
        
        stopping_time <- stop_signal_delay_abs + input$SSRT
        
        ### annotate the outcomes to probabilities (responded before stop signal, responded after stop and inhibited)
        
        df_for_graph$outcome <- "" 
        
        df_for_graph$outcome [df_for_graph$X_axis_values<stop_signal_delay_abs] <- "early responded"
        
        df_for_graph$outcome [df_for_graph$X_axis_values>=stop_signal_delay_abs & df_for_graph$X_axis_values< stopping_time ] <- "responded after SS"
        
        df_for_graph$outcome [df_for_graph$X_axis_values >= stopping_time ] <- "inhibited"
        
        
        df_for_graph$outcome<-factor(df_for_graph$outcome, levels = c("early responded", "responded after SS", "inhibited") )
        
        
        ### calculate cumulative probabilities to display
        
        probability_early_responded <- pnorm(stop_signal_delay_abs,input$mean ,input$sd)
        
        prob_responded_after_signal <- 
            pnorm(stopping_time,input$mean ,input$sd) - pnorm(stop_signal_delay_abs,input$mean ,input$sd)
        
        prob_inhibited <- 1- pnorm(stopping_time,input$mean ,input$sd)
        
        cummu_probabilities <- round(c(probability_early_responded,prob_responded_after_signal,prob_inhibited),3)
        
        df_cummu_prob<- as.data.frame(cummu_probabilities)
        
        df_cummu_prob$outcome <- factor( c("early responded", "responded after SS", "inhibited"), levels = c("early responded", "responded after SS", "inhibited") )
        
        label_position<- c(max(df_for_graph$probility_density_values),(max(df_for_graph$probility_density_values)*7/8), (max(df_for_graph$probility_density_values)*6/8) ) 
        
        df_cummu_prob$label_positions<-label_position
        
        ### draw the probability density plot with different outcomes
        
        ### draw the probability density curve for normal distribution
        ggplot(data = df_for_graph, aes(x=X_axis_values,y=probility_density_values,col=outcome)) + geom_line()+
          
        ###scale labels and colors
          scale_color_discrete(labels = c("Early response",expression(atop("Response after", "stop signal") ),"Inhibited response") )+
            
            ### add lines showing the time of stop signal delay and stopping
            geom_vline( aes(xintercept = stop_signal_delay_abs), linetype = "dashed", color="black",size = 1.1 )+ geom_vline(aes(xintercept = stopping_time),color="black", size=1.4)+
            
            ### add cumulative probabilities as text to the right corner of the graph  
            geom_text(data= df_cummu_prob,aes(label=as.character(cummu_probabilities),x=2100,y=label_positions), size =9,show_guide = FALSE) +
            
            ###add axis labels
            ylab("Probability Density")+xlab("Time (ms)")+
            
            ### add thematic aspects (font size etc...)
            theme(axis.text = element_text(size=13,face="bold",colour="black"),axis.title= element_text(size=16,face="bold",colour="black")) + theme( axis.line = element_line(colour = "black"),plot.title = element_text(hjust = 0.5,size = 18, face = "bold"),text = element_text(size=18),legend.position = "bottom") + labs(col="Outcome") +
        
            ### add guide arrows
            
            ### for stop signal
           geom_segment(aes(x = stop_signal_delay_abs-180,
                            y = label_position[1]/8, 
                            xend = stop_signal_delay_abs-20,
                            yend=label_position[1]/8), color="black", 
                        arrow = arrow (length = unit(0.5, "cm")) ) +
          
            ### for stopping time
          geom_segment(aes(x = stopping_time+180,
                           y = label_position[1]/8, 
                           xend = stopping_time+20,
                           yend=label_position[1]/8), color="black", 
                       arrow = arrow (length = unit(0.5, "cm")) ) +
           
           ### for stop signal reaction time 
          geom_segment(aes(x = stop_signal_delay_abs,
                           y = label_position[1]/4, 
                           xend = stopping_time,
                           yend=label_position[1]/4), color="chartreuse4" ,size= 1.25) +
          
          
           ### add texts to the arrows
          
          geom_text(aes(label="Stop signal",x=stop_signal_delay_abs-230,y=label_position[1]/5.6), size =5, color="black")+
          
          geom_text(aes(label="Time of stopping",x=stopping_time+290,y=label_position[1]/5.6), size =5, color="black")
        
        
    })
         ### 
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
