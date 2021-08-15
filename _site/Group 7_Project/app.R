
#Installing packages
packages = c('DT','ggiraph','plotly','tidyverse', 'igraph','tidygraph','ggraph','lubridate','clock','tmap','clock','mapview',
             'ggpubr','widyr','tibble','shiny', 'shinyWidgets', 'ggdist','ggplot2','stats','shinythemes',
             'tools','reshape2','scales','zoo','parsetR')


for (p in packages){
    if(!require(p,character.only = T)){
        install.packages(p)
    }
    library(p,character.only = T)
}


#Importing and cleaning data
singaporedata = read.csv("data/singapore.csv")

singaporedata$endtime <-  as.Date(as.POSIXct(singaporedata$endtime, format = "%d/%m/%Y %H:%M"))
singaporedata_cleaned <- separate(singaporedata,
                                  region,
                                  into = c("Region","Town"),
                                  sep = c("-"),
                                  remove = TRUE,
                                  convert = FALSE,
                                  extra = "warn",
                                  fill = "warn")

singaporedata_cleaned <- singaporedata_cleaned %>%
    rename(
        Gender = gender,
        Age = age,
        Household_Size = household_size,
        Household_Children = household_children,
        Employment_Status = employment_status,
        Household_Contact = i1_health,
        Non_Household_Contact = i2_health,
        Times_Left_House = i7a_health,
        Tested_Self = i3_health,
        Tested_Household = i4_health,
        Dry_Cough = i5_health_1,
        Fever = i5_health_2,
        Loss_Of_Smell = i5_health_3,
        Loss_Of_Taste = i5_health_4,
        Shortness_Of_Breath = i5_health_5,
        None_Of_Above = i5_health_99,
        Confirmed_Case_Contact = i5a_health,
        Self_Isolate_Symptom = i6_health,
        Visit_Doctor = i7b_health,
        Travelled = i8_health,
        Self_Isolate_Plan = i9_health,
        Self_Isolate_Difficulty = i10_health,
        Self_Isolate_Willingness = i11_health,
        Wear_Mask = i12_health_1,
        Use_Soap = i12_health_2,
        Use_Sanitiser = i12_health_3,
        Cover_Nose_Mouth = i12_health_4,
        Avoid_people_with_symptoms = i12_health_5,
        Avoid_going_out = i12_health_6,
        Avoid_hospitals = i12_health_7,
        Avoid_public_transport = i12_health_8,
        Avoid_working_outside = i12_health_9,
        Avoid_school = i12_health_10,
        Avoid_guests = i12_health_11,
        Avoid_small_gatherings = i12_health_12,
        Avoid_medium_gatherings = i12_health_13,
        Avoid_large_gatherings = i12_health_14,
        Avoid_crowds = i12_health_15,
        Avoid_shops = i12_health_16,
        Separate_Bedrooms = i12_health_17,
        Eat_separately = i12_health_18,
        Clean_frequently = i12_health_19,
        Avoid_touching_objects = i12_health_20,
        Times_Wash_Hands_Ytd = i13_health,
        Construction = i14_health_1,
        Delivery = i14_health_2,
        Food_Retail = i14_health_3,
        Healthcare = i14_health_4,
        Logistics = i14_health_5,
        Manufacturing = i14_health_6,
        Policing = i14_health_7,
        Public_Transport = i14_health_8,
        School =i14_health_9,
        Social_Care = i14_health_10,
        Others = i14_health_96,
        Not_Sure = i14_health_98,
        Not_Work_Outside = i14_health_99,
        Remarks = i14_health_other,
        Arthritis = d1_health_1,
        Asthma = d1_health_2,
        Cancer = d1_health_3,
        Cystic_Fibrosis = d1_health_4,
        COPD = d1_health_5,
        Diabetes = d1_health_6,
        Epilepsy = d1_health_7,
        Heart_Disease = d1_health_8,
        HBP = d1_health_9,
        High_Cholesterol = d1_health_10,
        HIV = d1_health_11,
        Mental_Health = d1_health_12,
        Sclerosis = d1_health_13,
        Not_Say = d1_health_98,
        No_Conditions = d1_health_99,
        Willing_to_vacc_immed = vac_1,
        Worried_about_Covid = vac2_1,
        Worried_about_side_effects_vacc = vac2_2,
        Belief_in_Govt = vac2_3,
        Belief_vacc_protect_Covid_effects = vac2_4,
        Belief_vacc_prevent_transmission = vac2_5,
        Vaccine_importance_to_health = vac4,
        Trust_in_vacc = vac7,
        Vacc_dose = vac,
        Vacc_most_trusted = vac11
        
    ) %>%
    mutate("Pre-existing_Conditions" = ifelse(
        No_Conditions == "Yes" , "No",
        ifelse(Not_Say == "Yes" | Not_Say == " " ,"Not specified", "Yes")
        
    ))  %>%
    mutate("Date" = as.Date(endtime), "%m-%Y")%>%
    mutate(Willing_to_vacc_immed = case_when(
        Willing_to_vacc_immed == "1" ~ "Strongly Agree",
        Willing_to_vacc_immed == "2" ~ "Agree",
        Willing_to_vacc_immed == "3" ~ "Neutral",
        Willing_to_vacc_immed == "4" ~ "Disagree",
        Willing_to_vacc_immed == "5" ~ "Strongly Disagree",
        Willing_to_vacc_immed == "1 - Strongly agree" ~ "Strongly Agree",
        Willing_to_vacc_immed == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Willing_to_vacc_immed) ~ "Not specified"
    )) %>%
    dplyr::mutate(Willing_to_vacc_immed = replace_na(Willing_to_vacc_immed, "Not specified"))%>%
    mutate(Worried_about_Covid = case_when(
        Worried_about_Covid == "1" ~ "Strongly Agree",
        Worried_about_Covid == "2" ~ "Agree",
        Worried_about_Covid == "3" ~ "Neutral",
        Worried_about_Covid == "4" ~ "Disagree",
        Worried_about_Covid == "5" ~ "Strongly Disagree",
        Worried_about_Covid == "1 - Strongly agree" ~ "Strongly Agree",
        Worried_about_Covid == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Worried_about_Covid) ~ "Not specified"
    )) %>%
    dplyr::mutate(Worried_about_Covid = replace_na(Worried_about_Covid, "Not specified"))%>%
    mutate(Worried_about_side_effects_vacc = case_when(
        Worried_about_side_effects_vacc == "1" ~ "Strongly Agree",
        Worried_about_side_effects_vacc == "2" ~ "Agree",
        Worried_about_side_effects_vacc == "3" ~ "Neutral",
        Worried_about_side_effects_vacc == "4" ~ "Disagree",
        Worried_about_side_effects_vacc == "5" ~ "Strongly Disagree",
        Worried_about_side_effects_vacc == "1 - Strongly agree" ~ "Strongly Agree",
        Worried_about_side_effects_vacc == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Worried_about_side_effects_vacc) ~ "Not specified"
    )) %>%
    dplyr::mutate(Worried_about_side_effects_vacc = replace_na(Worried_about_side_effects_vacc, "Not specified"))%>%
    mutate(Belief_in_Govt = case_when(
        Belief_in_Govt == "1" ~ "Strongly Agree",
        Belief_in_Govt == "2" ~ "Agree",
        Belief_in_Govt == "3" ~ "Neutral",
        Belief_in_Govt == "4" ~ "Disagree",
        Belief_in_Govt == "5" ~ "Strongly Disagree",
        Belief_in_Govt == "1 - Strongly agree" ~ "Strongly Agree",
        Belief_in_Govt == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Belief_in_Govt) ~ "Not specified"
    )) %>%
    dplyr::mutate(Belief_in_Govt = replace_na(Belief_in_Govt, "Not specified"))%>%
    mutate(Belief_vacc_protect_Covid_effects = case_when(
        Belief_vacc_protect_Covid_effects == "1" ~ "Strongly Agree",
        Belief_vacc_protect_Covid_effects == "2" ~ "Agree",
        Belief_vacc_protect_Covid_effects == "3" ~ "Neutral",
        Belief_vacc_protect_Covid_effects == "4" ~ "Disagree",
        Belief_vacc_protect_Covid_effects == "5" ~ "Strongly Disagree",
        Belief_vacc_protect_Covid_effects == "1 - Strongly agree" ~ "Strongly Agree",
        Belief_vacc_protect_Covid_effects == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Belief_vacc_protect_Covid_effects) ~ "Not specified"
    )) %>%
    dplyr::mutate(Belief_vacc_protect_Covid_effects = replace_na(Belief_vacc_protect_Covid_effects, "Not specified"))%>%
    mutate(Belief_vacc_prevent_transmission = case_when(
        Belief_vacc_prevent_transmission == "1" ~ "Strongly Agree",
        Belief_vacc_prevent_transmission == "2" ~ "Agree",
        Belief_vacc_prevent_transmission == "3" ~ "Neutral",
        Belief_vacc_prevent_transmission == "4" ~ "Disagree",
        Belief_vacc_prevent_transmission == "5" ~ "Strongly Disagree",
        Belief_vacc_prevent_transmission == "1 - Strongly agree" ~ "Strongly Agree",
        Belief_vacc_prevent_transmission == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
        is.na(Belief_vacc_prevent_transmission) ~ "Not specified"
    ))

singaporedata_cleaned$Age_Group = cut(singaporedata_cleaned$Age, 
                               breaks=c(0,12, 19, 60,999), 
                               right = TRUE, 
                               labels = c("Children","Teenager","Adult","Senior"))

singaporedata_cleaned$Avoid_working_outside[singaporedata_cleaned$Avoid_working_outside == " "] <- "Not specified"
singaporedata_cleaned$Avoid_school[singaporedata_cleaned$Avoid_school == " "] <- "Not specified"
singaporedata_cleaned$Separate_Bedrooms[singaporedata_cleaned$Separate_Bedrooms == " "] <- "Not specified"
singaporedata_cleaned$Eat_separately[singaporedata_cleaned$Eat_separately == " "] <- "Not specified"
singaporedata_cleaned$Clean_frequently[singaporedata_cleaned$Clean_frequently == " "] <- "Not specified"
singaporedata_cleaned$Avoid_touching_objects[singaporedata_cleaned$Avoid_touching_objects == " "] <- "Not specified"
singaporedata_cleaned$Belief_vacc_prevent_transmission[singaporedata_cleaned$Belief_vacc_prevent_transmission == " "] <- "Not specified"
singaporedata_cleaned$Vaccine_importance_to_health[singaporedata_cleaned$Vaccine_importance_to_health == " "] <- "Not specified"
singaporedata_cleaned$Trust_in_vacc[singaporedata_cleaned$Trust_in_vacc == " "] <- "Not specified"
singaporedata_cleaned$Vacc_dose[singaporedata_cleaned$Vacc_dose == " "] <- "Not specified"
singaporedata_cleaned$Vacc_most_trusted[singaporedata_cleaned$Vacc_most_trusted == " "] <- "Not specified"
singaporedata_cleaned$Vacc_most_trusted[singaporedata_cleaned$Vacc_most_trusted == "8"] <- "Not specified"



# Define UI for Shiny App
ui <- navbarPage(
    title = "Singapore's Covid-19 Behavioural Analysis",
    theme = shinytheme("flatly"),
    tabPanel("Introduction",titlePanel("Overview"),
             tags$head(tags$style(
        HTML('
         #sidebarpanel {
            background-color: #ffffff00;
        }')
    )),
    sidebarLayout(position = "right",
         sidebarPanel(id = "sidebarpanel", 
                      img(src = "EDA.png", height = 244, width = 230)),
         mainPanel(p("This application features findings from a survey of over 30,000 Singapore residents on their behaviour during the Covid-19 pandemic."),
                   p("The survey was conducted over a period from Apr'20 to Jul'21."),
                   br(),
                   p("Our interactive visualisation consists of Exploratory Data Analysis (EDA) to provide users with better understanding of changes in Covid-19 behaviours and perceptions on a more granular level."))
     )),
    navbarMenu("Exploratory Data Analysis",
               tabPanel("Bar Chart", titlePanel("Behavioural Demographics by Characteristics based on Proportion"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "xvariable",
                                            label = "X variable:",
                                            choices = c("Age_Group" = "Age_Group",
                                                        "Gender" = "Gender",
                                                        "Region" = "Region",
                                                        "Town" = "Town",
                                                        "Vaccinated" = "Vacc_dose"),
                                            selected = "Age_Group"),
                                
                                selectInput(inputId = "yvariable",
                                            label = "Y variable",
                                            choices = c("Wear_Mask" = "Wear_Mask",
                                                        "employment_status" = "Employment_Status",
                                                        "Self Tested" = "Tested_Self",
                                                        "Use Soap" = "Use_Soap",
                                                        "Region" = "Region",
                                                        "Town" = "Town",
                                                        "Cover Nose and Mouth" = "Cover_Nose_Mouth",
                                                        "Use Sanitiser" = "Use_Sanitiser",
                                                        "Willingness to Self-Isolate" = "Self_Isolate_Willingness",
                                                        "Plan to Self-Isolate" = "Self_Isolate_Plan",
                                                        "Difficulty to Self-Isolate" = "Self_Isolate_Difficulty",
                                                        "Avoids Going Out" = "Avoid_going_out",
                                                        "Avoid Hospital" = "Avoid_hospitals",
                                                        "Avoid Public Transportation" = "Avoid_public_transport",
                                                        "Vaccinated" = "Vacc_dose"),
                                            selected = "Wear_Mask"),
                                
                                radioButtons(inputId = "cough",
                                             label = "Dry Cough",
                                             choices = c("Yes","No"),
                                             selected = "No"),
                                
                                radioButtons(inputId = "fever",
                                             label = "Fever",
                                             choices = c("Yes","No"),
                                             selected = "No"),
                                
                                radioButtons(inputId = "smell",
                                             label = "Loss of Smell",
                                             choices = c("Yes","No"),
                                             selected = "No"),
                                
                                radioButtons(inputId = "taste",
                                             label = "Loss of Taste",
                                             choices = c("Yes","No"),
                                             selected = "No"),
                                
                                radioButtons(inputId = "breath",
                                             label = "Shortness of Breath",
                                             choices = c("Yes","No"),
                                             selected = "No"),
                                
                            ),
                            
                            mainPanel(
                                plotlyOutput("barPlot")
                            )
                        )
               ),
               tabPanel("Line Chart", titlePanel("Line Chart"),
                        sidebarLayout(
                            sidebarPanel(
                                selectInput(inputId = "yvariable1",
                                            label = "y Variable:",
                                            choices = c("Wear_Mask" = "Wear_Mask",
                                                        "Use_Soap" = "Use_Soap",
                                                        "Use_Sanitiser" = "Use_Sanitiser",
                                                        "Cover Nose and Mouth" = "Cover_Nose_Mouth",
                                                        "Avoid people with symptoms" = "Avoid_people_with_symptoms",
                                                        "Avoid going out" = "Avoid_going_out",
                                                        "Avoid hospitals" = "Avoid_hospitals",
                                                        "Avoid public transport" = "Avoid_public_transport",
                                                        "Avoid working outside" = "Avoid_working_outside",
                                                        "Avoid school" = "Avoid_school",
                                                        "Avoid guests" = "Avoid_guests",
                                                        "Avoid small gatherings" = "Avoid_small_gatherings",
                                                        "Avoid medium gatherings" = "Avoid_medium_gatherings",
                                                        "Avoid large gatherings" = "Avoid_large_gatherings",
                                                        "Avoid crowds" = "Avoid_crowds",
                                                        "Avoid shops" = "Avoid_shops",
                                                        "Separate Bedrooms" = "Separate_Bedrooms",
                                                        "Eat separately" = "Eat_separately",
                                                        "Clean frequently" = "Clean_frequently",
                                                        "Avoid touching objects" = "Avoid_touching_objects",
                                                        "Willing to vaccinate immediately" = "Willing_to_vacc_immed",
                                                        "Worried about Covid" = "Worried_about_Covid",
                                                        "Worried about side effects of vaccine" = "Worried_about_side_effects_vacc",
                                                        "Belief in Govt to provide effective vaccine" = "Belief_in_Govt",
                                                        "Belief vaccine protects from Covid effects" = "Belief_vacc_protect_Covid_effects",
                                                        "Belief vaccine prevents transmission to others" = "Belief_vacc_prevent_transmission",
                                                        "Vaccine importance to health" = "Vaccine_importance_to_health",
                                                        "Trust in vaccine" = "Trust_in_vacc",
                                                        "Number of doses of vaccine taken" = "Vacc_dose",
                                                        "Vaccine most trusted" = "Vacc_most_trusted"
                                            ),
                                            selected = "Wear_Mask"),
                                
                                textInput(
                                    inputId = "plot_title",
                                    label = "Plot title",
                                    placeholder = "Enter text to be used as plot title"),
                                dateRangeInput("date","Selct date range",start = as.Date("2020-04-03"),end = as.Date("2021-07-23"),format = "yyyy-mm-dd"),
                                actionButton("goButton", "Confirm Selection"),
                                width = 2
                            ),
                            
                            mainPanel(
                                plotlyOutput("LineChart", height = "650px"),
                                width = 10
                            )
                        )
               ),
               tabPanel("Parallel Sets", titlePanel("Parallel Sets"),
                        sidebarLayout(
                            sidebarPanel(
                                uiOutput("select_number_of_levels_UI"),
                                uiOutput("Select_level_UI"),
                                uiOutput("date_range_slide_UI"),
                                uiOutput("action_button_UI")
                            ),
                            
                            mainPanel(
                                parsetOutput("parset", height = "700px", width = "100%")
                            )
                        )
               ))
    )




    
    
    
    
# Define server logic required for Shiny App
server <- function(input, output, session) {
    
    output$barPlot <- renderPlotly({
        
        singaporedata_cleaned <- singaporedata_cleaned %>%
            filter(Dry_Cough == input$cough, 
                   Fever == input$fever, 
                   Loss_Of_Smell == input$smell,
                   Loss_Of_Taste == input$taste,
                   Shortness_Of_Breath == input$breath,
                   Vacc_dose != "Not specified") 
        
        
        singaporedata_cleaned2 <- singaporedata_cleaned %>%
            group_by(x1 = input$xvariable, y1 = input$yvariable, endtime ) %>%
            summarise(n = length(input$yvariable)/length(singaporedata_cleaned$endtime))
        
        
        a <- ggplot(singaporedata_cleaned,
                    aes_string(y = input$xvariable, 
                               x = length(input$yvariable)/length(singaporedata_cleaned$endtime),
                               fill = input$yvariable, text = input$yvariable)) +
            geom_col() 
        
        ggplotly(a,tooltip = c("text","y"))
        
    })
    
    plot_function <- eventReactive(input$goButton,{
        
        singaporedata_cleaned[,"Date"] <- format(singaporedata_cleaned[,"Date"],"%Y-%m")
        
        singaporedata_cleaned <- singaporedata_cleaned %>%
            group_by_("Date",input$yvariable1) %>%
            summarize(n = n()) %>%
            mutate(prop = n / sum(n))
        
        singaporedata_cleaned <- as.data.frame(singaporedata_cleaned)
        singaporedata_cleaned[,"Date"] <- as.Date(as.yearmon(singaporedata_cleaned[,"Date"]))
        
        singaporedata_cleaned <- as.data.frame(singaporedata_cleaned)
        
        singaporedata_cleaned <- singaporedata_cleaned[as.Date(singaporedata_cleaned[,"Date"]) >= input$date[1],]
        singaporedata_cleaned <- singaporedata_cleaned[as.Date(singaporedata_cleaned[,"Date"]) <= input$date[2],]
        
        singaporedata_cleaned[,"prop"] <- round(singaporedata_cleaned[,"prop"],2)
        singaporedata_cleaned[,"Date"] <- format(singaporedata_cleaned[,"Date"],"%Y-%m")
        
        plot <- ggplot(data = singaporedata_cleaned,
                       aes_string(x = "Date", y = "prop", color = c(input$yvariable1), group = c(input$yvariable1),text = input$yvariable1)) +
            geom_line() +
            geom_point() +
            labs(title = isolate({
                toTitleCase(input$plot_title) 
            }))
        #plot <- plot + scale_x_date(date_labels = "%m-%Y")
        
        #plot <- plot + scale_x_date(breaks = pretty_breaks()(as.Date(c(input$date[1],input$date[2]))))
        
        
        plot <- plot + theme_classic()
        
        plot <- ggplotly(plot,tooltip = c("text","x","y"))
        
        
        plot
    })
    
    output$LineChart <- renderPlotly({
        plot <- plot_function()
        plot <- layout(plot, yaxis = list(
            title = "Proportion"
        ),xaxis = list(
            title = "Date"
        ))
        
        plot
        
    })
    
    {
        RV <- reactiveValues(
            singaporedata = read.csv("data/singapore.csv"),
            singaporedata_cleaned = NULL,
            df_test = NULL
        )
    }
    
    observe({
        singaporedata = req(RV$singaporedata)
        singaporedata$endtime <-  as.Date(as.POSIXct(singaporedata$endtime, format = "%d/%m/%Y %H:%M"))
        singaporedata_cleaned <- separate(singaporedata,
                                          region,
                                          into = c("Region","Town"),
                                          sep = c("-"),
                                          remove = TRUE,
                                          convert = FALSE,
                                          extra = "warn",
                                          fill = "warn")
        
        singaporedata_cleaned <- singaporedata_cleaned %>%
            rename(
                Gender = gender,
                Age = age,
                "Household_Size" = household_size,
                "Household_Children" = household_children,
                "Employment_Status" = employment_status,
                Household_Contact = i1_health,
                Non_Household_Contact = i2_health,
                Times_Left_House = i7a_health,
                Tested_Self = i3_health,
                Tested_Household = i4_health,
                Dry_Cough = i5_health_1,
                Fever = i5_health_2,
                Loss_Of_Smell = i5_health_3,
                Loss_Of_Taste = i5_health_4,
                Shortness_Of_Breath = i5_health_5,
                None_Of_Above = i5_health_99,
                Confirmed_Case_Contact = i5a_health,
                Self_Isolate_Symptom = i6_health,
                Visit_Doctor = i7b_health,
                Travelled = i8_health,
                Self_Isolate_Plan = i9_health,
                Self_Isolate_Difficulty = i10_health,
                Self_Isolate_Willingness = i11_health,
                "Wear_Mask" = i12_health_1,
                "Use_Soap" = i12_health_2,
                "Use_Sanitiser" = i12_health_3,
                "Cover_Nose_&_Mouth" = i12_health_4,
                "Avoid_people_with_symptoms" = i12_health_5,
                "Avoid_going_out" = i12_health_6,
                "Avoid_hospitals" = i12_health_7,
                "Avoid_public_transport" = i12_health_8,
                "Avoid_working_outside" = i12_health_9,
                "Avoid_school" = i12_health_10,
                "Avoid_guests" = i12_health_11,
                "Avoid_small_gatherings" = i12_health_12,
                "Avoid_medium_gatherings" = i12_health_13,
                "Avoid_large_gatherings" = i12_health_14,
                "Avoid_crowds" = i12_health_15,
                "Avoid_shops" = i12_health_16,
                "Separate_Bedrooms" = i12_health_17,
                "Eat_separately" = i12_health_18,
                "Clean_frequently" = i12_health_19,
                "Avoid_touching_objects" = i12_health_20,
                Times_Wash_Hands_Ytd = i13_health,
                Construction = i14_health_1,
                Delivery = i14_health_2,
                Food_Retail = i14_health_3,
                Healthcare = i14_health_4,
                Logistics = i14_health_5,
                Manufacturing = i14_health_6,
                Policing = i14_health_7,
                Public_Transport = i14_health_8,
                School =i14_health_9,
                Social_Care = i14_health_10,
                Others = i14_health_96,
                Not_Sure = i14_health_98,
                Not_Work_Outside = i14_health_99,
                Remarks = i14_health_other,
                Arthritis = d1_health_1,
                Asthma = d1_health_2,
                Cancer = d1_health_3,
                Cystic_Fibrosis = d1_health_4,
                COPD = d1_health_5,
                Diabetes = d1_health_6,
                Epilepsy = d1_health_7,
                Heart_Disease = d1_health_8,
                HBP = d1_health_9,
                High_Cholesterol = d1_health_10,
                HIV = d1_health_11,
                Mental_Health = d1_health_12,
                Sclerosis = d1_health_13,
                Not_Say = d1_health_98,
                No_Conditions = d1_health_99,
                "Willing_to_vacc_immed" = vac_1,
                "Worried_about_Covid" = vac2_1,
                "Worried_about_side_effects_vacc" = vac2_2,
                "Belief_in_Govt" = vac2_3,
                "Belief_vacc_protect_Covid_effects" = vac2_4,
                "Belief_vacc_prevent_transmission" = vac2_5,
                "Vaccine_importance_to_health" = vac4,
                "Trust_in_vacc" = vac7,
                "Vacc_dose" = vac,
                "Vacc_most_trusted" = vac11
            ) %>%
            mutate("Pre-existing_Conditions" = ifelse(
                No_Conditions == "Yes" , "No",
                ifelse(Not_Say == "Yes" | Not_Say == " " ,"Not specified", "Yes")
            )) %>%
            mutate("Month" = get_month(endtime)) %>%
            mutate(Willing_to_vacc_immed = case_when(
                Willing_to_vacc_immed == "1" ~ "Strongly Agree",
                Willing_to_vacc_immed == "2" ~ "Agree",
                Willing_to_vacc_immed == "3" ~ "Neutral",
                Willing_to_vacc_immed == "4" ~ "Disagree",
                Willing_to_vacc_immed == "5" ~ "Strongly Disagree",
                Willing_to_vacc_immed == "1 - Strongly agree" ~ "Strongly Agree",
                Willing_to_vacc_immed == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Willing_to_vacc_immed) ~ "Not specified"
            )) %>%
            dplyr::mutate(Willing_to_vacc_immed = replace_na(Willing_to_vacc_immed, "Not specified"))%>%
            mutate(Worried_about_Covid = case_when(
                Worried_about_Covid == "1" ~ "Strongly Agree",
                Worried_about_Covid == "2" ~ "Agree",
                Worried_about_Covid == "3" ~ "Neutral",
                Worried_about_Covid == "4" ~ "Disagree",
                Worried_about_Covid == "5" ~ "Strongly Disagree",
                Worried_about_Covid == "1 - Strongly agree" ~ "Strongly Agree",
                Worried_about_Covid == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Worried_about_Covid) ~ "Not specified"
            )) %>%
            dplyr::mutate(Worried_about_Covid = replace_na(Worried_about_Covid, "Not specified"))%>%
            mutate(Worried_about_side_effects_vacc = case_when(
                Worried_about_side_effects_vacc == "1" ~ "Strongly Agree",
                Worried_about_side_effects_vacc == "2" ~ "Agree",
                Worried_about_side_effects_vacc == "3" ~ "Neutral",
                Worried_about_side_effects_vacc == "4" ~ "Disagree",
                Worried_about_side_effects_vacc == "5" ~ "Strongly Disagree",
                Worried_about_side_effects_vacc == "1 - Strongly agree" ~ "Strongly Agree",
                Worried_about_side_effects_vacc == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Worried_about_side_effects_vacc) ~ "Not specified"
            )) %>%
            dplyr::mutate(Worried_about_side_effects_vacc = replace_na(Worried_about_side_effects_vacc, "Not specified"))%>%
            mutate(Belief_in_Govt = case_when(
                Belief_in_Govt == "1" ~ "Strongly Agree",
                Belief_in_Govt == "2" ~ "Agree",
                Belief_in_Govt == "3" ~ "Neutral",
                Belief_in_Govt == "4" ~ "Disagree",
                Belief_in_Govt == "5" ~ "Strongly Disagree",
                Belief_in_Govt == "1 - Strongly agree" ~ "Strongly Agree",
                Belief_in_Govt == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Belief_in_Govt) ~ "Not specified"
            )) %>%
            dplyr::mutate(Belief_in_Govt = replace_na(Belief_in_Govt, "Not specified"))%>%
            mutate(Belief_vacc_protect_Covid_effects = case_when(
                Belief_vacc_protect_Covid_effects == "1" ~ "Strongly Agree",
                Belief_vacc_protect_Covid_effects == "2" ~ "Agree",
                Belief_vacc_protect_Covid_effects == "3" ~ "Neutral",
                Belief_vacc_protect_Covid_effects == "4" ~ "Disagree",
                Belief_vacc_protect_Covid_effects == "5" ~ "Strongly Disagree",
                Belief_vacc_protect_Covid_effects == "1 - Strongly agree" ~ "Strongly Agree",
                Belief_vacc_protect_Covid_effects == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Belief_vacc_protect_Covid_effects) ~ "Not specified"
            )) %>%
            dplyr::mutate(Belief_vacc_protect_Covid_effects = replace_na(Belief_vacc_protect_Covid_effects, "Not specified"))%>%
            mutate(Belief_vacc_prevent_transmission = case_when(
                Belief_vacc_prevent_transmission == "1" ~ "Strongly Agree",
                Belief_vacc_prevent_transmission == "2" ~ "Agree",
                Belief_vacc_prevent_transmission == "3" ~ "Neutral",
                Belief_vacc_prevent_transmission == "4" ~ "Disagree",
                Belief_vacc_prevent_transmission == "5" ~ "Strongly Disagree",
                Belief_vacc_prevent_transmission == "1 - Strongly agree" ~ "Strongly Agree",
                Belief_vacc_prevent_transmission == "5 â€“ Strongly disagree" ~ "Strongly Disagree",
                is.na(Belief_vacc_prevent_transmission) ~ "Not specified"
            )) %>%
            dplyr::mutate(Belief_vacc_prevent_transmission = replace_na(Belief_vacc_prevent_transmission, "Not specified"))
        
        singaporedata_cleaned$"Avoid_working_outside"[singaporedata_cleaned$"Avoid_working_outside" ==" "]<-"Not specified"
        singaporedata_cleaned$"Avoid_school"[singaporedata_cleaned$"Avoid_school" ==" "]<-"Not specified" 
        singaporedata_cleaned$"Separate_Bedrooms"[singaporedata_cleaned$"Separate_Bedrooms" ==" "]<-"Not specified" 
        singaporedata_cleaned$"Eat_separately"[singaporedata_cleaned$"Eat_separately" ==" "]<-"Not specified"  
        singaporedata_cleaned$"Clean_frequently"[singaporedata_cleaned$"Clean_frequently" ==" "]<-"Not specified"  
        singaporedata_cleaned$"Avoid_touching_objects"[singaporedata_cleaned$"Avoid_touching_objects" ==" "]<-"Not specified"  
        singaporedata_cleaned$"Willing_to_vacc_immed"[singaporedata_cleaned$"Willing_to_vacc_immed" ==" "]<-"Not specified"  
        singaporedata_cleaned$"Vaccine_importance_to_health"[singaporedata_cleaned$"Vaccine_importance_to_health" == " "] <- "Not specified"
        singaporedata_cleaned$"Trust_in_vacc"[singaporedata_cleaned$"Trust_in_vacc" == " "] <- "Not specified"
        singaporedata_cleaned$"Vacc_dose"[singaporedata_cleaned$"Vacc_dose" == " "] <- "Not specified"
        singaporedata_cleaned$"Vacc_most_trusted"[singaporedata_cleaned$"Vacc_most_trusted" == " "] <- "Not specified"
        singaporedata_cleaned$"Vacc_most_trusted"[singaporedata_cleaned$"Vacc_most_trusted" == "8"] <- "Not specified"
        
        RV$singaporedata_cleaned <- singaporedata_cleaned
    })
    
    output$date_range_slide_UI <- renderUI({
        
        singaporedata_cleaned = req(RV$singaporedata_cleaned)
        
        dateRangeInput('date_range_slide',
                       label = 'Date range input: yyyy-mm-dd',
                       min = min(singaporedata_cleaned$endtime), max = max(singaporedata_cleaned$endtime),
                       start = min(singaporedata_cleaned$endtime), end = max(singaporedata_cleaned$endtime)
        )
        
    })
    
    output$select_number_of_levels_UI <- renderUI({
        selectInput("select_number_of_levels",
                    "Select Number of Levels:",
                    choices = list("Two" = 2,
                                   "Three" = 3, "Four" = 4,
                                   "Five" = 5), selected = 3)
    })
    
    output$Select_level_UI <- renderUI({
        select_level = list()
        select_number_of_levels <- req(input$select_number_of_levels)
        
        for(i in 1:select_number_of_levels){
            
            random = paste0("select_level_", i)
            select_level[[i]] <- selectInput(inputId = random,
                                             paste("Select level ", i),
                                             choices = list("Gender",
                                                            "Age" , "Region" ,
                                                            "Household_Size", "Household_Children" ,
                                                            "Employment_Status", "Pre-existing_Conditions" ,
                                                            "Wear_Mask", "Use_Soap", "Use_Sanitiser" ,
                                                            "Cover_Nose_&_Mouth", "Avoid_people_with_symptoms" , 
                                                            "Avoid_going_out", "Avoid_hospitals",  "Avoid_public_transport",
                                                            "Avoid_working_outside" , "Avoid_school" , "Avoid_guests" ,
                                                            "Avoid_small_gatherings" , "Avoid_medium_gatherings" , 
                                                            "Avoid_large_gatherings" , "Avoid_crowds" , "Avoid_shops" ,
                                                            "Separate_Bedrooms" , "Eat_separately" , "Clean_frequently" ,
                                                            "Avoid_touching_objects","Willing_to_vacc_immed","Worried_about_Covid",
                                                            "Worried_about_side_effects_vacc","Belief_in_Govt","Belief_vacc_protect_Covid_effects",
                                                            "Belief_vacc_prevent_transmission", "Trust_in_vacc", 
                                                            "Vacc_dose", "Vacc_most_trusted"
                                             ), selected = req(RV$singaporedata_cleaned)[,i])
        }
        select_level
    })
    
    output$action_button_UI <- renderUI({
        actionButton("action_button", "Confirm Selection")
    })
    
    observeEvent(input$action_button,{
        singaporedata_cleaned = req(RV$singaporedata_cleaned)
        date_range_slide = req(input$date_range_slide)
        
        select_number_of_levels <- req(input$select_number_of_levels)
        selected_columns = paste0("select_level_", 1:select_number_of_levels)
        selected_columns <- sapply(selected_columns, function(x) input[[x]])
        
        RV$df_test <- singaporedata_cleaned[singaporedata_cleaned$endtime >= date_range_slide[1] &
                                                singaporedata_cleaned$endtime <= date_range_slide[2],selected_columns]
    })
    
    observe({
        df_test <<- req(RV$df_test)
        output$parset <- renderParset(
            expr = parset(data = df_test, dimensions = colnames(df_test) ,tension = 0.5),
            env = parent.frame()
        )
    })
    
    
     
    output$dist <- renderPlot({
        
        singaporedata = read.csv("data/singapore.csv")
        #singaporedata$endtime <-  as.Date(as.POSIXct(singaporedata$endtime, format = "%d/%m/%Y %H:%M"))
        singaporedata_cleaned <- separate(singaporedata,
                                          region,
                                          into = c("Region","Town"),
                                          sep = c("-"),
                                          remove = TRUE,
                                          convert = FALSE,
                                          extra = "warn",
                                          fill = "warn")
        
        singaporedata_cleaned <- singaporedata_cleaned %>%
            rename(
                Gender = gender,
                Age = age,
                Household_Size = household_size,
                Household_Children = household_children,
                Employment_Status = employment_status,
                Household_Contact = i1_health,
                Non_Household_Contact = i2_health,
                Times_Left_House = i7a_health,
                Tested_Self = i3_health,
                Tested_Household = i4_health,
                Dry_Cough = i5_health_1,
                Fever = i5_health_2,
                Loss_Of_Smell = i5_health_3,
                Loss_Of_Taste = i5_health_4,
                Shortness_Of_Breath = i5_health_5,
                None_Of_Above = i5_health_99,
                Confirmed_Case_Contact = i5a_health,
                Self_Isolate_Symptom = i6_health,
                Visit_Doctor = i7b_health,
                Travelled = i8_health,
                Self_Isolate_Plan = i9_health,
                Self_Isolate_Difficulty = i10_health,
                Self_Isolate_Willingness = i11_health,
                "Wear_Mask" = i12_health_1,
                "Use_Soap" = i12_health_2,
                "Use_Sanitiser" = i12_health_3,
                "Cover_Nose_&_Mouth" = i12_health_4,
                "Avoid_people_with_symptoms" = i12_health_5,
                "Avoid_going_out" = i12_health_6,
                "Avoid_hospitals" = i12_health_7,
                "Avoid_public_transport" = i12_health_8,
                "Avoid_working_outside" = i12_health_9,
                "Avoid_school" = i12_health_10,
                "Avoid_guests" = i12_health_11,
                "Avoid_small_gatherings" = i12_health_12,
                "Avoid_medium_gatherings" = i12_health_13,
                "Avoid_large_gatherings" = i12_health_14,
                "Avoid_crowds" = i12_health_15,
                "Avoid_shops" = i12_health_16,
                "Separate_Bedrooms" = i12_health_17,
                "Eat_separately" = i12_health_18,
                "Clean_frequently" = i12_health_19,
                "Avoid_touching_objects" = i12_health_20,
                Times_Wash_Hands_Ytd = i13_health,
                Construction = i14_health_1,
                Delivery = i14_health_2,
                Food_Retail = i14_health_3,
                Healthcare = i14_health_4,
                Logistics = i14_health_5,
                Manufacturing = i14_health_6,
                Policing = i14_health_7,
                Public_Transport = i14_health_8,
                School =i14_health_9,
                Social_Care = i14_health_10,
                Others = i14_health_96,
                Not_Sure = i14_health_98,
                Not_Work_Outside = i14_health_99,
                Remarks = i14_health_other,
                Arthritis = d1_health_1,
                Asthma = d1_health_2,
                Cancer = d1_health_3,
                Cystic_Fibrosis = d1_health_4,
                COPD = d1_health_5,
                Diabetes = d1_health_6,
                Epilepsy = d1_health_7,
                Heart_Disease = d1_health_8,
                HBP = d1_health_9,
                High_Cholesterol = d1_health_10,
                HIV = d1_health_11,
                Mental_Health = d1_health_12,
                Sclerosis = d1_health_13,
                Not_Say = d1_health_98,
                No_Conditions = d1_health_99,
                Willing_to_vacc_immed = vac_1,
                Willing_to_vacc_2021 = vac_2,
                Worried_about_Covid = vac2_1,
                Worried_about_side_effects_vacc = vac2_2,
                Belief_in_Govt = vac2_3,
                Belief_vacc_protect_Covid_effects = vac2_4,
                Belief_vacc_prevent_transmission = vac2_5,
                Willing_to_vacc_year = vac_3
            ) 
        labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                        sep = "-"), paste(100, "+", sep = ""))
        labs
        
        singaporedata_cleaned$Age_Group <- cut(singaporedata_cleaned$Age, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)
        
        singaporedata_cleaned[singaporedata_cleaned == "1 - Strongly agree"] <- 1
        singaporedata_cleaned[singaporedata_cleaned == "5 â€“ Strongly disagree"] <- 5
        
        singaporedata_drop<- singaporedata_cleaned[!(is.na(singaporedata_cleaned$Willing_to_vacc_immed) | singaporedata_cleaned$Willing_to_vacc_immed==" "),]
        singaporedata_drop1 <- singaporedata_drop[!(is.na(singaporedata_drop$Willing_to_vacc_2021) | singaporedata_drop$Willing_to_vacc_2021 ==" "),]
        
        
        
        #singaporedata_cleaned$Willing_to_vacc_immed <- as.factor(singaporedata_cleaned$Willing_to_vacc_immed)
        singaporedata_filter <- singaporedata_drop1 %>%
            select(Age_Group,Willng_to_vacc_immed) %>%
            group_by(Age_Group) 
        
        singaporedata_filter
        
        singaporedata_group <- singaporedata_filter %>%
            group_by(Age_Group) %>%
            summarize(mean = mean(as.numeric(Willng_to_vacc_immed)), sd = sd(Willng_to_vacc_immed))
        
        
        singaporedata_dist <- ggplot(singaporedata_group, aes(y=Age_Group,dist = "norm",arg1 = mean, arg2 = sd, fill = Age_Group)) +
            stat_dist_halfeye(position = "dodge") +
            ggtitle("stat_dist_eye(position = 'dodge')")
        
        singaporedata_dist
        
    })
    


    
}


# Run the application 
shinyApp(ui = ui, server = server)
