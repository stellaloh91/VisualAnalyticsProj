
singaporedata = read.csv("data/singapore.csv")

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

RV$singaporedata_cleaned <- singaporedata_cleaned