#Data Cleaning
SGdata <- read_csv("data/singapore.csv")

#Only first 78 columns are useful, the rest are empty



SGdata_cleaned <- separate(SGdata,
                           region,
                           into = c("Region","Town"),
                           sep = c("-"),
                           remove = TRUE,
                           convert = FALSE,
                           extra = "warn",
                           fill = "warn")

SGdata_cleaned <- SGdata_cleaned %>%
  rename(
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
    Soap = i12_health_2,
    Sanitiser = i12_health_3,
    Cover_Nose_Mouth = i12_health_4,
    Avoid_Symptoms = i12_health_5,
    Avoid_Going_Out = i12_health_6,
    Avoid_Hospital = i12_health_7,
    Avoid_Public_Transport = i12_health_8,
    Avoid_Working_Outside = i12_health_9,
    Avoid_School = i12_health_10,
    Avoid_Guests = i12_health_11,
    Avoid_Small_Gathering = i12_health_12,
    Avoid_Medium_Gathering = i12_health_13,
    Avoid_Large_Gathering = i12_health_14,
    Avoid_Crowd = i12_health_15,
    Avoid_Shops = i12_health_16,
    Separate_Bedroom = i12_health_17,
    Eat_Separately = i12_health_18,
    Clean_Frequently = i12_health_19,
    Avoid_Touching_Objects = i12_health_20,
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
    "Vac_Week" = vac_1,
    "Vac_2021" = vac_2,
    Covid_Worried = vac2_1,
    Vac_Worried = vac2_2,
    Vac_Govt_Effective = vac2_3,
    Vac_Regret = vac2_6,
    Vac_Year = vac_3,
    
    
  )



#SGdata_cleaned[SGdata_cleaned == "1 - Strongly agree"] <- 1
#SGdata_cleaned[SGdata_cleaned == "5 – Strongly disagree"] <- 5

labs <- c(paste(seq(0, 95, by = 5), seq(0 + 5 - 1, 100 - 1, by = 5),
                sep = "-"), paste(100, "+", sep = ""))
labs

SGdata_cleaned$Age_Group <- cut(SGdata_cleaned$age, breaks = c(seq(0, 100, by = 5), Inf), labels = labs, right = FALSE)


SGdata_cleaned <- separate(SGdata_cleaned,
                           Town,
                           into = c("Town1","Town2"),
                           sep = c(", "),
                           remove = TRUE,
                           convert = FALSE,
                           extra = "warn",
                           fill = "warn")

SGdata_cleaned <-
  mutate_at(SGdata_cleaned,"Town1",str_replace, "AMK", "Ang Mo Kio")

SGdata_cleaned$Town = toupper(SGdata_cleaned$Town1)

SGdata_cleaned %>% drop_na(Vac_Week)

SGfilter <- SGdata_cleaned %>%
  select(Vac_Week) 

SGdata_tested <- SGdata_cleaned %>%
  group_by(Town,Tested_Self) %>%
  summarize(count = n())


mpsz <- st_read(dsn = "map",
                layer = "MP14_PLNG_AREA_WEB_PL")

mpsz %>% mutate(PLN_AREA_N = trimws(as.character(PLN_AREA_N)))

SGdata_cleaned <- SGdata_cleaned %>% mutate(Town = trimws(as.character(Town)))

SGdata_tested <-  SGdata_tested %>% mutate(Town = trimws(as.character(Town)))

mpsz_join<- left_join(mpsz,SGdata_tested,by=c("PLN_AREA_N"="Town"))

mpsz_filter <-
  mpsz_join %>%
  filter()







