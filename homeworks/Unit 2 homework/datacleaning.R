library(tidyverse)

diabetic_data <- read.csv("~/Github/dkustats101spr2026s4/homeworks/Unit 2 homework/diabetes+130-us+hospitals+for+years+1999-2008/diabetic_data.csv")

diabetic_data <- diabetic_data %>% 
  filter(weight!="?") %>% 
  mutate(age_grp = case_when(age == "[0-10)" ~ 1,
                             age == "[10-20)" ~ 2,  
                             age == "[20-30)" ~ 3,  
                             age == "[30-40)" ~ 4,  
                             age == "[40-50)" ~ 5,  
                             age == "[50-60)" ~ 6,  
                             age == "[60-70)" ~ 7,
                             age == "[70-80)" ~ 8,
                             age == "[80-90)" ~ 9,
                             age == "[90-100)" ~ 10)) %>% 
  mutate(wgt_grp = case_when(weight == "[0-25)" ~ 1,
                             weight == "[25-50)" ~ 2,
                             weight == "[50-75)" ~ 3,
                             weight == "[75-100)" ~ 4,
                             weight == "[100-125)" ~ 5,
                             weight == "[125-150)" ~ 6, 
                             weight == "[150-175)" ~ 7,
                             weight == "[175-200)" ~ 8,
                             weight == ">200" ~ 9))


ggplot(diabetic_data, aes(wgt_grp, num_procedures)) +
  geom_jitter() +
  geom_smooth(method="lm")