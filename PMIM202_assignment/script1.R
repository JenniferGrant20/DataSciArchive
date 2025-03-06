## PMIM402J Script 1
## Data preparation and value extraction for Table 1
## E.g. cleaning variables and accounting for missing data through MICE
## Student no. 2005070
## Date: 18/06/24

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse) # for data handling / code syntax
library(mice) # for imputation

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data prep 
## Opening the data files
data <- read.csv('Born in Wales Data June 2024.csv',  na.strings=c(""," ","NA"))
followup <- read.csv('Born in Wales Follow-up Data June 2024.csv')

## Renaming all variables 
data <- data %>% rename(id = SYSTEM_ID, 
                        start = Start.time, 
                        exp_deliv_date = Expected.date.of.delivery.of.your.baby,
                        language = What.is.the.main.language.spoken.in.your.home.,
                        ethnicity = What.is.your.ethnic.group.,
                        nationality = Nationality, 
                        rltnship_status = What.is.your.current.relationship.status..,
                        sexuality = Would.you.consider.yourself.to.be.,
                        covid_symptoms = Have.you.had.symptoms.that.are.associated.with.COVID19..fever..dry.cough..loss.of.taste.or.smell..fatigue..muscle.pain..,
                        symptoms_date = When.did.your.symptoms.start..approximately..,
                        symptoms_type = What.symptoms.did.you.have.,
                        covid_treatment = What.treatment.did.you.have..,
                        covid_test = Have.you.had.a.test.,
                        low_mood = Have.you.experienced.low.mood.during.your.pregnancy.,
                        interest_pleasure = Little.interest.or.pleasure.in.doing.things.,
                        depressed_hopeless = Feeling.down..depressed..or.hopeless.,
                        sleep_problems = Trouble.falling.or.staying.asleep..or.sleeping.too.much,
                        tired_energy = Feeling.tired.or.having.little.energy.,
                        appetite = Poor.appetite.or.overeating.,
                        feel_bad = Feeling.bad.about.yourself.or.that.you.are.a.failure.of.have.let.yourself.or.family.down.,
                        trouble_concentrating = Trouble.concentrating.on.things..such.as.reading.the.newspaper.or.watching.television.,
                        lethargy_restlessness = Moving.or.speaking.so.slowly.that.other.people.could.have.noticed..Or.the.opposite.being.so.fidgety.or.restless.that.you.have.been.moving.around.a.lot.more.than.usual.,
                        selfharm_thoughts = Thoughts.that.you.would.be.better.off.dead..or.of.hurting.yourself.in.some.way.,
                        smoking = Do.you.smoke.,
                        alcohol = Do.you.drink.alcohol.,
                        nervous_anxious = Feeling.nervous..anxious.or.on.edge.,
                        uncontrolled_worrying = Not.being.able.to.stop.or.control.worrying.,
                        worrying = Worrying.too.much.about.different.things.,
                        trouble_relaxing = Trouble.relaxing.,
                        restlessness = Being.so.restless.that.it.is.hard.to.sit.still.,
                        annoyed_irritable = Becoming.easily.annoyed.or.irritable.,
                        fear = Feeling.afraid.as.if.something.awful.might.happen.,
                        work_physical_activity = Please.tell.us.the.type.and.amount.of.physical.activity.involved.in.your.work,
                        physical_exercise = Physical.exercise.such.as.swimming..jogging..aerobics..football..tennis..gym.workout.etc.,
                        cycling = Cycling..including.cycling.to.work.and.during.leisure.time,
                        walking = Walking..including.walking.to.work..shopping..for.pleasure.etc.,
                        housework_childcare = Housework.Childcare,
                        gardening_diy = Gardening.DIY,
                        walking_pace = How.would.you.describe.your.usual.walking.pace...Please.mark.one.box.only.,
                        prepreg_wt_kg = My.weight..before.pregnancy..in.Kg.is.,
                        height_cm = My.height.in.centimetres.is..,
                        bad_stress = Have.you.had.any.periods.of.bad.stress.in.your.pregnancy..,
                        rltnship_difficulties = any.serious.relationship.difficulties.with.your.husband.or.partner.or.separated.or.divorced.,
                        legal_financial_problems = any.serious.legal.or.financial.problems.,
                        violence_crime = were.you.or.someone.close.to.you.a.victim.of.violence.or.crime.,
                        serious_illness = someone.close.with.a.serious.illness,
                        close_death = the.death.of.someone.close.to.you..,
                        covid_stress = was.this.stressful.event.related.to.coronavirus.,
                        emotional_financial_support = During.this.time.did.you.have.someone.who.could.support.you.emotional.or.financially,
                        stress_scale = X0n.a.scale.of.1.to.10..how.stressful.was.this.time...1.is.not.at.all..10.is.overwhelming..,
                        healthcare_satisfaction = I.feel.satisfied.with.the.support.and.care.I.have.received.in.my.pregnancy.from.my.health.care.team..1.is.strongly.disagree..5.is.strongly.agree.,
                        covid_changed_birth_type = Has.COVID19.change.the.type.of.birth.you.feel.you.will.have.,
                        maternity_care_type = What.type.of.maternity.care.are.you.receiving.now.,
                        support_experience = How.would.you.describe.your.experience.of.this.pregnancy..support.from.midwife..how.you.feel.about.being.pregnant..,
                        intention_to_feed = How.are.you.planning.to.feed.your.baby.,
                        other_children = Do.you.have.other.children..,
                        num_children = Number.of.children,
                        children_ages = How.old.are.your.other.children..please.separate.each.child.s.age.by.a.comma...E.g...2..4.and.8,
                        occupation = What.is..your.occupation..,
                        education = What.is.the.highest.level.of.education.you.have.reached.,
                        working = Are.you.currently.working.,
                        working_hrs = Do.you.work,
                        household_num_ppl = How.many.people.live.in.your.home..not.including.you..,
                        household_income = What.is.the.number.that.best.describes.your.TOTAL.household.income.BEFORE.TAX.,
                        inperson_verbal_contact = How.many.people..who.are.not.part.of.your.household..did.you.talk.to.in.person.yesterday..e.g..were.within.1.metre.and.exchanged.words.but.did.not.touch...,
                        inperson_physical_contact = How.many.people..who.are.not.part.of.your.household..did.you.have.direct.physical.contact.with.yesterday..hugged..touched..,
                        prev2_accuracy = Do.you.feel.you.were.able.to.answer.the.last.two.questions.above.accurately.,
                        wimd_2019_rank = WIMD.2019.Rank,
                        wimd_2019_decile = WIMD.2019.Decile,
                        wimd_2019_quintile = WIMD.2019.Quintile,
                        wimd_2019_quartile = WIMD.2019.Quartile,
                        covid_vaccine = What.is.your.view.on.having.the.COVID.vaccination.in.pregnancy..have.you.or.would.you.have.the.COVID.vaccination.when.pregnant.and.why.,
                        extra_comments = PLEASE.PRESS.SUBMIT.AT.THE.BOTTOM..Thank.you.very.much.for.completing.this.questionnaire..If.you.feel.there.is.something.that.we.should.have.asked.or.something.you.were.not.able.to.tell.us.please....
                        )

## Formatting the date variables as dates
data$start <- as.Date(data$start, "%d/%m/%y")
data$exp_deliv_date <- as.Date(data$exp_deliv_date, "%d/%m/%y")

## Overview of the initial data
md.pattern(data) 
summary(data)

# Removing 7 individuals who only answered the first 3 questions, 2 individuals 
# who were missing intention to feed, and the final 5 variables which have 
# 714-716 NAs

sum(is.na(data[,4]))             # Finding those who answered only the first 
data %>% filter(is.na(data[,4])) # few questions (8 individuals but 1 of these
                                 # did answer subsequent questions)

data <- data %>% 
  filter(!id == 40 & !id == 601 & !id == 667 & !id == 686 &
         !id == 688 & !id == 697 & !id ==  708 & !is.na(intention_to_feed)) %>%
  select(-wimd_2019_rank, -wimd_2019_decile, -wimd_2019_quintile, 
         -wimd_2019_quartile, -extra_comments)

cleandata <- data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Recoding variables of interest: smoking, education, low mood, relationship
## status, having other children, economic status, anxiety.

## Recoding relationship status (0 = single, 1 = dating/engaged, 2 = married/
## living with partner)

cleandata$rltnship_status <- recode(data$rltnship_status, "Dating" = 1, 
                                "Dating " = 1, "Engaged" = 1,  "Engaged " = 1, 
                                "Living with partner/civil partnership" = 2,
                                "Married" = 2, "Partner- lives in army accommodation and at home when off " = 2,
                                "Separated" = 0, "Single" = 0, "Prefer not to say" = 3)
cleandata$rltnship_status <- na_if(cleandata$rltnship_status, 3)

cleandata$rltnship_status <- factor(cleandata$rltnship_status,
                                    levels = c(0, 1, 2),
                                    labels = c("Single", "Dating/engaged", 
                                               "Married/partnered"))

## Recoding smoking (0 = no smoking history, 1 = history of smoking)

cleandata$smoking <- recode(data$smoking, "No, never smoked" = 0,
                             "I have smoked but occasionally when younger. " = 1,
                             "I smoke aroung 15 a day" = 1, "I smoke e-cigarettes" = 1,
                             "I used but I stopped before I was pregnant" = 1, 
                             "I used to but I stopped when I knew I was pregnant" = 1,
                             "I used to, but I stopped before I was pregnant" = 1,
                             "I used to, but I stopped when I knew I was pregnant" = 1,
                             "Quit 5 years ago" = 1, "Stopped smoking 17 years ago. " = 1,
                             "Used to casually smoke when drinking but not for many years" = 1,
                             "Yes I smoke, less than 5 cigarettes a day" = 1,
                             "Yes I smoke, less than 5 cigarettes a week" = 1,
                             "Yes I smoke, more than 5 cigarettes a week" = 1)

cleandata$smoking <- factor(cleandata$smoking,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

## Recoding education (0 = none, 1 = GCSE or equiv., 2 = ALevel or equiv.,
## 3 = diploma or equiv., 4 = undergrad degree, 5 = masters degree, 6 = PhD)

cleandata$education <- recode(data$education, "None" = 0, "None " = 0,
                               "Exams at age 16 (GCSE or equivalent)" = 1,
                               "Exams at age 18 (A level or equivalent)" = 2,
                               "Currently studying degree " = 2, 
                               "Currently studying Nursing Degree" = 2,
                               "NVQ via work" = 2, "2 nvqs " = 2, "Dip he" = 3,
                               "DipHE" = 3, "Diploma " = 3, 
                               "Diploma of Higher Education" = 3, 
                               "Diploma of higher education " = 3, 
                               "Higher national diploma " = 3, "CACHE L5" = 3,
                               "Vocational qualifications" = 3,
                               "University degree" = 4, 
                               "University higher degree" = 4,
                               "Bachelor of Science in nursing. Qualified nurse in the Philippines and UK" = 4,
                               "Masters" = 5, "Master?s Degree " = 5, 
                               "PGCE" = 5, "Post grad" = 5, "Phd" = 6, "PhD" = 6,
                               "Clinical doctorate" = 6, "Doctorate" = 6)

cleandata$education <- factor(cleandata$education,
                                    levels = c(0, 1, 2, 3, 4, 5, 6),
                                    labels = c("None", "GCSE or equivalent", 
                                               "A-level or equivalent", 
                                               "Diploma or equivalent", 
                                               "Undergraduate degree", 
                                               "Master's degree or equivalent",
                                               "PhD or equivalent"))

## Recoding having other children (0 = no previous children/primiparous, 
## 1 = has previous children/multiparous)

cleandata$other_children <- recode(data$other_children, "No" = 0,
                                    "Yes" = 1, "Prefer not to say" = 2)
cleandata$other_children <- na_if(cleandata$other_children, 2)

cleandata$other_children <- factor(cleandata$other_children,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

## Recoding economic hardship (increases in increments of 10k, 0 being less than 
## 10k and 5 being more than 50k)

cleandata$household_income <- recode(data$household_income,
                                      "Less than \x9c10,000" = 0, 
                                      "Less than \x86\x9c10,000" = 0,
                                      "Between \x9c10,000-\x9c19,999" = 1,
                                      "Between \x86\x9c10,000-\x86\x9c19,999" = 1,
                                      "Between \x9c20,000-\x9c29,999" = 2,
                                      "Between \x86\x9c20,000-\x86\x9c29,999" = 2,
                                      "Between \x9c30,000-\x9c39,999" = 3,
                                      "Between \x86\x9c30,000-\x86\x9c39,999" = 3,
                                      "Between \x86\x9c40,000-\x86\x9c49,999" = 4,
                                      "Between \x9c40,000-\x9c49,999" = 4,
                                      "\x9c50,000 +" = 5, "\x86\x9c50,000 +" = 5,
                                      "150,000" = 5, "\x9c200,000" = 5,
                                      "Both me and my husband are nurses earning \x9c31,534 each (before tax) yearly" = 5,
                                      "Monthly less than \x9c5k" = 5,
                                      "Perfer not to say" = 6, "Prefer not to say" = 6)
cleandata$household_income <- na_if(cleandata$household_income, 6)

cleandata$household_income <- factor(cleandata$household_income,
                                    levels = c(0, 1, 2, 3, 4, 5),
                                    labels = c("Less than £10,000", 
                                               "Between £10k and £20k", 
                                               "Between £20k and £30k",
                                               "Between £30k and £40k",
                                               "Between £40k and £50k",
                                               "More than £50,000"))

## Recoding low mood (0 = no low mood, 1 = low mood) and PHQ-9 variables

cleandata$low_mood <- recode(data$low_mood, "Yes" = 1, "No" = 0)

cleandata$interest_pleasure <- recode(data$interest_pleasure, 
                                      "Not at all" = 0, "Several days" = 1, 
                                      "More than half the days" = 2, 
                                      "Nearly every day" = 3, 
                                      "Nearly everyday" = 3)

cleandata$depressed_hopeless <- recode(data$depressed_hopeless, 
                                       "Not at all" = 0, "Several days" = 1, 
                                       "More than half the days" = 2, 
                                       "Nearly every day" = 3, 
                                       "Nearly everyday" = 3)

cleandata$sleep_problems <- recode(data$sleep_problems, 
                                  "Not at all" = 0, "Several days" = 1, 
                                  "More than half the days" = 2, 
                                  "Nearly every day" = 3, 
                                  "Nearly everyday" = 3)

cleandata$tired_energy <- recode(data$tired_energy, 
                                 "Not at all" = 0, "Several days" = 1, 
                                 "More than half the days" = 2, 
                                 "Nearly every day" = 3, 
                                 "Nearly everyday" = 3)

cleandata$appetite <- recode(data$appetite, 
                            "Not at all" = 0, "Several days" = 1, 
                            "More than half the days" = 2, 
                            "Nearly every day" = 3, "Nearly everyday" = 3)

cleandata$feel_bad <- recode(data$feel_bad, 
                             "Not at all" = 0, "Several days" = 1, 
                             "More than half the days" = 2, 
                             "Nearly every day" = 3, 
                             "Nearly everyday" = 3)

cleandata$trouble_concentrating <- recode(data$trouble_concentrating, 
                                          "Not at all" = 0, "Several days" = 1, 
                                          "More than half the days" = 2, 
                                          "Nearly every day" = 3, 
                                          "Nearly everyday" = 3)

cleandata$lethargy_restlessness <- recode(data$lethargy_restlessness, 
                                          "Not at all" = 0, "Several days" = 1, 
                                          "More than half the days" = 2, 
                                          "Nearly every day" = 3, 
                                          "Nearly everyday" = 3)

cleandata$selfharm_thoughts <- recode(data$selfharm_thoughts, 
                                      "Not at all" = 0, "Several days" = 1, 
                                      "More than half the days" = 2, 
                                      "Nearly every day" = 3, 
                                      "Nearly everyday" = 3)

cleandata <- cleandata %>%  # coding PHQ-9 NAs as 0 if low mood was 0
  mutate(interest_pleasure = ifelse(low_mood == 0, 0, interest_pleasure)) %>%
  mutate(depressed_hopeless = ifelse(low_mood == 0, 0, depressed_hopeless)) %>%
  mutate(sleep_problems = ifelse(low_mood == 0, 0, sleep_problems)) %>%
  mutate(tired_energy = ifelse(low_mood == 0, 0, tired_energy)) %>%
  mutate(appetite = ifelse(low_mood == 0, 0, appetite)) %>%
  mutate(feel_bad = ifelse(low_mood == 0, 0, feel_bad)) %>%
  mutate(trouble_concentrating = ifelse(low_mood == 0, 0, trouble_concentrating)) %>%
  mutate(lethargy_restlessness = ifelse(low_mood == 0, 0, lethargy_restlessness)) %>%
  mutate(selfharm_thoughts = ifelse(low_mood == 0, 0, selfharm_thoughts))
  

## Recoding intention to breastfeed (0 = not intending to breastfeed,
## 1 = intending to breastfeed in some capacity)

cleandata$intention_to_feed <- recode(data$intention_to_feed,
                                      "Don't know yet" = 0, "Dont know yet" = 0,
                                      "Bottle milk only" = 0, 
                                      "Breast and bottle" = 1, 
                                      "Breast and expressed milk via bottle" = 1,
                                      "Breast milk only" = 1, 
                                      "Breast if possible " = 1, 
                                      "Breast if all well. If not fed is best either way " = 1,
                                      "Planned breastfeeding, ended up formula feeding" = 1)

cleandata$intention_to_feed <- factor(cleandata$intention_to_feed,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"))

## Recoding GAD-7 anxiety variables

cleandata$nervous_anxious <- recode(data$nervous_anxious,
                                    "Not at all" = 0, "Several days" = 1, 
                                    "More than half the days" = 2, 
                                    "Nearly everyday" = 3)

cleandata$uncontrolled_worrying <- recode(data$uncontrolled_worrying,
                                          "Not at all" = 0, 
                                          "Several days" = 1, 
                                          "More than half the days" = 2, 
                                          "Nearly everyday" = 3)

cleandata$worrying <- recode(data$worrying,
                             "Not at all" = 0, "Several days" = 1, 
                             "More than half the days" = 2, 
                             "Nearly everyday" = 3)

cleandata$trouble_relaxing <- recode(data$trouble_relaxing,
                                     "Not at all" = 0, "Several days" = 1, 
                                     "More than half the days" = 2, 
                                     "Nearly everyday" = 3)

cleandata$restlessness <- recode(data$restlessness,
                                 "Not at all" = 0, "Several days" = 1, 
                                 "More than half the days" = 2, 
                                 "Nearly everyday" = 3)

cleandata$annoyed_irritable <- recode(data$annoyed_irritable,
                                      "Not at all" = 0, "Several days" = 1, 
                                      "More than half the days" = 2, 
                                      "Nearly everyday" = 3)

cleandata$fear <- recode(data$fear,
                         "Not at all" = 0, "Several days" = 1, 
                         "More than half the days" = 2, 
                         "Nearly everyday" = 3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Recoding demographic variables

cleandata$language <- recode(data$language,
                             "English" = 1, "Welsh" = 2, 
                             "Bilingual Wel/Eng" = 3, 
                             "Bilingual Welsh and English " = 3,
                             "Equal welsh/English" = 3, "English and Welsh" = 3,
                             "Greek" = 4, "Italian" = 4, "Polish" = 4, 
                             "Portuguese " = 4, "Spanish " = 4, "Swahili" = 4,
                             "Waray waray" = 4)

cleandata <- cleandata %>% mutate(language = ifelse(is.na(language), 0, language))

cleandata$ethnicity <- recode(data$ethnicity, 
                              "Asian" = 1, "Chinese" = 1, "Mixed" = 2,
                              "White" = 3, "Prefer not to say" = 0)
cleandata <- cleandata %>% mutate(ethnicity = ifelse(is.na(ethnicity), 0, ethnicity))

cleandata$nationality <- recode(data$nationality, "Prefer not to say" = 0,
                                "Welsh " = 1, "Welsh" = 1, 
                                "British" = 2, "Dual British and other nationality" = 2,
                                "European" = 3, "Irish" = 3, "Greek" = 3,
                                "American" = 4, "Brazilian" = 5, "Indian" = 6,
                                "Filipino" = 6, "New Zealand" = 7)

cleandata$sexuality <- recode(data$sexuality, "Prefer not to say" = 0, 
                              "Heterosexual or straight?" = 1, 
                              "Homosexual or lesbian?" = 2,
                              "Bisexual?" = 3, "Pansexual" = 3)
cleandata <- cleandata %>% mutate(sexuality = ifelse(is.na(sexuality), 0, sexuality))

## Removing COVID related variables as they are not of interest 
cleandata <- cleandata %>% select(-covid_symptoms, -covid_treatment, - covid_test,
                                  -covid_stress, -covid_changed_birth_type, 
                                  -covid_vaccine, -symptoms_date, -symptoms_type)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Recoding other variables useful for mice imputation

## Relationship difficulties 

cleandata$bad_stress <- recode(data$bad_stress,
                               "No" = 0, "Yes" = 1, "Prefer not to say" = 2)

cleandata$rltnship_difficulties <- recode(data$rltnship_difficulties,
                                          "No" = 1, "No " = 1, "Yes" = 2)

cleandata <- cleandata %>% 
  mutate(rltnship_difficulties = ifelse(bad_stress == 0, 
                                        0, rltnship_difficulties))

## Physical activity 

cleandata$work_physical_activity <- recode(data$work_physical_activity,
                                           "I am not in employment (e.g. retired, retired for health reasons, unemployed, full-time carer etc.)" = 0,
                                           "I am not in employment (e.g. stay at home parent, unemployed, full-time carer etc.)" = 0,
                                           "I spend most of my time at work sitting (such as in an office)" = 1,
                                           "I spend most of my time at work standing or walking. However, my work does not require much intense physical effort (e.g. shop assistant, hairdresser, security guard, childminder, etc.)" = 2,
                                           "My work involves definite physical effort including handling of heavy objects and use of tools (e.g. cleaner, hospital nurse, gardener, postal delivery workers etc.)" = 3,
                                           "My work involves vigorous physical activity including handling of very heavy objects (e.g. construction worker, refuse collector, etc.)" = 4)

cleandata$physical_exercise <- recode(data$physical_exercise,
                                      "None" = 0, 
                                      "Some but less than 1 hour" = 1,
                                      "1 hour but less than 3 hours" = 2,
                                      "3 hours or more" = 3)

cleandata$cycling <- recode(data$cycling,
                            "None" = 0, 
                            "Some but less than 1 hour" = 1,
                            "1 hour but less than 3 hours" = 2,
                            "3 hours or more" = 3)

cleandata$walking <- recode(data$walking,
                            "None" = 0, 
                            "Some but less than 1 hour" = 1,
                            "1 hour but less than 3 hours" = 2,
                            "3 hours or more" = 3)

cleandata$housework_childcare <- recode(data$housework_childcare,
                                        "None" = 0, 
                                        "Some but less than 1 hour" = 1,
                                        "1 hour but less than 3 hours" = 2,
                                        "3 hours or more" = 3)

cleandata$gardening_diy <- recode(data$gardening_diy,
                                  "None" = 0, 
                                  "Some but less than 1 hour" = 1,
                                  "1 hour but less than 3 hours" = 2,
                                  "3 hours or more" = 3)

cleandata$walking_pace <- recode(data$walking_pace,
                                 "Slow pace" = 0,
                                 "Steady average pace" = 1,
                                 "Brisk pace" = 2,
                                 "Fast pace" = 3)

## Work

cleandata$working <- recode(data$working,
                            "No, I am unemployed" = 0,
                            "No, I can not do my work in lockdown (furlouged or business closed in lockdown)" = 0,
                            "No, I am a student" = 1,
                            "No, I am a stay at home parent" = 2,
                            "No I am on maternity leave now" = 3,
                            "Yes, I am working from home" = 4,
                            "Yes, working outside the home/ in the office" = 5,
                            "Yes, I am a key worker so working outside the home" = 5)

## Alcohol

cleandata$alcohol <- recode(data$alcohol,
                            "Hardly drant prior to pregnancy. Had none during pregnancy " = 2,
                            "I only drink once or twice a year, I doesn't appeal to me but if I'm in the mood I would drink when not pregnant " = 2,
                            "Just on the odd night out, hardly anything" = 3,
                            "Never during pregnant but never a massive drinker anway " = 2,
                            "No, have in the past" = 1,
                            "No, I have never drunk alcohol" = 0,
                            "Not for a few years" = 1,
                            "Occasionally before pregnancy" = 2,
                            "Occasionally before pregancy" = 2,
                            "On very few occasions, but not since I have been pregnant " = 2,
                            "Only on special occasions" = 3,
                            "Rarely, perhaps 3-4 times a year" = 3,
                            "Very rarely before pregnancy" = 2,
                            "Yes but maybe 2/3 a year" = 3,
                            "Yes, about once per week" = 4,
                            "Yes, but I stopped before I was pregnant" = 2,
                            "Yes, I stopped as soon as I knew I was pregant" = 2,
                            "Yes, I stopped as soon as I knew I was pregnant" = 2,
                            "Yes, very occasionally now" = 3)

## Removing remaining variables that aren't useful 

cleandata <- cleandata %>% 
  select(-legal_financial_problems, - violence_crime, -serious_illness, 
         -close_death, -emotional_financial_support, -support_experience, 
         -occupation, -maternity_care_type, -num_children, -children_ages, 
         -prev2_accuracy, -working_hrs, -stress_scale, -inperson_verbal_contact,
         -inperson_physical_contact) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Imputation

md.pattern(cleandata) # Looking at the missing data
summary(cleandata)
imputedata <- cleandata %>% select(-start, -exp_deliv_date)
imputedata <- mice(imputedata, m=5, method='pmm')
imputedata <- complete(imputedata)
summary(imputedata) # Checking the data looks okay after imputation
summary(cleandata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Congregating depression and anxiety-related variables into only two variables

## Creating PHQ-9 score by summing PHQ-9 related variables
imputedata$phq9_score <- imputedata$interest_pleasure + 
  imputedata$depressed_hopeless + imputedata$sleep_problems + 
  imputedata$tired_energy + imputedata$appetite +
  imputedata$feel_bad + imputedata$trouble_concentrating +
  imputedata$lethargy_restlessness + imputedata$selfharm_thoughts

## Converting PHQ-9 score to a severity category
imputedata$depression_severity <- recode(imputedata$phq9_score,
                                        "0" = 0, "1" = 1, "2" = 1, "3" = 1,
                                        "4" = 1, "5" = 2, "6" = 2, "7" = 2,
                                        "8" = 2, "9" = 2, "10" = 3, "11" = 3,
                                        "12" = 3, "13" = 3, "14" = 3, "15" = 4,
                                        "16" = 4, "17" = 4, "18" = 4, "19" = 4,
                                        "20" = 5, "21" = 5, "22" = 5, "23" = 5,
                                        "24" = 5, "27" = 5)

## Converting PHQ-9 severity to a factor with labels
imputedata$depression_severity <- factor(imputedata$depression_severity,
                                    levels = c(0, 1, 2, 3, 4, 5),
                                    labels = c("None", "Minimal", "Mild",
                                               "Moderate", "Moderately severe",
                                               "Severe"))

## Creating GAD-7 score by summing GAD-7 related variables
imputedata$gad7_score <- imputedata$nervous_anxious + 
  imputedata$uncontrolled_worrying + imputedata$worrying + 
  imputedata$trouble_relaxing + imputedata$restlessness +
  imputedata$annoyed_irritable + imputedata$fear

## Converting GAD-7 score to a severity category
imputedata$anxiety_severity <- recode(imputedata$gad7_score,
                                        "0" = 0, "1" = 1, "2" = 1, "3" = 1,
                                        "4" = 1, "5" = 2, "6" = 2, "7" = 2,
                                        "8" = 2, "9" = 2, "10" = 3, "11" = 3,
                                        "12" = 3, "13" = 3, "14" = 3, "15" = 4,
                                        "16" = 4, "17" = 4, "18" = 4, "19" = 4,
                                        "20" = 4, "21" = 4)

## Converting GAD-7 severity to a factor with labels
imputedata$anxiety_severity <- factor(imputedata$anxiety_severity,
                                    levels = c(0, 1, 2, 3, 4),
                                    labels = c("None", "Minimal", "Mild",
                                               "Moderate", "Severe"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Creating a BMI variable

summary(imputedata$height_cm) # Checking the values seem normal
table(imputedata$height_cm)

imputedata <- imputedata %>% # Changing values in feet to cm
  mutate(height_cm = ifelse(height_cm == "5.2", 157, 
                            ifelse(height_cm == "5.4", 163, 
                                   ifelse(height_cm == "5.6", 168, height_cm))))
table(imputedata$height_cm) # Checking the values are now correct                              
table(imputedata$prepreg_wt_kg)

imputedata <- imputedata %>%  # Creating the BMI variable
  mutate(height_m = (height_cm / 100)) %>%
  mutate(bmi = (prepreg_wt_kg / (height_m ^2))) %>%
  select(-height_m)
imputedata$bmi <- round(imputedata$bmi, digits = 1) # Rounding BMI to 1 decimal
summary(imputedata$bmi) # Checking the BMI values seem plausible

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Selecting only the variables of interest for analysis

analysisdata <- imputedata %>%
  select(id, intention_to_feed, rltnship_status, depression_severity, 
         anxiety_severity, smoking_history = smoking, other_children, 
         household_income, education, bmi)

# Save the data files (not needed to be able to run scripts 2 & 3 as the  
# necessary data are included in the zip file)
# save(data, file = 'data.RData')
# save(cleandata, file = 'cleandata.RData')
# save(imputedata, file = 'imputedata.RData')
# save(analysisdata, file = 'analysisdata.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Finding the values for Table 1 of demographics

summary(cleandata)

## Number of individuals in the two intention groups
table(cleandata$intention_to_feed)

## Ethnicity (white)
cleandata %>% filter(intention_to_feed == "Yes" & ethnicity == 3) %>% nrow()
cleandata %>% filter(intention_to_feed == "No" & ethnicity == 3) %>% nrow()

## Nationality (Welsh/British)
cleandata %>% filter(intention_to_feed == "Yes" & nationality == 1 |
                       intention_to_feed == "Yes" & nationality == 2) %>% nrow()
cleandata %>% filter(intention_to_feed == "No" & nationality == 1 |
                       intention_to_feed == "No" & nationality == 2) %>% nrow()

## Smoking history
analysisdata %>% 
  filter(intention_to_feed == "Yes" & smoking_history == "Yes") %>% nrow()
analysisdata %>% 
  filter(intention_to_feed == "No" & smoking_history == "Yes") %>% nrow()

## Previous children
analysisdata %>% 
  filter(intention_to_feed == "Yes" & other_children == "Yes") %>% nrow()
analysisdata %>% 
  filter(intention_to_feed == "No" & other_children == "Yes") %>% nrow()

## BMI
temp1 <- analysisdata %>% filter(intention_to_feed == "Yes")
temp0 <- analysisdata %>% filter(intention_to_feed == "No") 
mean(temp1$bmi)
sd(temp1$bmi)
mean(temp0$bmi)
sd(temp0$bmi)
mean(analysisdata$bmi)
sd(analysisdata$bmi)

## Relationship status
## Single
analysisdata %>% 
  filter(intention_to_feed == "Yes" & rltnship_status == "Single") %>% 
  nrow()
analysisdata %>% 
  filter(intention_to_feed == "No" & rltnship_status == "Single") %>% 
  nrow()
## Dating or engaged
analysisdata %>% 
  filter(intention_to_feed == "Yes" & rltnship_status == "Dating/engaged") %>% 
  nrow()
analysisdata %>% 
  filter(intention_to_feed == "No" & rltnship_status == "Dating/engaged") %>% 
  nrow()
## Married or living with partner
analysisdata %>% 
  filter(intention_to_feed == "Yes" & rltnship_status == "Married/partnered") %>% 
  nrow()
analysisdata %>% 
  filter(intention_to_feed == "No" & rltnship_status == "Married/partnered") %>% 
  nrow()

## PHQ9 depression severity
summary(temp1$depression_severity)
summary(temp0$depression_severity)

## GAD7 anxiety severity
summary(temp1$anxiety_severity)
summary(temp0$anxiety_severity)

## Level of education
summary(temp1$education)
summary(temp0$education)

## Household income
summary(temp1$household_income)
summary(temp0$household_income)

rm(temp1, temp0)
