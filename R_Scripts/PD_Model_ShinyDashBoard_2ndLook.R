#setwd("C:\\Users\\system 4\\Py_Command")

library(shiny)
library(shinyalert)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "PD Model"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Take Test", tabName = "details", icon = icon("home")),
      menuItem("About Tests", tabName = "grid", icon = icon("search"))
    ),
    width = 123
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "details", 
              fluidRow(
                column(
                  width = 6,
                  h3("Enter Details", actionButton("info_btn", icon("info-circle"))),
                  radioButtons("Gender", "Select Your Gender", list("Male", "Female"), ""),
                  numericInput("Age", "Your Age", ""),
                  selectInput(
                    "Feature", "Select Your Feature",
                    choices = list(
                      "Anxiety","Apathy","Benton","Clock","Cognition","COGSTATE","Constipate","Depress","DopaDefic","Education",
                      "Epworth","Fatigue","Geriatric_Depression","Hallucination","Hand","Hopkins","Hopkins_Recog",
                      "Impulsive_CompulsiveBehavior","Impulsive_ICD","LetterNumber","Lexical_Fluency","LightHead","Modif_Boston",
                      "Montreal_Cognitive","Pain","REM_AwakeDream","REM_AwakeProblem","REM_Dream","REM_Movement","SCOPA_Cardio",
                      "SCOPA_Eye","SCOPA_Gastro","SCOPA_Sex","SCOPA_Thermo","SCOPA_Urine","Semantic","SleepDay","SleepNight",
                      "STAIA","STAIS","Symbol_Digit","Trail_Making_A","Trail_Making_B","UPSIT","Urine"
                    ),
                    multiple = TRUE
                  ),
                  uiOutput("score_range"),
                  actionButton("submit_btn", "Submit")
                ),
                column(
                  width = 6,
                  h3("User Information"),
                  box(
                    width = 12,
                    textOutput("mygender")
                  ),
                  box(
                    width = 12,
                    textOutput("myage")
                  ),
                  box(
                    width = 12,
                    textOutput("myfeature")
                  ),
                  box(
                    width = 12, background = 'green',
                    textOutput("myscore")
                  )
                )
              )
      ),
      tabItem(tabName = "grid",
              fluidRow(
                column(
                  width = 7,
                  h3("About Tests"),
                  tags$div(
                    style = "display: grid; grid-template-columns: repeat(5, 1fr); grid-gap: 30px; grid-row-gap: 50px; width: 150px;",
                    apply(
                      matrix(
                        c("Anxiety", "Apathy", "Benton", "Clock", "Cognition",
                          "COGSTATE","Constipate", "Depress", "Education", "Epworth",
                          "Fatigue", "GeriatricDepression","Hand", "Hopkins", "Impulsive",
                          "LetterNumber", "LexicalFluency", "LightHead", "ModifiedBoston", 
                          "MontrealCognitive","Pain", "REM_Sleep", "SCOPA", "Semantic", "Sleep_Day",
                          "Sleep_Night", "STAI", "Symbol_Digit", "TrailMaking", "UPSIT", "Urine",
                          "Parkinson", "Non_Motor", "About_Model", "More"),
                        ncol = 5), MARGIN = c(1, 2), FUN = function(x) {
                          div(
                            style = " border: 1px solid #8BF5FA; width: 150px; text-align: center; padding: 5px; box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3); border-radius: 5px;",
                            id = paste0("box_", x),
                            onclick = sprintf("Shiny.setInputValue('box_clicked', '%s');", x),
                            actionButton(
                              paste0("button_", x),
                              x
                            )
                          )
                        }
                    )
                  )
                )
                ,
                column(
                  width = 5,
                  h3("Information"),
                  tags$div(
                    id = "message",
                    style = "max-height: 600px; max-width: 600px; overflow-y: auto; text-align: justify; padding: 10px; border: 1px solid gray;
               border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.3); background-color: #97DEFF; font-size: 16px;",
               textOutput("message_text", container = div)
                  ),
               
                )
              )
      )
      
    )
  )
)
  




server <- function(input, output) {
  feature_scores <- reactiveValues(features = character(), scores = numeric())
  output$mygender <- renderText(input$Gender)
  output$myage <- renderText(input$Age)
  output$myfeature <- renderText({
    paste("Selected features: ", paste(input$Feature, collapse = ", "))
  })
  output$myscore <- renderText({
    scores <- lapply(input$Feature, function(f) {
      paste0(f, ": ", input[[paste0("Score_", f)]])
    })
    paste("Scores: ", paste(scores, collapse = "; "))
  })
  
  
  # Add custom messages
  box_messages <- list(
    Anxiety = "1. Anxiety is a normal response to stress and is a natural part of the body's fight-or-flight response.
However, when anxiety becomes excessive and interferes with daily life, it can be a symptom of an anxiety disorder.
Anxiety disorders are treatable with therapy, medication, and lifestyle changes, and early intervention can improve long-term outcomes.
Anxiety is a common symptom experienced by individuals with Parkinson's disease, which is a neurodegenerative disorder that affects movement and cognition.
Studies suggest that anxiety may be an early indicator of Parkinson's disease, as it can occur years before motor symptoms appear.
Chronic anxiety may also exacerbate the cognitive and motor symptoms of Parkinson's disease, making it important for individuals with the condition to manage their anxiety effectively.",
    Apathy = "Apathy is a state of indifference or lack of interest in one's surroundings or activities. It can be a symptom of various mental health conditions, such as depression and anxiety. 
	Apathy can also be a response to stress or a coping mechanism for dealing with overwhelming emotions.
	Apathy is a common symptom in neurodegenerative diseases such as Parkinson's disease. 
	It is characterized by a lack of motivation and decreased interest in daily activities. 
	Studies have suggested that apathy in Parkinson's disease may be caused by changes in the brain's reward and motivation systems, including reduced dopamine levels in certain areas of the brain.",
    Benton = "Purpose: The Benton Judgment of Line Orientation is a test used to assess visual perception and spatial orientation ability. It was developed by Arthur Benton in the 1960s to assess visual perceptual abilities and spatial orientation in patients with brain damage or neurological disorders.
	Test format: The test consists of 30 cards, each with two sets of 11 lines at different angles. The task is to identify which two lines on each card are oriented in the same direction.
	Scoring: The test is scored by counting the number of correct responses out of a total of 30, with a maximum score of 30. The scoring can be further divided into sub-scores for each quadrant of the visual field.
	Norms: The Benton Judgment of Line Orientation has established norms for various populations, including healthy adults, children, and individuals with neurological disorders.
	Clinical utility: The test is widely used in clinical settings to assess visual perceptual abilities and spatial orientation in patients with brain damage or neurological disorders. It has been shown to be sensitive to a range of neurological conditions, including stroke, traumatic brain injury, and dementia.
	Limitations: The test has some limitations, including the fact that it is highly dependent on visual acuity and may not be suitable for individuals with significant visual impairments. Additionally, the test is focused on one specific aspect of visual perception and does not provide a comprehensive assessment of cognitive function.",
    Clock = "The clock drawing test (CDT) is a screening tool used to assess cognitive function, particularly visuospatial and executive abilities, in individuals with neurological or psychiatric conditions.
    The test involves asking the participant to draw a clock face with numbers and set the hands to a specific time, such as 10 past 11. The test administrator will then evaluate the quality of the drawing and the accuracy of the time placement.
    The clock drawing test can provide information on a range of cognitive abilities, including attention, memory, planning, organization, and problem-solving.
    The clock drawing test is relatively quick and easy to administer, making it a useful tool for healthcare professionals in a variety of settings, such as hospitals, clinics, and nursing homes.
    Research suggests that the clock drawing test has good sensitivity and specificity for identifying cognitive impairment and dementia, and it can be used in combination with other assessment tools to help diagnose and monitor the progression of these conditions.
	While the clock drawing test is a useful screening tool, it should not be used in isolation to diagnose cognitive impairment or dementia. It is recommended that the test results are interpreted in conjunction with other clinical and neuropsychological evaluations.",
    Cognition = "Cognition refers to the mental processes involved in acquiring, processing, and using information. It includes a range of functions such as attention, perception, memory, language, and problem-solving.
    Cognition is influenced by a variety of factors, including genetics, environment, and experience. Developmental disorders, neurological conditions, and aging can all affect cognitive functioning.
    Assessment and treatment of cognitive deficits often involve a combination of behavioral, cognitive, and pharmacological approaches. Cognitive rehabilitation, cognitive-behavioral therapy, and medication management are some of the strategies used to improve cognitive function.
    Parkinson's disease is a progressive neurological disorder that primarily affects movement, but can also impact cognition. Up to 80% of people with Parkinson's disease experience some degree of cognitive impairment.
    Cognitive deficits in Parkinson's disease typically involve executive functions such as planning, attention, working memory, and mental flexibility. Other cognitive domains such as language, visuospatial skills, and memory may also be affected.
    Treatment for cognitive deficits in Parkinson's disease may involve medication management, cognitive rehabilitation, and lifestyle interventions such as exercise and cognitive stimulation. Early detection and intervention are crucial for optimizing cognitive function in people with Parkinson's disease.",
    COGSTATE = "Cogstate is a digital cognitive assessment tool that measures cognitive function, including memory, attention, processing speed, and executive function.
    It was founded in 1999 in Melbourne, Australia, by psychologists Brad and Keith Hill.
    Cogstate assessments are administered on a computer or mobile device and typically take 10-15 minutes to complete.
    The tests are designed to be engaging and simple to use, with minimal language requirements, making them suitable for use with diverse populations.
    Cogstate assessments are widely used in sports medicine to assess athletes who have experienced concussion or other head injuries.
    Cogstate assessments are also used in clinical trials to evaluate the cognitive effects of drugs and other interventions, and the company offers a range of services including study design, data management, and statistical analysis.",
    Constipate = "Constipation is a common digestive problem that affects many people. It is characterized by difficulty passing stools or infrequent bowel movements.
    Constipation can be caused by a variety of factors, including a lack of fiber in the diet, dehydration, certain medications, and medical conditions such as irritable bowel syndrome and thyroid disorders.
    Treatment for constipation may include changes to diet and lifestyle, such as increasing fiber intake, staying hydrated, and exercising regularly. In some cases, medication or surgery may be necessary to treat underlying conditions or to relieve severe constipation.
    Constipation is a common non-motor symptom of Parkinson's disease, affecting up to 80% of patients. It can occur many years before the onset of motor symptoms and may be an early warning sign of the disease.
    The exact relationship between constipation and Parkinson's disease is not fully understood, but it is believed that both conditions may be caused by the same underlying pathology in the nervous system.
    Some research suggests that constipation may even be a risk factor for developing Parkinson's disease. One study found that individuals with chronic constipation had a higher risk of developing Parkinson's disease later in life, although more research is needed to confirm this association.",
    Depress = "Depression is a common mental health condition that can affect people of all ages and backgrounds. It is characterized by persistent feelings of sadness, hopelessness, and loss of interest in activities that were once enjoyable.
    Depression can be caused by a combination of genetic, environmental, and psychological factors. Stressful life events, such as trauma or loss, can trigger depression in some people.
    Depression can be treated with a combination of therapy and medication. Cognitive-behavioral therapy (CBT) is a commonly used form of talk therapy that helps people identify and change negative thought patterns that contribute to depression.
    Depression is a common non-motor symptom of Parkinson disease, affecting up to 50% of patients. It can occur at any stage of the disease and can have a significant impact on quality of life.
    The exact relationship between depression and Parkinson disease is not fully understood, but it is believed that the degeneration of dopamine-producing neurons in the brain, which is a hallmark of Parkinson disease, may contribute to the development of depression.
    The treatment of depression in Parkinson disease is challenging because many antidepressant medications can worsen motor symptoms. However, some antidepressants, such as selective serotonin reuptake inhibitors (SSRIs), may be beneficial for both depression and anxiety in Parkinson disease patients. Psychotherapy and exercise may also be effective treatments for depression in Parkinson disease.",
    Education = "Education has been found to have a protective effect against neurodegenerative diseases such as Alzheimer's and Parkinson's disease. People with higher levels of education tend to develop these diseases later in life and experience less severe symptoms than those with less education.
    Studies have shown that education can promote cognitive reserve, which is the brain's ability to adapt and function despite damage or degeneration. This means that individuals with higher levels of education may be better able to compensate for brain changes associated with neurodegenerative diseases.
    Educational interventions have been developed to help people with neurodegenerative diseases maintain cognitive function and quality of life. These interventions may include memory training, cognitive stimulation, and social support. By continuing to learn and engage in mentally stimulating activities, individuals with neurodegenerative diseases can potentially slow the progression of their condition and improve their overall well-being.",
    Epworth = "The Epworth Sleepiness Scale (ESS) is a self-administered questionnaire that is commonly used to assess daytime sleepiness and the severity of sleep apnea.
    The ESS is a simple and easy-to-use questionnaire that consists of eight questions that assess a person's likelihood of falling asleep in different situations, such as while reading, watching TV, or sitting in traffic.
    Each question on the ESS is scored on a scale of 0 to 3, with 0 meaning that the person would never doze off in that situation and 3 meaning that they would almost always doze off.
    The total score on the ESS ranges from 0 to 24, with higher scores indicating greater daytime sleepiness. A score of 10 or higher is generally considered to be indicative of excessive daytime sleepiness.
    The ESS has been validated in a variety of populations and has been shown to be a reliable measure of daytime sleepiness.
    The ESS is often used as a screening tool for sleep apnea, as excessive daytime sleepiness is a common symptom of this condition.
    The ESS can also be used to monitor the effectiveness of treatment for sleep apnea, as a decrease in daytime sleepiness scores can be indicative of improved sleep quality.",
    Fatigue = "Fatigue refers to a feeling of physical or mental tiredness or exhaustion that can occur due to a variety of reasons such as lack of sleep, physical exertion, or illness.
    Prolonged fatigue can have significant impacts on one's quality of life, impairing concentration, productivity, and emotional well-being.
    Managing fatigue typically involves identifying and addressing the underlying causes, as well as adopting healthy lifestyle habits such as regular exercise, balanced diet, and good sleep hygiene.
    Fatigue is a common non-motor symptom experienced by people with Parkinson's disease, affecting up to 50% of patients.
    In Parkinson's disease, fatigue can be caused by both the disease process itself and the medications used to treat the condition.
    Fatigue in neurodegenerative diseases such as Parkinson's can lead to a decrease in daily activities and quality of life, and can also impact other motor and non-motor symptoms of the disease, such as depression and cognitive impairment.",
    GeriatricDepression = "The Geriatric Depression Test (GDT), also known as the Geriatric Depression Scale (GDS), is a widely used tool for assessing depression in older adults. Here are six points about the Geriatric Depression Test:
    It is a self-report questionnaire designed to identify symptoms of depression in adults aged 65 and over. It contains 30 questions that cover different aspects of depression, such as mood, motivation, and feelings of worthlessness.
    The GDT is a screening tool, not a diagnostic test. A high score on the GDT suggests the presence of depression, but a diagnosis of depression must be made by a healthcare professional.
    It is a reliable and valid measure of depression in older adults, and has been used in many clinical and research settings.
    The GDT is easy to administer and can be completed in 10-15 minutes. It can be administered in person, over the phone, or through self-administration.
    It is available in several different versions, including a short form with 15 questions and a yes/no response format. The short form can be useful in settings where time is limited or when a quick screening tool is needed.
    The GDT has been translated into many different languages and has been used in diverse cultural settings. However, some caution should be exercised when interpreting scores from individuals with limited literacy or cognitive impairment.",
    Hand = "Around 90% of people are right-handed, while only 10% are left-handed. The remaining 1% of the population is ambidextrous, meaning they can use both hands equally well.
    Studies have shown that left-handed people tend to have better spatial awareness and are more likely to excel in fields such as mathematics, architecture, and art. This is because the right side of the brain, which is associated with spatial and creative thinking, is more dominant in left-handed individuals.
    On the other hand, right-handed people are more likely to have better verbal abilities and excel in fields such as writing, language, and public speaking. This is because the left side of the brain, which is associated with language and logic, is more dominant in right-handed individuals.
    Ambidextrous people have been found to have a higher incidence of certain mental health conditions, such as schizophrenia and attention deficit hyperactivity disorder (ADHD), compared to right- or left-handed individuals.
    The reason why people have a dominant hand is still not fully understood, but it is thought to be linked to genetics and the development of the brain during fetal development.
    Interestingly, studies have found that the preference for using a particular hand can change over time, particularly during childhood. This suggests that handedness may not be fully determined at birth and can be influenced by environmental factors.",
    Hopkins = "The Hopkins Verbal Learning Test (HVLT) is a widely used neuropsychological assessment tool that measures verbal learning and memory abilities. 
    Purpose: The HVLT is used to assess a person's ability to learn and recall verbal information. It is often used to help diagnose cognitive disorders such as dementia, traumatic brain injury, and other neurological conditions.
    Administration: The HVLT consists of three learning trials in which the person is presented with a list of 12 words and asked to recall as many words as possible immediately after each trial. After a 20-30 minute delay, the person is asked to recall the words again (delayed recall). Finally, the person is presented with a second list of 12 words and asked to identify which words were on the original list (recognition trial).
    Scoring: The HVLT produces several scores, including total immediate recall, delayed recall, recognition discriminability, and recognition false positive errors. These scores are used to assess the person's overall learning and memory abilities, as well as their ability to recognize the original words.
    Norms: The HVLT has been standardized on a large sample of individuals, so scores can be compared to the norms for that person's age, education level, and other demographic factors.
    Advantages: The HVLT is a quick and easy assessment tool that can provide valuable information about a person's verbal learning and memory abilities. It is also widely used and well-researched, so it has a strong evidence base.
    Limitations: While the HVLT is a useful tool, it is important to remember that it measures only one aspect of cognitive functioning (verbal learning and memory). It is also not a diagnostic tool on its own, but rather one part of a comprehensive neuropsychological evaluation.
	Overall, the Hopkins Verbal Learning Test is a valuable tool for assessing verbal learning and memory abilities, and can provide important information for diagnosing and treating cognitive disorders.",
    Impulsive = "The QUIP (Questionnaire for Impulsive-Compulsive Disorders in Parkinson's Disease) is a screening tool used to assess the presence of impulsive and compulsive behaviors in individuals with Parkinson's disease.
    The QUIP test was developed by researchers at the University of Pennsylvania and has been validated as a reliable and valid measure of impulsive and compulsive behaviors in Parkinson's disease.
    The QUIP test consists of 19 questions that assess a range of behaviors, including gambling, shopping, eating, and sexual behavior.
    The QUIP test is designed to be self-administered and can be completed by the patient or a caregiver.
    A positive score on the QUIP test indicates the presence of impulsive or compulsive behaviors and may warrant further evaluation by a healthcare professional.
    The QUIP test is a valuable tool for clinicians in assessing the risk of impulsive and compulsive behaviors in individuals with Parkinson's disease who are being treated with dopamine agonist medications.
	Impulsive-compulsive behavior is a non-motor symptom that can occur in some people with Parkinson's disease who are being treated with dopamine replacement therapy. These behaviors can include compulsive gambling, excessive shopping, hypersexuality, and binge eating, among others.
    The exact mechanism behind impulsive-compulsive behavior in Parkinson's disease is not fully understood, but it is thought to be related to the medication's effect on the dopamine system in the brain. Dopamine replacement therapy can lead to changes in the reward pathways of the brain, which may result in impulsive-compulsive behaviors in some people with Parkinson's disease. It is important for healthcare providers to monitor and manage these behaviors, as they can have a significant impact on quality of life and social functioning.",
    LetterNumber = "Letter Number Sequencing Test is a cognitive assessment tool that measures a person's ability to maintain and manipulate information in working memory.
    Purpose: The LNST is commonly used in clinical and research settings to evaluate working memory capacity, attention, and executive functioning. It is often used as part of a battery of cognitive tests to assess various aspects of cognitive function.
    Format: The LNST involves the presentation of a series of alternating letters and numbers, and the participant is asked to recall them in ascending order according to the letter first and then the number. For example, if the sequence is A-1-B-2-C-3, the correct response would be A-B-C-1-2-3.
    Administration: The test is administered in a standardized manner, with a set number of trials, typically ranging from 2 to 7 items per trial. The difficulty level of the test can be increased or decreased depending on the individual's performance, making it a flexible assessment tool.
    Scoring: The LNST is scored based on the number of correct sequences recalled. Scores can be used to assess cognitive function and track changes over time. Normative data is available to compare individual performance to that of a healthy population of the same age and education level.
    Validity: The LNST has been shown to have good reliability and validity in assessing working memory and executive functioning. It is sensitive to cognitive decline in aging and neurological disorders such as Alzheimer's disease.
    Limitations: While the LNST is a useful tool for assessing working memory and executive functioning, it does not provide a comprehensive assessment of cognitive function. Other tests may be needed to assess additional cognitive domains, such as language, visuospatial ability, and attention.",
    LexicalFluency = "Lexical Fluency is a language assessment tool used to measure a person's ability to generate words rapidly and accurately within a specified category, such as animals or fruits.
    The test typically involves asking the person to name as many words as possible within a specific category within a limited time frame, usually one minute.
    Lexical Fluency is considered a valuable tool for assessing a person's verbal fluency and cognitive flexibility. It can provide insights into language development, cognitive impairment, and certain neurological conditions, such as dementia.
    The test is usually administered as part of a battery of assessments, including tests of memory, attention, and language comprehension. It can also be used as part of a broader cognitive evaluation.
    The scoring of the test is usually based on the number of correct words generated within the given time frame, as well as the quality of the words generated. Some assessments may also include measures of the individual's ability to switch between different categories of words.
    While the Lexical Fluency test is a valuable tool, it is important to recognize that performance on the test can be influenced by factors such as fatigue, anxiety, and cultural background. Therefore, it is important to interpret the results of the test in the context of other assessment measures and with consideration of the individual's unique circumstances.",
    LightHead = "Lightheadedness is a common symptom characterized by a sensation of dizziness, weakness, or faintness. It can be caused by a variety of factors, including low blood pressure, dehydration, anxiety, and medication side effects.
    Lightheadedness can occur suddenly and may be accompanied by symptoms such as blurred vision, nausea, and sweating. In some cases, it may lead to fainting, which can be dangerous if it occurs while driving or operating heavy machinery.
    Treatment for lightheadedness depends on the underlying cause. Simple measures such as drinking plenty of fluids, getting up slowly from a sitting or lying position, and avoiding triggers such as alcohol and caffeine can help prevent or alleviate symptoms. In some cases, medication or other medical interventions may be necessary.
    Lightheadedness is a common symptom of Parkinson's disease and may be caused by medication side effects, autonomic dysfunction, or changes in blood pressure regulation.
    In Parkinson's disease, lightheadedness can be particularly problematic because it can increase the risk of falls, which are already more common in people with the condition.
    Treatment for lightheadedness in Parkinson's disease may involve adjusting medication dosages, addressing underlying autonomic dysfunction, or incorporating physical therapy and balance exercises into the management plan. Close monitoring and communication with a healthcare provider is important to ensure effective management of symptoms.",
    ModifiedBoston = "The Modified Boston Naming Test (MBNT) is a standardized language assessment tool that evaluates a person's ability to retrieve and produce the names of common objects.
    The MBNT typically consists of 60 line drawings of objects, and the individual being tested is asked to name each object within a specified time limit.
    The purpose of the MBNT is to assess language disorders, such as aphasia, and to identify cognitive strengths and weaknesses in individuals who have suffered from neurological damage.
    The MBNT is administered by a trained professional, such as a speech-language pathologist, and can be completed in approximately 10-15 minutes.
    Scoring for the MBNT is based on the number of objects named correctly within the time limit. A lower score may indicate a language disorder or impairment.
    The MBNT is a reliable and valid measure of language ability, and has been extensively researched in both clinical and research settings. However, limitations of the MBNT include potential cultural biases in the object names, and the fact that the test only assesses one aspect of language ability.",
    MontrealCognitive = "The Montreal Cognitive Assessment (MoCA) is a brief screening tool used to assess cognitive function in adults, particularly those with mild cognitive impairment (MCI) or early-stage dementia.
    MoCA consists of a series of questions that assess various cognitive domains, including attention and concentration, memory, language, visuospatial ability, executive function, and orientation.
    The MoCA test takes around 10 to 15 minutes to complete and is administered by a healthcare professional or trained examiner.
    MoCA scores range from 0 to 30, with a score of 26 or above considered normal. However, the scoring may vary depending on the age, education, and cultural background of the individual being tested.
    MoCA is widely used in clinical practice and research settings to identify and track cognitive impairment over time. It is also used to assess the effectiveness of interventions and treatments for cognitive impairment.
    The MoCA test is available in over 100 languages, making it a useful tool for assessing cognitive function in diverse populations. However, it is important to note that the test is not a substitute for a comprehensive medical evaluation and should be used in conjunction with other diagnostic tools and assessments.",
    Pain = "Pain is a subjective experience that is unique to each individual. It can be influenced by various factors such as age, gender, genetics, previous experiences, and psychological state.
    Pain can be classified into different types based on its duration, location, and intensity. Acute pain is usually a result of injury or surgery and is of short duration, while chronic pain persists for more than 3-6 months and can significantly impact a person's quality of life.
    Pain is a protective mechanism that alerts the body to potential or actual tissue damage. It can also trigger a cascade of physiological responses that help to prevent further injury and promote healing.
    Pain is a common symptom of Parkinson's disease and can occur at any stage of the disease. It can be due to various factors such as stiffness, muscle rigidity, and changes in posture and gait.
    Studies have shown that chronic pain can accelerate cognitive decline and worsen motor symptoms in people with Parkinson's disease. It can also contribute to depression and reduce overall quality of life.
    Pain management is an important aspect of the overall care of people with Parkinson's disease. This can include pharmacological and non-pharmacological interventions such as physical therapy, exercise, and mindfulness-based stress reduction. Effective pain management can improve functional outcomes and overall well-being in people with Parkinson's disease.",
    REM_Sleep = "The REM Sleep Behavior Disorder (RBD) Test is a questionnaire designed to assess the presence and severity of symptoms associated with RBD, a sleep disorder characterized by acting out vivid dreams during REM (Rapid Eye Movement) sleep. The questionnaire is usually administered by a healthcare professional and consists of several points that aim to evaluate the presence and frequency of symptoms.
    Dream content: The questionnaire may ask about the content of your dreams, such as whether they are violent or involve physical activity. This is because RBD is often associated with violent dreams that involve physical movement.
    Sleep quality: The questionnaire may ask about the quality of your sleep, such as how often you wake up during the night and how rested you feel in the morning. RBD can cause disrupted sleep, leading to feelings of exhaustion and fatigue during the day.
    Physical activity during sleep: The questionnaire may ask about physical activity during sleep, such as kicking, punching, or jumping out of bed. These movements are common in RBD and can lead to injury to yourself or your sleep partner.
    Timing of symptoms: The questionnaire may ask about the timing of your symptoms, such as whether they occur primarily during REM sleep or throughout the night. RBD is characterized by symptoms occurring during REM sleep, which usually happens later in the night.
    Duration of symptoms: The questionnaire may ask about the duration of your symptoms, such as how long they have been present and whether they have worsened over time. RBD symptoms tend to be chronic and progressive, with episodes becoming more frequent and severe over time.
    Impact on daily life: The questionnaire may ask about the impact of your symptoms on your daily life, such as whether they affect your work or social life. RBD can lead to feelings of embarrassment and social isolation, as well as increased risk of injury to yourself or others.
	Overall, the REM Sleep Behavior Disorder Test questionnaire is a useful tool for assessing the presence and severity of RBD symptoms. If you are experiencing symptoms of RBD, it is important to talk to a healthcare professional for diagnosis and treatment",
    SCPOA = "The SCOPA-AUT is a questionnaire designed to assess the presence and severity of autonomic dysfunction in people with Parkinson's disease (PD).
    Autonomic dysfunction refers to problems with the automatic functions of the body, such as blood pressure regulation, digestion, bladder control, and sweating.
    The SCOPA-AUT consists of 25 questions that cover various aspects of autonomic function, including gastrointestinal symptoms, urinary symptoms, cardiovascular symptoms, and sexual function.
    The questionnaire takes around 10-15 minutes to complete and is usually administered by a healthcare professional, such as a neurologist or nurse.
    Each question is scored on a scale of 0-3, with higher scores indicating greater severity of autonomic dysfunction.
    The total score on the SCOPA-AUT ranges from 0 to 75, with higher scores indicating more severe autonomic dysfunction.
    The SCOPA-AUT has been shown to be a reliable and valid tool for assessing autonomic dysfunction in people with PD.
    The questionnaire can be used to monitor changes in autonomic function over time and to assess the effectiveness of treatments for autonomic dysfunction.
    In addition to the SCOPA-AUT, there are several other scales and questionnaires that are commonly used to assess outcomes in PD, including the Movement Disorder Society-Unified Parkinson's Disease Rating Scale (MDS-UPDRS) and the Parkinson's Disease Questionnaire (PDQ-39).
    Overall, the SCOPA-AUT is a useful tool for assessing autonomic dysfunction in people with PD and can help healthcare professionals provide appropriate treatment and support to improve quality of life.",
    Semantic = "The Semantic Fluency Test is a widely used cognitive assessment tool that measures an individual's ability to generate words or phrases related to a given category.
    In the test, participants are asked to generate as many words or phrases as possible within a specified time limit, usually one minute, that are related to a specific category, such as animals, fruits, or countries.
    The Semantic Fluency Test is designed to assess an individual's semantic memory, which is the ability to recall and access information about concepts and their relationships.
    The test is often used in neuropsychological evaluations to detect impairments in semantic memory, which can occur as a result of neurological disorders such as Alzheimer's disease or stroke.
    The test has been shown to be a reliable and valid measure of semantic memory, with good test-retest reliability and sensitivity to cognitive impairments.
    The Semantic Fluency Test is commonly administered in both verbal and written formats, with participants either speaking their responses or writing them down.
    The test is often used in conjunction with other cognitive assessments, such as the Mini-Mental State Examination (MMSE), to provide a comprehensive evaluation of an individual's cognitive functioning.",
    Sleep_Day = "Involuntary daytime sleeping, also known as excessive daytime sleepiness (EDS), is a condition in which a person feels the need to sleep during the day, regardless of how much sleep they got the night before.
    EDS can be caused by a variety of factors, including sleep disorders, medications, and underlying medical conditions such as depression or anxiety.
    EDS can have a significant impact on a person's quality of life, as it can affect their ability to work, drive, and perform daily activities.
    Involuntary daytime sleeping is a common symptom of Parkinson's disease, affecting up to 50% of patients with the condition.
    Parkinson's disease is a neurodegenerative disorder that affects the dopamine-producing neurons in the brain, which can result in a range of symptoms including tremors, stiffness, and problems with movement and balance.
    Involuntary daytime sleeping in Parkinson's disease is thought to be caused by changes in the brain's sleep-wake cycle, which can result in disrupted sleep patterns and increased daytime sleepiness.",
    Sleep_Night = "Insomnia: Insomnia is a common sleep disorder that affects millions of people worldwide. People with insomnia have trouble falling asleep or staying asleep, which can lead to fatigue, irritability, and other health problems.
    Sleep Apnea: Sleep apnea is another common sleep disorder that can cause difficulty sleeping at night. It occurs when the airway becomes blocked during sleep, causing a person to stop breathing temporarily. This interruption in breathing can wake a person up multiple times during the night, disrupting their sleep.
    Restless Leg Syndrome: Restless leg syndrome is a neurological disorder that causes an uncomfortable sensation in the legs, often described as a creeping, crawling, or tingling feeling. This sensation can make it difficult for a person to fall asleep or stay asleep.
    REM Sleep Behavior Disorder (RBD): RBD is a sleep disorder that is often associated with neurodegenerative diseases, including Parkinson's disease. It causes a person to physically act out their dreams during the rapid eye movement (REM) stage of sleep, often resulting in injuries.
    Sleep Fragmentation: Sleep fragmentation is a common sleep disturbance among people with neurodegenerative diseases, including Parkinson's disease. It is characterized by frequent waking during the night, which can lead to daytime sleepiness, fatigue, and cognitive impairment.
    Circadian Rhythm Disorders: Neurodegenerative diseases, including Parkinson's disease, can disrupt a person's circadian rhythm, which can lead to difficulty sleeping at night. This disruption can result in daytime sleepiness, fatigue, and other health problems.",
    STAI = "The State-Trait Anxiety Inventory (STAI) is a psychological assessment tool used to measure anxiety levels in individuals.
    The STAI test was developed in the late 1960s by psychologist Charles Spielberger.
    The test is designed to measure both state anxiety (how anxious an individual is feeling in the moment) and trait anxiety (how anxious an individual tends to be in general).
    The test consists of two separate scales: the State Anxiety Scale and the Trait Anxiety Scale.
    The State Anxiety Scale contains 20 items that measure how anxious an individual is feeling at the time of the test.
    The Trait Anxiety Scale contains 20 items that measure how anxious an individual tends to be in general.
    Each item on the test is scored on a 4-point Likert scale, with responses ranging from 'not at all' to 'very much so'.
    The STAI test has been widely used in both clinical and research settings to assess anxiety levels in individuals with various mental health conditions.
    The test is considered to be a reliable and valid measure of anxiety, and has been translated into numerous languages for use around the world.
    Some limitations of the test include the potential for response biases and the fact that it may not be sensitive enough to detect changes in anxiety levels over time.
    The STAI test is not intended to be used as a diagnostic tool, and should be used in conjunction with other assessment methods to make clinical decisions.",
    Symbol_Digit = "The Symbol Digit Modalities Test (SDMT) is a neuropsychological assessment tool that measures cognitive processing speed and attention.
    The SDMT requires the test-taker to match specific symbols with their corresponding numbers as quickly as possible within a set time limit.
    The test is widely used in clinical and research settings to evaluate cognitive impairments in various neurological conditions, such as multiple sclerosis, traumatic brain injury, and Alzheimer's disease.
    The SDMT is a relatively simple test to administer and takes around 5-10 minutes to complete, making it a practical choice for busy clinical environments.
    The test is sensitive to changes in cognitive function over time, which can be useful for monitoring disease progression or the effectiveness of treatment.
    There are several versions of the SDMT available, each with different symbol sets and scoring methods, but they all assess cognitive processing speed and attention using the same basic principles.",
    TrailMaking = "The Trail Making Test (TMT) is a widely used neuropsychological assessment tool that evaluates an individual's visual attention, processing speed, and executive function. 
	The test consists of two parts - A and B - which are designed to assess different aspects of cognitive function.
	Trail Making Test Part A:
    In Part A, the individual is asked to connect a series of 25 encircled numbers on a sheet of paper as quickly as possible while maintaining accuracy.
    The numbers are arranged randomly on the page, and the task requires the individual to shift their attention from one number to the next in a sequential order.
    Part A is used to assess an individual's visual scanning ability, attention, and processing speed.
    The time taken to complete the task is recorded and compared to age-matched norms.
	Trail Making Test Part B:
    Part B is a more complex version of the test, requiring the individual to connect a series of 25 encircled numbers and letters in an alternating pattern (e.g., 1-A-2-B-3-C, etc.).
    This task requires the individual to use both visual scanning and cognitive flexibility to switch between numbers and letters and connect them in the correct sequence.
    Part B is used to assess an individual's executive functioning, including cognitive flexibility, working memory, and mental processing speed.
    The time taken to complete the task, as well as any errors made, are recorded and compared to age-matched norms.
	Overall, the Trail Making Test is a valuable tool for assessing cognitive function and identifying impairments in visual attention, processing speed, and executive function. The test can be used in a variety of settings, including clinical, research, and occupational settings, and has been shown to have good reliability and validity.",
    UPSIT = "The UPSIT test (University of Pennsylvania Smell Identification Test) is a standardized smell identification test that assesses a person's ability to identify different odors.
    The test consists of 40 scratch-and-sniff cards, each with a different odor.
    The odors range from common scents such as coffee and chocolate to more obscure scents such as turpentine and leather.
    The test is scored on a scale of 0 to 40, with a higher score indicating better olfactory function.
    The UPSIT test is often used to diagnose olfactory dysfunction, which can be a symptom of neurological disorders such as Alzheimer's disease and Parkinson's disease.
    The UPSIT test is a quick and non-invasive way to assess a person's sense of smell, and it is a useful tool for healthcare professionals in diagnosing and monitoring certain conditions.
	Olfactory dysfunction, or a loss of sense of smell, is a common non-motor symptom of Parkinson's disease. Studies have shown that up to 90% of people with Parkinson's disease experience some degree of smell loss. In fact, smell loss may occur years before other motor symptoms of Parkinson's disease, such as tremors or stiffness, develop.
    The exact mechanism behind the link between smell loss and Parkinson's disease is not fully understood, but it is thought to be related to the degeneration of neurons in the olfactory system and in the brain regions that control movement. This degeneration is caused by the accumulation of abnormal protein deposits, known as Lewy bodies, in these areas. Olfactory dysfunction is therefore considered a potential early biomarker for Parkinson's disease, which could help with early detection and intervention.",
    Urine = "Urinary Tract Infections (UTIs): UTIs are common urinary problems that occur due to bacterial infection. Symptoms include pain or burning sensation during urination, frequent urination, and cloudy or foul-smelling urine.
    Urinary Incontinence: Urinary incontinence is the involuntary leakage of urine. It can occur due to weak bladder muscles, overactive bladder muscles, or nerve damage.
    Kidney Stones: Kidney stones are hard deposits of minerals and salts that form in the kidneys. They can cause pain in the back or lower abdomen, and difficulty passing urine.
    Urinary dysfunction is a common non-motor symptom of Parkinson's disease. It can include urinary urgency, frequency, and incontinence.
    Research has shown that changes in urinary biomarkers, such as increased levels of alpha-synuclein, may be associated with the early stages of Parkinson's disease.
    In some neurodegenerative diseases, such as Multiple System Atrophy (MSA), urinary dysfunction is a prominent symptom. MSA is a rare disorder that affects the autonomic nervous system, causing symptoms such as orthostatic hypotension and urinary incontinence.",
    Parkinson = "This is the Parkinson information box",
    Non_Motor = "This is the Non Motor information box",
    About_Model = "This is the About Model information box",
    More = "This is the More information box"
  )
  
  output$message_text <- renderText({
    clicked_box <- input$box_clicked
    if (!is.null(clicked_box)) {
      message <- box_messages[[clicked_box]]
      paste(noquote(message), sep = "\n")
    }
  })
  

  
  output$score_range <- renderUI({
    feature <- input$Feature
    score_ranges <- lapply(feature, function(feat) {
      switch(
        feat,
        'Anxiety' = sliderInput(paste0('Score_', feat), 'Score of Anxiety', min = 0, max = 3, value = 1),
        'Apathy' = sliderInput(paste0('Score_', feat), 'Score of Apathy', min = 0, max = 2, value = 1),
        'Benton' = sliderInput(paste0('Score_', feat), 'Score of Benton', min = 5.1, max = 16.38, value = 10),
        'Clock' = sliderInput(paste0('Score_', feat), 'Score of Clock', min = 1, max = 7, value = 4),
        'Cognition' = sliderInput(paste0('Score_', feat), 'Score of Cognition', min = 0, max = 2, value = 1),
        'COGSTATE' = sliderInput(paste0('Score_', feat), 'Score of COGSTATE', min = 1, max = 2, value = 1, step = 1),
        'Constipate' = sliderInput(paste0('Score_', feat), 'Score of Constipate', min = 0, max = 3, value = 1),
        'Depress' = sliderInput(paste0('Score_', feat), 'Score of Depress', min = 0, max = 4, value = 1),
        'DopaDefic' = sliderInput(paste0('Score_', feat), 'Score of DopaDefic', min = 0, max = 2, value = 1),
        'Education' = sliderInput(paste0('Score_', feat), 'Score of Education', min = 8, max = 26, value = 12),
        'Epworth' = sliderInput(paste0('Score_', feat), 'Score of Epworth', min = 0, max = 11, value = 5),
        'Fatigue' = sliderInput(paste0('Score_', feat), 'Score of Fatigue', min = 0, max = 4, value = 2),
        'Geriatric_Depression' = sliderInput(paste0('Score_', feat), 'Score of Geriatric_Depression', min = 1, max = 15, value = 6),
        'Hallucination' = sliderInput(paste0('Score_', feat), 'Score of Hallucination', min = 0, max = 1, value = 0, step = 1),
        'Hand' = sliderInput(paste0('Score_', feat), 'Score of Hand', min = 1, max = 3, value = 1),
        'Hopkins' = sliderInput(paste0('Score_', feat), 'Score of Hopkins', min = 20, max = 64, value = 30),
        'Hopkins_Recog' = sliderInput(paste0('Score_', feat), 'Score of Hopkins_Recog', min = 20, max = 61, value = 30),
        'Impulsive_CompulsiveBehavior' = sliderInput(paste0('Score_', feat), 'Score of Impulsive_CompulsiveBehavior', min = 0, max = 1, value = 1, step = 1),
        'Impulsive_ICD' = sliderInput(paste0('Score_', feat), 'Score of Impulsive_ICD', min = 0, max = 4, value = 0),
        'LetterNumber' = sliderInput(paste0('Score_', feat), 'Score of LetterNumber', min = 4, max = 19, value = 10),
        'Lexical_Fluency' = sliderInput(paste0('Score_', feat), 'Score of Lexical_Fluency', min = 11, max = 101, value = 50),
        'LightHead' = sliderInput(paste0('Score_', feat), 'Score of LightHead', min = 0, max = 3, value = 1),
        'Modif_Boston' = sliderInput(paste0('Score_', feat), 'Score of Modif_Boston', min = 8, max = 60, value = 30),
        'Montreal_Cognitive' = sliderInput(paste0('Score_', feat), 'Score of Montreal_Cognitive', min = 17, max = 30, value = 20),
        'Pain' = sliderInput(paste0('Score_', feat), 'Score of Pain', min = 0, max = 3, value = 1),
        'REM_AwakeDream' = sliderInput(paste0('Score_', feat), 'Score of REM_AwakeDream', min = 0, max = 1, value = 0, step = 1),
        'REM_AwakeProblem' = sliderInput(paste0('Score_', feat), 'Score of REM_AwakeProblem', min = 0, max = 2, value = 1),
        'REM_Dream' = sliderInput(paste0('Score_', feat), 'Score of REM_Dream', min = 0, max = 3, value = 1),
        'REM_Movement' = sliderInput(paste0('Score_', feat), 'Score of REM_Movement', min = 0, max = 6, value = 1),
        'SCOPA_Cardio' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Cardio', min = 0, max = 6, value = 1),
        'SCOPA_Eye' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Eye', min = 0, max = 3, value = 1),
        'SCOPA_Gastro' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Gastro', min = 0, max = 9, value = 1),
        'SCOPA_Sex' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Sex', min = 0, max = 6, value = 1),
        'SCOPA_Thermo' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Thermo', min = 0, max = 6, value = 1),
        'SCOPA_Urine' = sliderInput(paste0('Score_', feat), 'Score of SCOPA_Urine', min = 0, max = 17, value = 1),
        'Semantic' = sliderInput(paste0('Score_', feat), 'Score of Semantic', min = 25, max = 81, value = 60),
        'SleepDay' = sliderInput(paste0('Score_', feat), 'Score of SleepDay', min = 0, max = 2, value = 1),
        'SleepNight' = sliderInput(paste0('Score_', feat), 'Score of SleepNight', min = 0, max = 4, value = 1),
        'STAIA' = sliderInput(paste0('Score_', feat), 'Score of STAIA', min = 31, max = 54, value = 40),
        'STAIS' = sliderInput(paste0('Score_', feat), 'Score of STAIS', min = 19, max = 59, value = 40),
        'Symbol_Digit' = sliderInput(paste0('Score_', feat), 'Score of Symbol_Digit', min = 20, max = 80, value = 60),
        'Trail_Making_A' = sliderInput(paste0('Score_', feat), 'Score of Trail_Making_A', min = 13, max = 150, value = 100),
        'Trail_Making_B' = sliderInput(paste0('Score_', feat), 'Score of Trail_Making_B', min = 20, max = 294, value = 100),
        'UPSIT' = sliderInput(paste0('Score_', feat), 'Score of UPSIT', min = 0, max = 40, value = 30),
        'Urine' = sliderInput(paste0('Score_', feat), 'Score of Urine', min = 0, max = 4, value = 2)
      )
    })
    do.call(tagList, score_ranges)
  })
  
  # Add modal with overview
  observeEvent(input$info_btn, {
    showModal(
      modalDialog(
        title = "Overview",
        "Parkinson's disease is a progressive disorder that affects the nervous system and the 
        parts of the body controlled by the nerves.
        In the early stages of Parkinson's disease.",
        easyClose = TRUE,
        footer = NULL
      ))
  })
  observeEvent(input$submit_btn, {
    # Define the 10 texts to display
    texts <- c("Parkinson's Disease in second most common neurodegeneartive disease",
               "Non-motor symptoms appear early in Parkinson's Disease",
               "Men are 1.5 times more likely to have PD than women",
               "The risk of PD increases with age, some people are diagnosed with early-onset PD before the age of 50",
               "PD symptoms include: TRAP - Tremor, Rigidity, Akinesia, Postural Instability",
               "Dopamine levels decreased in Parkinson's patient",
               "Report Early - Stop Early",
               "Environmental factors, Genetics, Head Injury are major reason for PD",
               "Parkinson's Disease progression can be slowed down")
    # Choose a random text
    random_text <- sample(texts, 1)
    # Show the alert with the random text
    shinyalert(random_text, type = "info", timer = 10000, title = 'Do you know ?', closeOnClickOutside = T)
  })
  
  observeEvent(input$submit_btn, {
    # saveRDS(list(input$Gender, input$Age, input$Feature), "user_data.RDS")
    
    feature_scores$features <- input$Feature
    feature_scores$age <- input$Age
    feature_scores$gender <- input$Gender
    feature_scores$scores <- sapply(input$Feature, function(f) input[[paste0("Score_", f)]])
    n = length(input$Feature)
    age <- feature_scores$age
    output$myscore <- renderText({
      paste("Selected Features: ", paste(feature_scores$features, collapse = ", "), "\n",
            "Scores: ", paste(feature_scores$scores, collapse = ", "))
    })
    vec=c()
    for (i in 1:length(feature_scores$features)){
      vec=c(vec,noquote(feature_scores$features[i]))
      vec=c(vec,as.vector(feature_scores$scores[i]))
    }
    
    # Check if at least three features are selected
    if(length(feature_scores$features) < 3) {
      showModal(modalDialog(
        title = "Error",
        "Please select at least three features.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      inputVec = paste(vec,  collapse =" ")
      command = sprintf("python Z:\\PPMI_Data\\Excels\\CollaborativeFiltering\\ThreeModelPrediction.py %s %s", age, n)
      command = paste(command, inputVec, " ")
      out = shell(command,intern = TRUE)
      #out[9]
      print(out[1])
    }
    
  })
}

shinyApp(ui = ui, server = server)