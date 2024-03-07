########### 1. Female ############
library(RColorBrewer)
x = list()
x$NMI <- as.character(c('Constipation','Lexical Fluency','LightHead','MDSP Urine','Montreal Cognitive',
                        'Pain','SCOPA Gastro','SCOPA Urine','STAIS','UPSIT'))
x$DecisionTree <- as.character(c('UPSIT','Constipation','Montreal Cognitive','Pain','SCOPA Gastro'))
x$RandomForest <- as.character(c('Benton','Hopkins','LetterNumber','Montreal Cognitive','Pain','SCOPA Sex',
                                 'SCOPA Thermo','SCOPA Urine','Semantic','UPSIT'))
x$Boruta <- as.character(c('Constipation','Montreal Cognitive','SCOPA Gastro','UPSIT'))


ggvenn(x, show_elements = T, label_sep = "\n", text_color = "black", text_size = 4,set_name_color='red',
       fill_color = brewer.pal(name="Set3",n=4))


########### 2. Male ############
x = list()
x$NMI <- as.character(c('Apathy','Depression','Fatigue','LightHead','Montreal Cognitive','SCOPA Gastro','SCOPA Sex',
                        'SleepDay','Trail Making B','UPSIT'))
x$DecisionTree <- as.character(c('Fatigue','Montreal Cognitive','Symbol Digit','Trail Making B','UPSIT'))
x$RandomForest <- as.character(c('Benton','COGSTATE','Clock','Cognition','Epworth','Geriatric Depressionion',
                                 'Hopkins Recog','Hopkins','LetterNumber','Lexical Fluency'))
x$Boruta <- as.character(c('Apathy','Depression','Fatigue','LetterNumber','Montreal Cognitive','SCOPA Sex',
                           'Symbol Digit','Trail Making B','UPSIT'))


ggvenn(x, show_elements = T, label_sep = "\n", text_color = "black", text_size = 4,set_name_color='red',
       fill_color = brewer.pal(name="Set3",n=4))
