
x = list()
x$Female <- as.character(c('Constipate','Pain','Montreal Cognitive','SCOPA Gastro','UPSIT'))
x$Male <- as.character(c('Apathy','Depress','Fatigue','SleepDay','Montreal Cognitive'
,'REM Dream','SCOPA Sex','Symbol Digit','Trail Making A','Trail Making B','UPSIT'))
x$Combined <- as.character(c('Apathy',	'Fatigue',	'Montreal Cognitive',	'REM Dream',	'SCOPA Gastro',	
    'Symbol Digit',	'Trail Making A',	'Trail Making B',	'UPSIT'))

# Showing names
# Define colors for each set
set_colors <- c("#F1746B", "#36C3D1", "#F2CC8F")

# Create the Venn diagram
ggvenn(x, show_elements = TRUE, label_sep = "\n", text_color = "black", text_size = 5,
       set_name_color = set_colors, fill_color = set_colors)

# Showing numbers
ggvenn(x, show_elements = F, label_sep = "\n", text_color = "black", text_size = 7,set_name_color= set_colors,
       show_percentage = F, fill_color = set_colors)
