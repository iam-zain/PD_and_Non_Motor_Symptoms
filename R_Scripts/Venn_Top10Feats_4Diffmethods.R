###########  Combined #########

x <- list()
x$DecisionTree <- sort(as.character(c('UPSIT','Trail Making B','Fatigue','Montreal Cognitive','Symbol Digit')))
x$RandomForest <- sort(as.character(c('UPSIT','Fatigue','Symbol Digit','Trail Making B','Montreal Cognitive',
                                      'Lexical Fluency','LetterNumber','REM Dream','SCOPA Sex','Modif Boston')))
x$Boruta <- sort(as.character(c('Fatigue','Apathy','Montreal Cognitive','REM Dream','SCOPA Gastro',
                                'Symbol Digit','Trail Making A','Trail Making B','UPSIT')))
x$NMI <- sort(as.character(c('UPSIT','Montreal Cognitive','Trail Making B','Fatigue','Symbol Digit',
                             'Constipate','SCOPA Gastro','Apathy','LightHead','Trail Making A')))

# Define colors
colors <- c("#F1746B", "#36C3D1", "#F2CC8F", "#DCD6F7")

# Create a Venn diagram with beautiful formatting
venn <- venn.diagram(
  x, main = "Top Features of 4 Different Methods - Combined",fill = colors,
  fontface = "bold", filename = NULL, cex = 1.5, cat.cex = 2.2, main.cex = 3,
  category.names = c("DecisionTree", "NMI", "RandomForest", "Boruta"))

# Calculate and sort overlaps
overlaps <- calculate.overlap(x)
overlaps <- overlaps[str_sort(names(overlaps), numeric = TRUE)]

# Add overlap labels
walk2(seq(overlaps) + 8, seq(overlaps), function(x, y) {
  venn[[x]]$label <<- paste0(overlaps[[y]], collapse = "\n")})

# Draw the Venn diagram
grid.draw(venn)




###########  Female  ##############
# Define the order of the boxes
x <- list()
x$DecisionTree <- sort(as.character(c('UPSIT','Montreal Cognitive','Constipate','Trail Making B','Pain')))
x$RandomForest <- sort(as.character(c('UPSIT','Montreal Cognitive','Pain','Semantic','Benton','SCOPA Thermo','SCOPA Sex',
                                 'LetterNumber','Constipate','REM Dream')))
x$Boruta <- sort(as.character(c('Constipate','Montreal Cognitive','SCOPA Gastro','UPSIT')))
x$NMI <- sort(as.character(c('UPSIT','Montreal Cognitive','SCOPA Gastro','Constipate','Pain','SCOPA Urine',
                        'STAIS','Urine','LightHead','Lexical Fluency')))

colors <- c("#F1746B", "#36C3D1", "#F2CC8F", "#DCD6F7")
venn <- venn.diagram(
  x, main = "Top Features of 4 Different Methods - Female",fill = colors,
  fontface = "bold", filename = NULL, cex = 1.5, cat.cex = 2.2, main.cex = 3,
  category.names = c("DecisionTree", "NMI", "RandomForest", "Boruta"))

overlaps <- calculate.overlap(x)
overlaps <- overlaps[str_sort(names(overlaps), numeric = TRUE)]
walk2(seq(overlaps) + 8, seq(overlaps), function(x, y) {
  venn[[x]]$label <<- paste0(overlaps[[y]], collapse = "\n")})
grid.draw(venn)





###########  Male  ##############
x <- list()
x$DecisionTree <- sort(as.character(c('UPSIT','Symbol Digit','Trail Making B','Fatigue','Apathy')))
x$RandomForest <- sort(as.character(c('UPSIT','Symbol Digit','Fatigue','Trail Making B','REM Dream',
                                      'Pain','Geriatric Depression','SCOPA Sex','SleepDay','SleepNight')))
x$Boruta <- sort(as.character(c('Hopkins Recog','LetterNumber','Fatigue','Depress','Apathy',
                                'Modif Boston','Montreal Cognitive','REM Dream','Symbol Digit','Trail Making A',
                                'Trail Making B','UPSIT')))
x$NMI <- sort(as.character(c('UPSIT','Trail Making B','Fatigue','Montreal Cognitive','Trail Making A','Apathy',
                             'Depress','SleepDay','SCOPA Sex','LightHead')))

colors <- c("#F1746B", "#36C3D1", "#F2CC8F", "#DCD6F7")
venn <- venn.diagram(
  x, main = "Top Features of 4 Different Methods - Male",fill = colors,
  fontface = "bold", filename = NULL, cex = 1.5, cat.cex = 2.2, main.cex = 3,
  category.names = c("DecisionTree", "NMI", "RandomForest", "Boruta"))

overlaps <- calculate.overlap(x)
overlaps <- overlaps[str_sort(names(overlaps), numeric = TRUE)]
walk2(seq(overlaps) + 8, seq(overlaps), function(x, y) {
  venn[[x]]$label <<- paste0(overlaps[[y]], collapse = "\n")})
grid.draw(venn)

