setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files')
# your data
df <- read.csv('Feats45_Categ.csv')
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

# Define custom labels for the 'Value' column
value_labels <- c('Normal', 'Mild', 'Severe')

# Convert 'Value' to a factor with custom labels

# Reshape the data for plotting
data_long <- df %>% gather(key = "Independent_Var", value = "Value", -APPRDX)
data_long$Value <- factor(data_long$Value, levels = 0:2, labels = value_labels)

# Calculate the total count for each combination of Independent_Var, APPRDX, and Value
total_counts <- data_long %>%
  group_by(Independent_Var, APPRDX, Value) %>%
  summarise(Count = n())

# Calculate the total count for each combination of Independent_Var and Value
total_counts_total <- total_counts %>%
  group_by(Independent_Var, Value) %>%
  summarise(TotalCount = sum(Count))

# Calculate the percentage of TotalCount for each combination of Independent_Var, APPRDX, and Value
percentage_data <- total_counts %>%
  left_join(total_counts_total, by = c("Independent_Var", "Value")) %>%
  mutate(Percentage = (Count / TotalCount) * 100)

# Define the image dimensions
image_width <- 3  # Specify the width in inches
image_height <- 5  # Specify the height in inches

# Create a folder in the present directory to store the images
if (!dir.exists("Count_Plots")) {
  dir.create("Count_Plots")}

# Create separate stacked bar plots for each independent variable
plots <- list()
independent_vars <- unique(percentage_data$Independent_Var)

for (var in independent_vars) {
  plot_data <- filter(percentage_data, Independent_Var == var)
  
  p <- ggplot(plot_data, aes(x = Value, y = Percentage, fill = APPRDX)) +
    geom_bar(stat = "identity") +
    labs(title = paste("", var), x = "", y = "Percentage") +
    scale_y_continuous(limits = c(0, 100)) +  # Ensure the y-axis goes from 0 to 100
    theme_minimal() + scale_x_discrete(labels = value_labels) +  # Use custom labels for x-axis
    theme(axis.text.x = element_text(size=16, angle=60, vjust = 0.99, hjust=1, color="black")) +
    theme(axis.text.y = element_text(size=16, color="black")) + guides(fill = FALSE) +
    theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=16)) +
    theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=16)) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  # Save the plot as an SVG file with specified dimensions
  svg_filename <- paste("Count_Plots/", var, ".svg", sep = "")
  ggsave(svg_filename, p, width = image_width, height = image_height)
  
  plots[[var]] <- p
}

# Print the plots
plots
