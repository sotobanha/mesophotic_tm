####

## R Code used in the manuscript by Banha et al. (submitted to Coral Reefs)
## Mesophotic ecosystems: a topic modeling analysis of research trends and gaps

####


#### Creating the Topic Model ####

library(bibliometrix)
library(tm)
library(slam)
library(topicmodels)
library(readxl)

# Load data
dat <- read.csv("data/topmod_mesophotic.csv", stringsAsFactors = F)
names(dat)
dat$text <- paste(dat$Article.Title, dat$Author.Keywords, dat$Abstract, sep = " ")
DF <- as.data.frame(cbind(dat$Paper.ID, dat$text))
names(DF) <- c("doc_id", "text")
CPmatrix <- VCorpus(DataframeSource(DF))
CPmatrix <- tm_map(CPmatrix, content_transformer(tolower))

# Remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})
CPmatrix <- tm_map(CPmatrix, toSpace, "_")
CPmatrix <- tm_map(CPmatrix, toSpace, "-")
CPmatrix <- tm_map(CPmatrix, toSpace, "’")
CPmatrix <- tm_map(CPmatrix, toSpace, "‘")
CPmatrix <- tm_map(CPmatrix, toSpace, "”")
CPmatrix <- tm_map(CPmatrix, toSpace, "“")
CPmatrix <- tm_map(CPmatrix, toSpace, ";")
CPmatrix <- tm_map(CPmatrix, toSpace, "=")
CPmatrix <- tm_map(CPmatrix, toSpace, "@")
CPmatrix <- tm_map(CPmatrix, toSpace, ",")

changeWords <- content_transformer(function(x, pattern, sub) {
  pattern <- paste0("\\b", pattern, "\\b")
  return(gsub(pattern, sub, x))
})

# Remove punctuation
CPmatrix <- tm_map(CPmatrix, removePunctuation)
# Remove numbers
CPmatrix <- tm_map(CPmatrix, removeNumbers)
# Remove stopwords
CPmatrix <- tm_map(CPmatrix, removeWords, stopwords("english"))
# Remove whitespace
CPmatrix <- tm_map(CPmatrix, stripWhitespace)
# Check the data
writeLines(as.character(CPmatrix[[2]]))

remove_whole_words <- content_transformer(function(text, words) {
  pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
  gsub(pattern, "", text)
})

# Extract only the text content from CPmatrix (without metadata structure issues)
text.processed <- sapply(CPmatrix, as.character)

# Save for reference in a clean format (optional)
write.csv(data.frame(document_id = names(text.processed), content = text.processed),
  "data/text.processed.csv",
  row.names = FALSE
)


# N-gram analysis on the processed text
library(ngram)

# Combine all processed documents into one string for n-gram analysis
combined_text <- paste(text.processed, collapse = " ")

# Bigram analysis
ng <- ngram(combined_text, n = 2)
get.phrasetable(ng)[1:150, ]

# Trigram analysis
ng3 <- ngram(combined_text, n = 3)
get.phrasetable(ng3)[1:150, ]

CPmatrix <- tm_map(CPmatrix, changeWords, "mesophotic coral ecosystems", "mesophotic_coral_ecosystem")
CPmatrix <- tm_map(CPmatrix, changeWords, "mesophotic coral ecosystem", "mesophotic_coral_ecosystem")
CPmatrix <- tm_map(CPmatrix, changeWords, "mce", "mesophotic_coral_ecosystem")
CPmatrix <- tm_map(CPmatrix, changeWords, "great barrier reef", "great_barrier_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "gbr", "great_barrier_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "remotely operated vehicle", "remotely_operated_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "remotely operated vehicles", "remotely_operated_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "rovs", "remotely_operated_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "rov", "remotely_operated_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "us virgin islands", "us_virgin_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "flower garden banks", "flower_garden_banks")
CPmatrix <- tm_map(CPmatrix, changeWords, "sea level rise", "sea_level_rise")
CPmatrix <- tm_map(CPmatrix, changeWords, "reef building corals", "reef_building_corals")
CPmatrix <- tm_map(CPmatrix, changeWords, "northwestern hawaiian islands", "northwestern_hawaiian_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "nwhi", "northwestern_hawaiian_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "baited remote underwater video", "bruv")
CPmatrix <- tm_map(CPmatrix, changeWords, "baited remote underwater", "bruv")
CPmatrix <- tm_map(CPmatrix, changeWords, "remote underwater video", "bruv")
CPmatrix <- tm_map(CPmatrix, changeWords, "main hawaiian islands", "main_hawaiian_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "crustose coralline algae", "crustose_coralline_algae")
CPmatrix <- tm_map(CPmatrix, changeWords, "last glacial maximum", "last_glacial_maximum")
CPmatrix <- tm_map(CPmatrix, changeWords, "autonomous underwater vehicle", "autonomous_underwater_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "gulf of mexico", "gulf_of_mexico")
CPmatrix <- tm_map(CPmatrix, changeWords, "gulf mexico", "gulf_of_mexico")
CPmatrix <- tm_map(CPmatrix, changeWords, "shelf edge", "shelf_edge")
CPmatrix <- tm_map(CPmatrix, changeWords, "new species", "new_species")
CPmatrix <- tm_map(CPmatrix, changeWords, "shallow water", "shallow_water")
CPmatrix <- tm_map(CPmatrix, changeWords, "shallow reefs", "shallow_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "shallow reef", "shallow_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "deep reefs", "deep_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "deep reef", "deep_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "shallow mesophotic", "shallow_mesophotic")
CPmatrix <- tm_map(CPmatrix, changeWords, "sea level", "sea_level")
CPmatrix <- tm_map(CPmatrix, changeWords, "continental shelf", "continental_shelf")
CPmatrix <- tm_map(CPmatrix, changeWords, "depth gradient", "depth_gradient")
CPmatrix <- tm_map(CPmatrix, changeWords, "depth range", "depth_range")
CPmatrix <- tm_map(CPmatrix, changeWords, "sp nov", "sp_nov")
CPmatrix <- tm_map(CPmatrix, changeWords, "upper mesophotic", "upper_mesophotic")
CPmatrix <- tm_map(CPmatrix, changeWords, "climate change", "climate_change")
CPmatrix <- tm_map(CPmatrix, changeWords, "coral cover", "coral_cover")
CPmatrix <- tm_map(CPmatrix, changeWords, "deep sea", "deep_sea ")
CPmatrix <- tm_map(CPmatrix, changeWords, "coralline algae", "coralline_algae")
CPmatrix <- tm_map(CPmatrix, changeWords, "rhodolith beds", "rhodolith_bed")
CPmatrix <- tm_map(CPmatrix, changeWords, "rhodolith bed", "rhodolith_bed")
CPmatrix <- tm_map(CPmatrix, changeWords, "black corals", "black_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "black coral", "black_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "scleractinian corals", "scleractinian_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "scleractinian coral", "scleractinian_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "virgin islands", "virgin_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "lower mesophotic", "lower_mesophotic")
CPmatrix <- tm_map(CPmatrix, changeWords, "outer shelf", "outer_shelf")
CPmatrix <- tm_map(CPmatrix, changeWords, "environmental conditions", "environmental_condition")
CPmatrix <- tm_map(CPmatrix, changeWords, "environmental condition", "environmental_condition")
CPmatrix <- tm_map(CPmatrix, changeWords, "montastraea cavernosa", "montastraea_cavernosa")
CPmatrix <- tm_map(CPmatrix, changeWords, "cavernosa", "montastraea_cavernosa")
CPmatrix <- tm_map(CPmatrix, changeWords, "red coral", "red_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "new records", "new_record")
CPmatrix <- tm_map(CPmatrix, changeWords, "new record", "new_record")
CPmatrix <- tm_map(CPmatrix, changeWords, "depth generalists", "depth_generalist")
CPmatrix <- tm_map(CPmatrix, changeWords, "depth generalist", "depth_generalist")
CPmatrix <- tm_map(CPmatrix, changeWords, "spawning aggregations", "spawning_aggregation")
CPmatrix <- tm_map(CPmatrix, changeWords, "spawning aggregation", "spawning_aggregation")
CPmatrix <- tm_map(CPmatrix, changeWords, "low light", "low_light")
CPmatrix <- tm_map(CPmatrix, changeWords, "water column", "water_column")
CPmatrix <- tm_map(CPmatrix, changeWords, "upper slope", "upper_slope")
CPmatrix <- tm_map(CPmatrix, changeWords, "pulley ridge", "pulley_ridge")
CPmatrix <- tm_map(CPmatrix, changeWords, "bioconstructions", "bioconstruction")
CPmatrix <- tm_map(CPmatrix, changeWords, "bioherms", "bioherm")
CPmatrix <- tm_map(CPmatrix, changeWords, "bleached", "bleach")
CPmatrix <- tm_map(CPmatrix, changeWords, "bruvs", "bruv")
CPmatrix <- tm_map(CPmatrix, changeWords, "cavernosa", "montastraea_cavernosa")
CPmatrix <- tm_map(CPmatrix, changeWords, "congeners", "congener")
CPmatrix <- tm_map(CPmatrix, changeWords, "counterparts", "counterpart")
CPmatrix <- tm_map(CPmatrix, changeWords, "gbr", "great_barrier_reef")
CPmatrix <- tm_map(CPmatrix, changeWords, "gorgonians", "gorgonian")
CPmatrix <- tm_map(CPmatrix, changeWords, "hotspots", "hotspot")
CPmatrix <- tm_map(CPmatrix, changeWords, "italian", "italy")
CPmatrix <- tm_map(CPmatrix, changeWords, "mce", "mesophotic_coral_ecosystem")
CPmatrix <- tm_map(CPmatrix, changeWords, "mces", "mesophotic_coral_ecosystem")
CPmatrix <- tm_map(CPmatrix, changeWords, "nwhi", "northwestern_hawaiian_islands")
CPmatrix <- tm_map(CPmatrix, changeWords, "octocorallia", "octocoral")
CPmatrix <- tm_map(CPmatrix, changeWords, "octocorals", "octocoral")
CPmatrix <- tm_map(CPmatrix, changeWords, "outcrops", "outcrop")
CPmatrix <- tm_map(CPmatrix, changeWords, "rhodoliths", "rhodolith")
CPmatrix <- tm_map(CPmatrix, changeWords, "rov", "remotely_operated_vehicle")
CPmatrix <- tm_map(CPmatrix, changeWords, "scleractinia", "scleractinian_coral")
CPmatrix <- tm_map(CPmatrix, changeWords, "seamounts", "seamount")
CPmatrix <- tm_map(CPmatrix, changeWords, "shelves", "shelf")
CPmatrix <- tm_map(CPmatrix, changeWords, "sounds", "sound")
CPmatrix <- tm_map(CPmatrix, changeWords, "submersibles", "submersible")
CPmatrix <- tm_map(CPmatrix, changeWords, "symbiodiniaceae", "symbiodinium")
CPmatrix <- tm_map(CPmatrix, changeWords, "terraces", "terrace")
CPmatrix <- tm_map(CPmatrix, changeWords, "twilight zone", "twilight_zone")
CPmatrix <- tm_map(CPmatrix, changeWords, "usvi", "us_virgin_islands")

# Save the data
saveRDS(CPmatrix, "data/CPmatrix.rds")

# Load the data
CPmatrix <- readRDS("data/CPmatrix.rds")

DTMmatrix <- DocumentTermMatrix(CPmatrix)
dim(DTMmatrix)
which(col_sums(DTMmatrix) == 555)

# Filter terms by frequency (remove overly common or rare terms)
summary(col_sums(DTMmatrix))
table(col_sums(DTMmatrix))
DTMmatrix <- DTMmatrix[, col_sums(DTMmatrix) > 20]
DTMmatrix <- DTMmatrix[, col_sums(DTMmatrix) < 650]

col_sums(DTMmatrix)[order(names((col_sums(DTMmatrix))))]

saveRDS(DTMmatrix, "data/DTMmatrix.rds")
DTMmatrix <- readRDS("data/DTMmatrix.rds")

# Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE
keep <- 100

################ Find optimal number of topics

library("ldatuning")

dtm <- DTMmatrix
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = "Deveaud2014",
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

# Run LDA using Gibbs sampling


ldaOut19 <- LDA(DTMmatrix, 19, method = "Gibbs", control = list(seed = seed, nstart = nstart, best = best, burnin = burnin, iter = iter, keep = keep))
ldaOut.terms <- as.matrix(terms(ldaOut19, 190))
write.csv(ldaOut.terms, "data/model19.csv")
saveRDS(ldaOut19, "data/ldaOut19.rds")
M19 <- modeltools::posterior(ldaOut19)$topics # article x topic
write.csv(M19, "data/Articles_m19.csv")




#### Data Analysis ####


library(bibliometrix)
library(tm)
library(slam)
library(topicmodels)
library(readxl)

# Open the data

ldaOut19 <- readRDS("data/ldaOut19.rds")
dat <- read_excel("data/topmod_mesophotic.xlsx") # the .csv is also available

# Topic labels
topic_names <- c(
  "Trophic ecology",
  "Reef development and framework",
  "Photosymbiosis",
  "Species distribution and range",
  "Spatial patterns and environmental drivers",
  "Global change impacts",
  "Depth gradient and zonation",
  "Movement ecology and spawning aggregations",
  "Benthic communities",
  "Genetic connectivity and population structure",
  "Biogeography and biodiversity",
  "Anthropogenic impacts and management",
  "Deep reef refuge hypothesis",
  "Geomorphology and sedimentology",
  "New species description",
  "Fish assemblages",
  "Survey methods",
  "Mediterranean Sea",
  "Species and habitats distribution modeling"
)

topic_num.names <- c(
  "1.Trophic ecology",
  "2.Reef development and framework",
  "3.Photosymbiosis",
  "4.Species distribution and range",
  "5.Spatial patterns and environmental drivers",
  "6.Global change impacts",
  "7.Depth gradient and zonation",
  "8.Movement ecology and spawning aggregations",
  "9.Benthic communities",
  "10.Genetic connectivity and population structure",
  "11.Biogeography and biodiversity",
  "12.Anthropogenic impacts and management",
  "13.Deep reef refuge hypothesis",
  "14.Geomorphology and sedimentology",
  "15.New species description",
  "16.Fish assemblages",
  "17.Survey methods",
  "18.Mediterranean Sea",
  "19.Species and habitats distribution modeling"
)


topic_num <- c(
  "1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "10.",
  "11.", "12.", "13.", "14.", "15.", "16.", "17.", "18.", "19."
)

M1 <- t(modeltools::posterior(ldaOut19)$terms) # word x topic
M2 <- modeltools::posterior(ldaOut19)$topics # article x topic
M3 <- as.data.frame(cbind(M2, dat$Publication.Year)) # article x topic with year

####
#### ARTICLES PER YEAR WITH ACCUMULATION ####
####

# Articles per year with accumulation curve
dat <- read_excel("data/topmod_mesophotic.xlsx")

library(ggplot2)
library(dplyr)

# Number of articles

# Calculate total articles per year
total_articles_per_year <- dat %>%
  group_by(Publication.Year) %>%
  summarise(total_articles = n(), .groups = "drop") # Summarize total articles per year


## Merging both

# Calculate number of articles per year and accumulation curve
artigos_por_ano <- dat %>%
  group_by(Publication.Year) %>%
  summarise(n_artigos = n(), .groups = "drop") %>%
  mutate(acumulado = cumsum(n_artigos)) # Calculate accumulation curce

# Define year intervals
anos_intervalo <- c(1958, seq(1990, 2020, by = 10), 2024)


# Create plot with two y-axes
par(mar = c(5, 5, 2, 5)) # Adjust plot margins

# Bar plot
barplot_heights <- barplot(artigos_por_ano$n_artigos,
  names.arg = artigos_por_ano$Publication.Year, # Display all years
  col = "white", border = "black",
  ylim = c(0, max(artigos_por_ano$n_artigos) + 5), # Adjust y-axis limit
  xlab = NULL, ylab = "# Articles published per year",
  xaxt = "n", # Do not draw x-axis automatically
  # cex.names = 1,  # Aumenta o tamanho dos rótulos no eixo x
  cex.lab = 1.3, # Aumenta o tamanho do título do eixo y
  cex.axis = 1.3
) # Aumenta o tamanho dos rótulos no eixo y
# Add labels only for desired intervals
posicoes_intervalo <- match(anos_intervalo, artigos_por_ano$Publication.Year)

# Add interval labels aligned with bars
axis(1,
  at = barplot_heights[posicoes_intervalo],
  labels = anos_intervalo, tick = TRUE, line = 0, cex.axis = 1.3
) # Add interval labels

# Add asterisk above 2024 bar
text(
  x = barplot_heights[which(artigos_por_ano$Publication.Year == 2024)],
  y = artigos_por_ano$n_artigos[which(artigos_por_ano$Publication.Year == 2024)] + 5,
  labels = "*", cex = 3, col = "black"
)

# Add line at x-axis zero
abline(h = 0, col = "black") # Reference line at zero

# Accumulation curve plot
par(new = TRUE) # Allow plot overlay
plot(artigos_por_ano$Publication.Year,
  artigos_por_ano$acumulado,
  type = "l", col = "red", lwd = 3,
  ylim = c(0, max(artigos_por_ano$acumulado) + 200), # Adjust y-axis limit
  ylab = "", xlab = "", axes = FALSE
) # Do not draw axes

# Add right y-axis for accumulation
axis(4, col.axis = "black", cex.axis = 1.3) # Right y-axis
mtext("# Articles accumulated", side = 4, line = 3, col = "black", cex = 1.3) # Right y-axis label


#### ARTICLES PER TOPICS ####


M2tops <- M2
colnames(M2tops) <- topic_names


# Identify the index of the topic with the highest proportion in each article
principal_topic <- apply(M2tops, 1, which.max)

# Count how many times each topic is the main one
topico_counts <- table(principal_topic)

# Create a table with the count of main topics
topico_counts_df <- data.frame(
  Topico = as.numeric(names(topico_counts)),
  Quantidade_de_artigos = as.vector(topico_counts)
)

# Display the result
print(topico_counts_df)

# Sum total number of articles
total_artigos <- sum(topico_counts)

# Display total articles
print(total_artigos)


#### ARTICLES PER TOPICS BAR PLOT ####

# Required packages
library(ggplot2)
library(vegan) # For vegdist and clustering


# Ordering by article count descending
topico_counts_df <- topico_counts_df[order(-topico_counts_df$Quantidade_de_artigos), ]

# Convert topic number to topic name using topic_names object
topico_counts_df$Nome_do_Topico <- topic_names[topico_counts_df$Topico]

# Creating bar plot, removing cluster colors and adding black borders
ggplot(topico_counts_df, aes(y = reorder(Nome_do_Topico, Quantidade_de_artigos), x = Quantidade_de_artigos)) +
  geom_bar(stat = "identity", fill = NA, color = "black", linewidth = 1, width = 0.85) + # Hollow bars and black borders
  labs(y = NULL, x = "Main topic (# articles)", title = NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks()) + # Add tick marks to axis
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16, angle = 0, hjust = 1, colour = "black"),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 14), # Increase X-axis title size
    axis.ticks.x = element_line(size = 0.5, color = "black") # Add tick marks to X-axis
  )


#### TOP 50 words similarity - Cluster ####

# MAKING THE TOP 50 MATRIX

# Initialize list to store unique words
palavras_selecionadas50 <- c()

# Loop over each topic (column) to identify top 50 words with highest probabilities
for (i in 1:19) {
  # Sort words by current topic (i) in descending order of probability
  top50_palavras <- rownames(M1)[order(M1[, i], decreasing = TRUE)]

  # Select top 50 most important words
  top50_unicas <- top50_palavras[1:50]

  # Add words to selected words list
  palavras_selecionadas50 <- unique(c(palavras_selecionadas50, top50_unicas))
}

# Create new empty matrix M1_top50
M1_top50 <- data.frame(matrix(0, nrow = length(palavras_selecionadas50), ncol = 19))
rownames(M1_top50) <- palavras_selecionadas50 # Set words as row names

# Fill M1_top50 matrix with probabilities
for (palavra in palavras_selecionadas50) {
  for (i in 1:19) {
    M1_top50[palavra, i] <- M1[palavra, i] # Assign corresponding probability
  }
}

# Name columns from 1 to 19
colnames(M1_top50) <- topic_names

# Saving spreadsheet with top 50 words

# Initialize list to store top 50 words for each topic
top50_matrix <- matrix(NA, nrow = 50, ncol = 19)

# Loop over each topic (column) to identify top 50 words with highest probabilities
for (i in 1:19) {
  # Sort words by current topic (i) in descending order of probability
  top50_palavras <- rownames(M1)[order(M1[, i], decreasing = TRUE)]

  # Select top 50 most important words
  top50_matrix[, i] <- top50_palavras[1:50]
}

# Assign names to matrix columns
colnames(top50_matrix) <- topic_names

# Save matrix to file top50_words_mce.csv
write.csv(top50_matrix, file = "data/top50_words_mce.csv")

library(openxlsx)

# Read existing spreadsheet
top50_data <- read.csv("data/top50_words_mce.csv")

# Reorganize columns alphabetically
top50_data_sorted <- top50_data[, order(colnames(top50_data))]

# Save reorganized spreadsheet to new file or overwrite existing
write.xlsx(top50_data_sorted, file = "data/top50_words_mce_sorted.xlsx")

# Saving spreadsheet with probabilities
write.csv(M1_top50, file = "data/top50_words_probability_mce.csv", row.names = TRUE)


# The new M1_top50 matrix will have all words from the
# top 50 most important of each topic and their probabilities


# DISSIMILARITY


library(vegan)
library(ggplot2)
# Calculate dissimilarity matrix using Bray-Curtis
dissimilaridade_bray_curtis50 <- vegdist(t(M1_top50), method = "bray")

# Visualize dissimilarity matrix
print(dissimilaridade_bray_curtis50)

## CLUSTER

# Perform hierarchical clustering
cluster_hierarquico50 <- hclust(dissimilaridade_bray_curtis50, method = "ward.D2") # Clustering method can be changed

# Plot dendrogram
plot(cluster_hierarquico50, main = "Cluster Bray-Curtis with top 50", xlab = "Topics", ylab = "Dissimilarity")

# Check dissimilarity and clustering structure
cat("Number of topics in M1_top50:", ncol(M1_top50), "\n") # should be 19
cat("Number of groups:", length(grupos), "\n") # should be equal to number of topics

# Calculate groups using cutree
grupos <- cutree(cluster_hierarquico50, h = 0.94)

# Ensure groups correspond to topics (19)
if (length(grupos) != ncol(M1_top50)) {
  stop("Number of groups does not match number of topics.")
}

# Create data frame with topics and their respective groups
tópicos_com_grupos50 <- data.frame(Tópico = colnames(M1_top50), Grupo = grupos)

# Display data frame
print(tópicos_com_grupos50)

cluster_colors <- c(
  alpha("mediumpurple", 1),
  alpha("dodgerblue3", 1),
  alpha("darkorange1", 1),
  alpha("firebrick1", 1),
  alpha("darkgreen", 1),
  alpha("lightseagreen", 1),
  alpha("gold", 1),
  alpha("rosybrown2", 1)
)

par(lwd = 2)
par(mar = c(1, 4, 1, 2))

# Cut clustering tree to get groups
sub_grp <- cutree(cluster_hierarquico50, h = grupos)

# Plot dendrogram
plot(cluster_hierarquico50, axes = TRUE, main = NULL, xlab = NULL, ylab = "Dissimilarity")

# Add rectangles to indicate groups
rect.hclust(cluster_hierarquico50, k = 7, border = cluster_colors)


####
#### TOPIC PROPORTION PER YEAR ####
###


library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)

# Topic distribution per document
topic_distribution <- M3 # already a DF with "Year" column
colnames(topic_distribution)[colnames(topic_distribution) == "V20"] <- "Year"

# Ensure Year column is numeric
topic_distribution$Year <- as.numeric(topic_distribution$Year)

# Define custom intervals
breaks <- c(1958, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024) # Interval limits
labels <- c(
  "1958-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009",
  "2010-2014", "2015-2019", "2020-2024"
) # Interval names

# Create Year_Group column with custom intervals
topic_distribution$Year_Group <- cut(topic_distribution$Year,
  breaks = breaks,
  labels = labels,
  right = TRUE,
  include.lowest = TRUE
)

# Calculate average topic proportions for each custom interval
average_topic_proportions <- topic_distribution %>%
  group_by(Year_Group) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Rename columns for easier visualization
colnames(average_topic_proportions) <- c("Year", topic_names)

# Transform dataframe to long format for easier plotting
average_topic_long <- reshape2::melt(average_topic_proportions,
  id.vars = "Year",
  variable.name = "Topic",
  value.name = "Proportion"
)

# Filter valid data
average_topic_long_filtered <- average_topic_long[average_topic_long$Proportion
>= 0 & average_topic_long$Proportion <= 1, ]

# Check for missing values after filtering
print(sum(is.na(average_topic_long_filtered$Proportion)))


# Function to calculate linear regression slope
calculate_slope <- function(data) {
  model <- lm(Proportion ~ as.numeric(Year), data = data)
  return(coef(model)[2]) # Returns slope
}

# Calculate slope for each topic
slopes <- average_topic_long_filtered %>%
  group_by(Topic) %>%
  summarise(Slope = calculate_slope(cur_data()))

# Add column with color based on slope
slopes <- slopes %>%
  mutate(Color = case_when(
    Slope > 0 ~ "#0000FF", # Positive trend
    Slope < 0 ~ "#FF0000", # Negative trend
    TRUE ~ "black" # Neutral trend
  ))

# Merge slope data back to original dataset
average_topic_long_filtered <- merge(average_topic_long_filtered, slopes, by = "Topic")

# Sort Topic column alphabetically
unique(average_topic_long_filtered$Topic)

# Reassign Topic levels alphabetically
average_topic_long_filtered$Topic <- factor(average_topic_long_filtered$Topic,
  levels = sort(levels(average_topic_long_filtered$Topic))
)

# Drop levels that are no longer present
average_topic_long_filtered$Topic <- droplevels(average_topic_long_filtered$Topic)

# Check levels
levels(average_topic_long_filtered$Topic)
str(average_topic_long_filtered)

par(lwd = 1)
# Create faceted plot for each topic and add trend line with custom colors
p <- ggplot(average_topic_long_filtered, aes(x = Year, y = Proportion, group = Topic)) +
  geom_line(size = 1.5) + # Main line
  geom_point(size = 2) + # Points
  geom_smooth(method = "lm", se = FALSE, aes(color = Color), linetype = "dashed", size = .8) + # Colored trend line
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean Proportion"
  ) +
  theme(legend.position = "none") + # Remove legend since each plot has its own title
  scale_x_discrete(breaks = labels) +
  facet_wrap(~ str_wrap(Topic, width = 24), scales = "free_y", ncol = 5) + # Line break in facet titles
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + # Expand upper limit slightly
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, colour = "black"), # Rotate X-axis labels
    axis.title.y = element_text(size = 14), # Increase Y-axis title
    axis.text.y = element_text(size = 12, colour = "black"), # Increase Y-axis label size
    strip.text = element_text(size = 14)
  ) # Aumentar o tamanho do título de cada facet
plot(p)
ggsave("data/grafico_facets.jpg",
  plot = p,
  width = 14, height = 10
)


####
#### TOPIC GENERALITY-SPECIFICITY ####
###


M1 <- t(modeltools::posterior(ldaOut19)$terms) # word x topic
M2 <- modeltools::posterior(ldaOut19)$topics # article x topic
M3 <- as.data.frame(cbind(M2, dat$Publication.Year))
generality <- lapply(c(1:ldaOut19@k), function(a, dist, topics) {
  x <- as.numeric(M2[, a])
  c(included = mean(x[topics == a]), excluded = mean(x[topics != a]))
}, topics = topics(ldaOut19), dist = M2)
generality_result <- as.data.frame(do.call(rbind, generality))
generality_result$topic <- c(1:ldaOut19@k)
generality_result$topic <- topic_names

# ggplot
library(ggplot2)
library(wesanderson)
library(ggrepel)


# Prepare the data frame
generality_result <- as.data.frame(do.call(rbind, generality))
generality_result$topic <- c(1:ldaOut19@k)
generality_result$topic <- topic_names
generality_result$gradient <- with(generality_result, included - excluded)


# Create the plot
ggplot(generality_result, aes(x = excluded, y = included)) +
  scale_x_continuous(limits = c(0.035, 0.050)) +
  scale_y_continuous(limits = c(0.15, 0.30)) +
  geom_point(aes(color = gradient), size = 5) +
  scale_color_gradientn(
    colors = wes_palette("Zissou1", n = 5, type = "continuous"),
    labels = function(x) format(round(x, 2), nsmall = 2), guide = guide_colourbar(title = NULL, ticks = FALSE, barheight = 5)
  ) +
  geom_text_repel(aes(
    label = topic, # segment.color = NA,
    hjust = ifelse(topic == "Cardiac arrest response", 0.22, ifelse(topic == "Cold chain maintenance", -0.10, ifelse(topic == "Topic B", 0.7, 0.7))),
    vjust = ifelse(topic == "Topic A", 1.8, ifelse(topic == "Topic B", 1.8, 2))
  ), fontface = 1, cex = 3.5) +
  labs(x = "Mean weight (unselected articles)", y = "Mean weight (selected articles)") +
  theme_minimal() +
  annotate("text", x = 0.052, y = 0.23, label = "Specific topics", fontface = 2, size = 4, angle = 0) +
  annotate("text", x = 0.052, y = 0.20, label = "General topics", fontface = 2, size = 4, angle = 0) +
  theme(legend.position = c(0.85, 0.5))


####
#### GAPS CO-OCCURENCE  ####
###


M1 <- t(modeltools::posterior(ldaOut19)$terms) # word x topic
M2 <- modeltools::posterior(ldaOut19)$topics # article x topic
M3 <- as.data.frame(cbind(M2, dat$Publication.Year))

# Topic distribution per document
topic_distribution <- posterior(ldaOut19)$topics # equal to M2
# colnames(topic_distribution) <- topic_names

# Initialize co-occurrence matrix
num_topics <- ncol(topic_distribution)
co_occurrence_matrix <- matrix(0, nrow = num_topics, ncol = num_topics)
colnames(co_occurrence_matrix) <- topic_names
row.names(co_occurrence_matrix) <- topic_names

# Calculate co-occurrences by multiplying probability of topic pairs
for (i in 1:num_topics) {
  for (j in 1:num_topics) {
    co_occurrence_values <- numeric(nrow(topic_distribution)) # Stores co-occurrences for each article

    for (k in 1:nrow(topic_distribution)) {
      prob_T1 <- topic_distribution[k, i]
      prob_T2 <- topic_distribution[k, j]

      # Multiply probability of occurrence of both topics
      co_occurrence_values[k] <- prob_T1 * prob_T2
    }

    # Calculate mean co-occurrence for topic pair
    co_occurrence_matrix[i, j] <- mean(co_occurrence_values, na.rm = TRUE)
  }
}

# Normalize matrix values so max value is 1
max_co_occurrence <- max(co_occurrence_matrix)
co_occurrence_matrix <- co_occurrence_matrix / max_co_occurrence

# Create triangular heatmap
library(ggplot2)
library(reshape2)

# Transform matrix to long format data.frame for ggplot2
co_occurrence_df <- melt(co_occurrence_matrix)
colnames(co_occurrence_df) <- c("Topic1", "Topic2", "CoOccurrence")

# Convert Topic1 and Topic2 columns to character
co_occurrence_df$Topic1 <- as.character(co_occurrence_df$Topic1)
co_occurrence_df$Topic2 <- as.character(co_occurrence_df$Topic2)

# Remove redundant values to create triangular heatmap
co_occurrence_df <- co_occurrence_df[co_occurrence_df$Topic1 < co_occurrence_df$Topic2, ]

# Create triangular heatmap to identify knowledge gaps
ggplot(co_occurrence_df, aes(x = factor(Topic1), y = factor(Topic2), fill = CoOccurrence)) +
  geom_tile() +
  geom_text(aes(label = round(CoOccurrence, 2)), color = "black", size = 4) + # Add co-occurrence values
  scale_fill_gradient(low = "white", high = "red") +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Co-occurence"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(hjust = 1, size = 12, colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, colour = "black")
  )
