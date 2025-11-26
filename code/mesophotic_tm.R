####

## R Code used in the manuscript by Banha et al. (submitted to Coral Reefs)

####


#### creating the model ####

library(bibliometrix)
library(tm)
library(slam)
library(topicmodels)
library(readxl)

# loading data
dat <- read.csv("data/topmod_mesophotic.csv", stringsAsFactors = F)
dat$text <- paste(dat$Article.Title, dat$Author.Keywords, dat$Abstract, sep = " ")
DF <- as.data.frame(cbind(dat$Paper.ID, dat$text))
names(DF) <- c("doc_id", "text")
CPmatrix <- VCorpus(DataframeSource(DF))
CPmatrix <- tm_map(CPmatrix, content_transformer(tolower))

# remove potentially problematic symbols
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

# remove punctuation
CPmatrix <- tm_map(CPmatrix, removePunctuation)
# Strip digits
CPmatrix <- tm_map(CPmatrix, removeNumbers)
# remove stopwords
CPmatrix <- tm_map(CPmatrix, removeWords, stopwords("english"))
# remove whitespace
CPmatrix <- tm_map(CPmatrix, stripWhitespace)

# check the data
writeLines(as.character(CPmatrix[[2]]))

remove_whole_words <- content_transformer(function(text, words) {
  pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
  gsub(pattern, "", text)
})

text.processed <- data.frame(text = sapply(CPmatrix, identity), stringsAsFactors = F)
text.processed <- t(text.processed)
write.csv(text.processed, "data/text.processed.csv")

# create column names in excel
text.processed <- read.csv("data/text.processed.csv", row.names = 1)

content <- dat$text


library(ngram)
ng <- ngram(text.processed$content, n = 2)
get.phrasetable(ng)[1:150, ]

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


# saveRDS(CPmatrix, 'CPmatrix.rds')
CPmatrix <- readRDS("CPmatrix.rds")

DTMmatrix <- DocumentTermMatrix(CPmatrix)
dim(DTMmatrix)


which(col_sums(DTMmatrix) == 555)

# Cutoff terms
summary(col_sums(DTMmatrix))
table(col_sums(DTMmatrix))
DTMmatrix <- DTMmatrix[, col_sums(DTMmatrix) > 20]
DTMmatrix <- DTMmatrix[, col_sums(DTMmatrix) < 650]

col_sums(DTMmatrix)[order(names((col_sums(DTMmatrix))))]

saveRDS(DTMmatrix, "DTMmatrix.rds")
DTMmatrix <- readRDS("DTMmatrix.rds")

# eigth to late script
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
write.csv(ldaOut.terms, "model19.csv")
saveRDS(ldaOut19, "ldaOut19.rds")
M19 <- modeltools::posterior(ldaOut19)$topics # article x topic
write.csv(M19, "Articles_m19.csv")


# BANHA ####


#### Run the model ####

# mac
setwd("/Users/thomas/Library/CloudStorage/GoogleDrive-sotobanha@gmail.com/My Drive/#Doutorado/##Data/Topic Modeling")


library(bibliometrix)
library(tm)
library(slam)
library(topicmodels)
library(readxl)

# install.packages("readxl")


# Open the data

ldaOut19 <- readRDS("data/ldaOut19.rds")
dat <- read_excel("data/topmod_mesophotic.xlsx")


# labels
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
M3 <- as.data.frame(cbind(M2, dat$Publication.Year))

####
#### ARTICLES PER YEAR WITH ACCUMULATION ####
####

# Artigos por ano com acumulação
dat <- read_excel("topmod_mesophotic.xlsx")


library(ggplot2)
library(dplyr)

# N de artigos

# Supondo que 'dat' é seu DataFrame que contém a coluna "Publication.Year"
# Calcular o número total de artigos por ano
total_articles_per_year <- dat %>%
  group_by(Publication.Year) %>%
  summarise(total_articles = n(), .groups = "drop") # Resumir o total de artigos por ano


## JUNTANDO OS DOIS

# Calcular o número de artigos por ano e a acumulação
artigos_por_ano <- dat %>%
  group_by(Publication.Year) %>%
  summarise(n_artigos = n(), .groups = "drop") %>%
  mutate(acumulado = cumsum(n_artigos)) # Calcular a acumulação

# Definir intervalo de anos
anos_intervalo <- c(1958, seq(1990, 2020, by = 10), 2024)

par(lwd)
# Criar gráfico com dois eixos y
par(mar = c(5, 5, 2, 5)) # Ajustar margens do gráfico

# Gráfico de barras
barplot_heights <- barplot(artigos_por_ano$n_artigos,
  names.arg = artigos_por_ano$Publication.Year, # Exibir todos os anos
  col = "white", border = "black",
  ylim = c(0, max(artigos_por_ano$n_artigos) + 5), # Ajustar limite do eixo y
  xlab = NULL, ylab = "# Articles published per year",
  xaxt = "n", # Não desenhar o eixo x automaticamente
  # cex.names = 1,  # Aumenta o tamanho dos rótulos no eixo x
  cex.lab = 1.3, # Aumenta o tamanho do título do eixo y
  cex.axis = 1.3
) # Aumenta o tamanho dos rótulos no eixo y
# Adicionar rótulos apenas para os intervalos desejados
posicoes_intervalo <- match(anos_intervalo, artigos_por_ano$Publication.Year)

# Adicionar rótulos de intervalo alinhados com as barras e passando pelo zero
axis(1,
  at = barplot_heights[posicoes_intervalo],
  labels = anos_intervalo, tick = TRUE, line = 0, cex.axis = 1.3
) # Adicionar rótulos de intervalo

# Adicionar asterisco acima da barra de 2024
text(
  x = barplot_heights[which(artigos_por_ano$Publication.Year == 2024)],
  y = artigos_por_ano$n_artigos[which(artigos_por_ano$Publication.Year == 2024)] + 5,
  labels = "*", cex = 3, col = "black"
)

# Adicionar uma linha no eixo x no valor zero
abline(h = 0, col = "black") # Linha no zero para referência

# Gráfico da curva de acumulação
par(new = TRUE) # Permitir sobreposição do gráfico
plot(artigos_por_ano$Publication.Year,
  artigos_por_ano$acumulado,
  type = "l", col = "red", lwd = 3,
  ylim = c(0, max(artigos_por_ano$acumulado) + 200), # Ajustar limite do eixo y
  ylab = "", xlab = "", axes = FALSE
) # Não desenhar os eixos

# Adicionar eixo y direito para acumulação
axis(4, col.axis = "black", cex.axis = 1.3) # Eixo y direito em vermelho
mtext("# Articles accumulated", side = 4, line = 3, col = "black", cex = 1.3) # Rótulo do eixo y direito


####
#### ARTICLES PER TOPICS ####
####

M2tops <- M2
colnames(M2tops) <- topic_names


# Identifica o índice do tópico com a maior proporção em cada artigo
principal_topic <- apply(M2tops, 1, which.max)

# Conta quantas vezes cada tópico é o principal
topico_counts <- table(principal_topic)

# Cria uma tabela com o número de vezes que cada tópico foi o principal
topico_counts_df <- data.frame(
  Topico = as.numeric(names(topico_counts)),
  Quantidade_de_artigos = as.vector(topico_counts)
)

# Exibe o resultado
print(topico_counts_df)

# Soma o número total de artigos
total_artigos <- sum(topico_counts)

# Exibe o total de artigos
print(total_artigos)

###
# ARTICLES PER TOPICS BAR PLOT
###

# Pacotes necessários
library(ggplot2)
library(vegan) # Para o vegdist e clustering

# Supondo que o `topico_counts_df` contém:
# Colunas: 'Topico' (número do tópico) e 'Quantidade_de_artigos'

# Ordenando pela quantidade de artigos de forma decrescente
topico_counts_df <- topico_counts_df[order(-topico_counts_df$Quantidade_de_artigos), ]

# Atribuindo os clusters aos tópicos
topico_counts_df$Cluster <- as.factor(sub_grp[topico_counts_df$Topico])

# Converte o número do tópico para o nome do tópico usando o objeto topic_names
topico_counts_df$Nome_do_Topico <- topic_names[topico_counts_df$Topico]

###
# BARRAS Pretas
###

# Pacotes necessários
library(ggplot2)
library(vegan) # Para o vegdist e clustering

# Supondo que o `topico_counts_df` contém:
# Colunas: 'Topico' (número do tópico) e 'Quantidade_de_artigos'

# Ordenando pela quantidade de artigos de forma decrescente
topico_counts_df <- topico_counts_df[order(-topico_counts_df$Quantidade_de_artigos), ]

# Converte o número do tópico para o nome do tópico usando o objeto topic_names
topico_counts_df$Nome_do_Topico <- topic_names[topico_counts_df$Topico]

# Criando o gráfico de barras, removendo as cores dos clusters e colocando bordas pretas
ggplot(topico_counts_df, aes(y = reorder(Nome_do_Topico, Quantidade_de_artigos), x = Quantidade_de_artigos)) +
  geom_bar(stat = "identity", fill = NA, color = "black", size = 1, width = 0.85) + # Barras vazadas e bordas pretas
  labs(y = NULL, x = "Main topic (# articles)", title = NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks()) + # Adicionar tick marks no eixo
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16, angle = 0, hjust = 1, colour = "black"),
    axis.text.x = element_text(size = 14, colour = "black"),
    axis.title.x = element_text(size = 14), # Aumenta o tamanho do título do eixo X
    axis.ticks.x = element_line(size = 0.5, color = "black") # Adicionar *tick marks* no eixo X
  )


# COLORIDO
# Criando o gráfico de barras, mapeando color para o Cluster e removendo o preenchimento
# ggplot(topico_counts_df, aes(y = reorder(Nome_do_Topico, Quantidade_de_artigos), x = Quantidade_de_artigos, color = Cluster)) +
geom_bar(stat = "identity", fill = NA, size = 1) + # Preenchimento vazio e borda mais espessa
  scale_color_manual(values = cluster_colors) + # Definindo as cores dos clusters para as bordas
  labs(y = NULL, x = "Main topic (# articles)", title = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, angle = 0, hjust = 1)) # Ajustando o texto do eixo Y


####
#### TOP 50 SIMILARITY - CLUSTER ####
####


# MAKING THE TOP 50 MATRIX

# Supondo que M1 é um data frame onde as linhas são palavras e as colunas são tópicos (probabilidades)

# Inicializar uma lista para armazenar as palavras únicas
palavras_selecionadas50 <- c()

# Loop sobre cada tópico (coluna) para identificar as 50 palavras com as maiores probabilidades
for (i in 1:19) {
  # Ordenar as palavras pelo tópico atual (i) em ordem decrescente de probabilidade
  top50_palavras <- rownames(M1)[order(M1[, i], decreasing = TRUE)]

  # Selecionar as 50 palavras mais importantes
  top50_unicas <- top50_palavras[1:50]

  # Adicionar as palavras à lista de palavras selecionadas
  palavras_selecionadas50 <- unique(c(palavras_selecionadas50, top50_unicas))
}

# Criar uma nova matriz vazia M1_top50
M1_top50 <- data.frame(matrix(0, nrow = length(palavras_selecionadas50), ncol = 19))
rownames(M1_top50) <- palavras_selecionadas50 # Definir as palavras como nomes das linhas

# Preencher a matriz M1_top50 com as probabilidades
for (palavra in palavras_selecionadas50) {
  for (i in 1:19) {
    M1_top50[palavra, i] <- M1[palavra, i] # Atribuir a probabilidade correspondente
  }
}

# Nomear as colunas de 1 a 19
colnames(M1_top50) <- topic_names

# Salvando a planilha com as top 50 palavras

# Inicializar uma lista para armazenar as palavras top 50 para cada tópico
top50_matrix <- matrix(NA, nrow = 50, ncol = 19)

# Loop sobre cada tópico (coluna) para identificar as 50 palavras com as maiores probabilidades
for (i in 1:19) {
  # Ordenar as palavras pelo tópico atual (i) em ordem decrescente de probabilidade
  top50_palavras <- rownames(M1)[order(M1[, i], decreasing = TRUE)]

  # Selecionar as 50 palavras mais importantes
  top50_matrix[, i] <- top50_palavras[1:50]
}

# Atribuir nomes às colunas da matriz
colnames(top50_matrix) <- topic_names

# Salvar a matriz em uma planilha chamada top50_words_mce.xlsx
write.csv(top50_matrix, file = "top50_words_mce.csv")

# Carregar o pacote openxlsx
library(openxlsx)

# Ler a planilha existente
top50_data <- read.csv("top50_words_mce.csv")

# Reorganizar as colunas em ordem alfabética
top50_data_sorted <- top50_data[, order(colnames(top50_data))]

# Salvar a planilha reorganizada em um novo arquivo ou sobrescrever o existente
write.xlsx(top50_data_sorted, file = "top50_words_mce_sorted.xlsx")



# Salvando a planilha com as probabilidades
write.csv(M1_top50, file = "top50_words_probability_mce.csv", row.names = TRUE)



# A nova matriz M1_top50 terá todas as palavras das 50 palavras mais importantes de cada tópico e suas probabilidades

###
# DISSIMILARITY
###

library(vegan)
library(ggplot2)
# Calcular a matriz de dissimilaridade usando Bray-Curtis
dissimilaridade_bray_curtis50 <- vegdist(t(M1_top50), method = "bray")


# Visualizar a matriz de dissimilaridade
print(dissimilaridade_bray_curtis50)

####
# CLUSTER
###

# Supondo que dissimilaridade_bray_curtis já foi calculada
# Realizar o agrupamento hierárquico
cluster_hierarquico50 <- hclust(dissimilaridade_bray_curtis50, method = "ward.D2") # Método de agrupamento pode ser alterado
# cluster_hierarquico_euc <- hclust(dissimilaridade_euclidean, method = "ward.D2")  # Método de agrupamento pode ser alterado


# Plotar o dendrograma
plot(cluster_hierarquico50, main = "Cluster BC top 50", xlab = "Tópicos", ylab = "Dissimilaridade")

# plot(cluster_hierarquico_euc, main = "Cluster Euclidean", xlab = "Tópicos", ylab = "Dissimilaridade")


# Primeiro, verifique a estrutura da dissimilaridade e do agrupamento
cat("Número de tópicos em M1_top50:", ncol(M1_top50), "\n") # deve ser 19
cat("Número de grupos:", length(grupos), "\n") # deve ser igual ao número de tópicos

# Se você já calculou os grupos usando cutree
# num_grupos <- 7 # ou o número que você deseja
grupos <- cutree(cluster_hierarquico50, h = 0.94)

# Certifique-se de que grupos corresponda aos tópicos (19)
if (length(grupos) != ncol(M1_top50)) {
  stop("O número de grupos não corresponde ao número de tópicos.")
}

# Criar um data frame com tópicos e seus respectivos grupos
tópicos_com_grupos50 <- data.frame(Tópico = colnames(M1_top50), Grupo = grupos)

# Exibir o data frame
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

# Cortar a árvore de agrupamento para obter os grupos
sub_grp <- cutree(cluster_hierarquico50, h = grupos)

# Plotar o dendrograma
plot(cluster_hierarquico50, axes = TRUE, main = NULL, xlab = NULL, ylab = "Dissimilarity")

# Adicionar retângulos para indicar os grupos
rect.hclust(cluster_hierarquico50, k = num_grupos, border = cluster_colors) # Ajuste as cores conforme necessário


####
#### TOPIC PROPORTION PER YEAR ####
####


library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)

# Topic distribution per document
topic_distribution <- M3 # já é DF e tem a coluna "Year"
colnames(topic_distribution)[colnames(topic_distribution) == "V20"] <- "Year"

# Garantir que a coluna Year é numérica
topic_distribution$Year <- as.numeric(topic_distribution$Year)

# Definir os intervalos personalizados
breaks <- c(1958, 1989, 1994, 1999, 2004, 2009, 2014, 2019, 2024) # Limites dos intervalos
labels <- c(
  "1958-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009",
  "2010-2014", "2015-2019", "2020-2024"
) # Nomes dos intervalos

# Criar a coluna Year_Group com os intervalos personalizados
topic_distribution$Year_Group <- cut(topic_distribution$Year,
  breaks = breaks,
  labels = labels,
  right = TRUE,
  include.lowest = TRUE
)

# Calcular as médias das proporções de tópicos para cada intervalo personalizado
average_topic_proportions <- topic_distribution %>%
  group_by(Year_Group) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Renomear as colunas para facilitar a visualização
colnames(average_topic_proportions) <- c("Year", topic_names)

# Transformar o dataframe para formato longo para facilitar o gráfico
average_topic_long <- reshape2::melt(average_topic_proportions,
  id.vars = "Year",
  variable.name = "Topic",
  value.name = "Proportion"
)

# Filtrar dados válidos
average_topic_long_filtered <- average_topic_long[average_topic_long$Proportion >= 0 & average_topic_long$Proportion <= 1, ]

# Verificar se ainda há valores ausentes após a filtragem
print(sum(is.na(average_topic_long_filtered$Proportion)))


# Função para calcular a inclinação da regressão linear
calculate_slope <- function(data) {
  model <- lm(Proportion ~ as.numeric(Year), data = data)
  return(coef(model)[2]) # Retorna a inclinação (slope)
}

# Calcular a inclinação para cada tópico
slopes <- average_topic_long_filtered %>%
  group_by(Topic) %>%
  summarise(Slope = calculate_slope(cur_data()))

# Adicionar uma coluna com a cor baseada na inclinação
slopes <- slopes %>%
  mutate(Color = case_when(
    Slope > 0 ~ "#0000FF", # Tendência negativa
    Slope < 0 ~ "#FF0000", # Tendência positiva
    TRUE ~ "black" # Tendência quase nula
  ))

# Juntar os dados de inclinação de volta ao conjunto de dados original
average_topic_long_filtered <- merge(average_topic_long_filtered, slopes, by = "Topic")

# Ordenar a coluna Topic em ordem alfabética
unique(average_topic_long_filtered$Topic)

# Reatribuir os níveis da coluna Topic em ordem alfabética
average_topic_long_filtered$Topic <- factor(average_topic_long_filtered$Topic,
  levels = sort(levels(average_topic_long_filtered$Topic))
)

# Dropar níveis que não estão mais presentes
average_topic_long_filtered$Topic <- droplevels(average_topic_long_filtered$Topic)

# verificar níveis
levels(average_topic_long_filtered$Topic)

str(average_topic_long_filtered)

par(lwd = 1)
# Criar o gráfico com facetas para cada tópico e adicionar a linha de tendência com cores personalizadas
p <- ggplot(average_topic_long_filtered, aes(x = Year, y = Proportion, group = Topic)) +
  geom_line(size = 1.5) + # Linha principal
  geom_point(size = 2) + # Pontos
  geom_smooth(method = "lm", se = FALSE, aes(color = Color), linetype = "dashed", size = .8) + # Linha de tendência colorida
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL,
    y = "Mean Proportion"
  ) +
  theme(legend.position = "none") + # Remover a legenda já que cada gráfico tem seu próprio título
  scale_x_discrete(breaks = labels) +
  facet_wrap(~ str_wrap(Topic, width = 24), scales = "free_y", ncol = 5) + # Quebra de linha nos títulos dos facets
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + # Expandir um pouco o limite superior
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, colour = "black"), # Rotacionar os rótulos do eixo X
    axis.title.y = element_text(size = 14), # Aumentar o título do eixo Y
    axis.text.y = element_text(size = 12, colour = "black"), # Aumentar o tamanho dos rótulos do eixo Y
    strip.text = element_text(size = 14)
  ) # Aumentar o tamanho do título de cada facet
plot(p)
ggsave("grafico_facets5.jpg",
  plot = p,
  width = 14, height = 10
) # Ajuste width e height conforme necessário, maior height mais ingrime


###
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
####

####
# USING NUMBERS
####


M1 <- t(modeltools::posterior(ldaOut19)$terms) # word x topic
M2 <- modeltools::posterior(ldaOut19)$topics # article x topic
M3 <- as.data.frame(cbind(M2, dat$Publication.Year))

# Usando a matriz de distribuição dos tópicos M2tops
topic_distribution <- M2 # Igual à sua matriz de proporções
num_topics <- ncol(topic_distribution)

# Inicializando a matriz de co-ocorrência
co_occurrence_matrix <- matrix(0, nrow = num_topics, ncol = num_topics)
colnames(co_occurrence_matrix) <- 1:num_topics
row.names(co_occurrence_matrix) <- 1:num_topics

# Calcular as co-ocorrências multiplicando a probabilidade de ocorrência dos pares de tópicos
for (i in 1:num_topics) {
  for (j in 1:num_topics) {
    co_occurrence_values <- numeric(nrow(topic_distribution)) # Armazena co-ocorrências para cada artigo

    for (k in 1:nrow(topic_distribution)) {
      prob_T1 <- topic_distribution[k, i]
      prob_T2 <- topic_distribution[k, j]

      # Multiplicar a probabilidade de ocorrência dos dois tópicos
      co_occurrence_values[k] <- prob_T1 * prob_T2
    }

    # Calcular a média de co-ocorrência para o par de tópicos
    co_occurrence_matrix[i, j] <- mean(co_occurrence_values, na.rm = TRUE)
  }
}

# Normalizar os valores da matriz para que o maior valor seja igual a 1
max_co_occurrence <- max(co_occurrence_matrix)
co_occurrence_matrix <- co_occurrence_matrix / max_co_occurrence

# Criar um heatmap triangular
library(ggplot2)
library(reshape2)

# Transformar a matriz em um data.frame no formato longo para o ggplot2
co_occurrence_df <- melt(co_occurrence_matrix)
colnames(co_occurrence_df) <- c("Topic1", "Topic2", "CoOccurrence")

# Remover valores redundantes para criar o heatmap triangular
co_occurrence_df <- co_occurrence_df[co_occurrence_df$Topic1 < co_occurrence_df$Topic2, ]

# Criar o heatmap triangular para identificar lacunas de conhecimento
ggplot(co_occurrence_df, aes(x = factor(Topic1), y = factor(Topic2), fill = CoOccurrence)) +
  geom_tile() +
  geom_text(aes(label = round(CoOccurrence, 2)), color = "black", size = 4) + # Adicionar os valores de co-ocorrência
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap de Co-Ocorrência dos Tópicos (Lacunas de Conhecimento)",
    x = NULL,
    y = NULL,
    fill = "Co-ocorrência"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####
# USING NAMES
####

M1 <- t(modeltools::posterior(ldaOut19)$terms) # word x topic
M2 <- modeltools::posterior(ldaOut19)$topics # article x topic
M3 <- as.data.frame(cbind(M2, dat$Publication.Year))

# Distribui??o dos t?picos por documento
topic_distribution <- posterior(ldaOut19)$topics # igual ao M2
# colnames(topic_distribution) <- topic_names

# Inicializando a matriz de co-ocorr?ncia
num_topics <- ncol(topic_distribution)
co_occurrence_matrix <- matrix(0, nrow = num_topics, ncol = num_topics)
colnames(co_occurrence_matrix) <- topic_names
row.names(co_occurrence_matrix) <- topic_names

# Calcular as co-ocorr?ncias multiplicando a probabilidade de ocorr?ncia dos pares de t?picos
for (i in 1:num_topics) {
  for (j in 1:num_topics) {
    co_occurrence_values <- numeric(nrow(topic_distribution)) # Armazena co-ocorr?ncias para cada artigo

    for (k in 1:nrow(topic_distribution)) {
      prob_T1 <- topic_distribution[k, i]
      prob_T2 <- topic_distribution[k, j]

      # Multiplicar a probabilidade de ocorr?ncia dos dois t?picos
      co_occurrence_values[k] <- prob_T1 * prob_T2
    }

    # Calcular a m?dia de co-ocorr?ncia para o par de t?picos
    co_occurrence_matrix[i, j] <- mean(co_occurrence_values, na.rm = TRUE)
  }
}

# Normalizar os valores da matriz para que o maior valor seja igual a 1
max_co_occurrence <- max(co_occurrence_matrix)
co_occurrence_matrix <- co_occurrence_matrix / max_co_occurrence

# Criar um heatmap triangular
library(ggplot2)
library(reshape2)

# Transformar a matriz em um data.frame no formato longo para o ggplot2
co_occurrence_df <- melt(co_occurrence_matrix)
colnames(co_occurrence_df) <- c("Topic1", "Topic2", "CoOccurrence")

# Converta as colunas Topic1 e Topic2 para caracteres
co_occurrence_df$Topic1 <- as.character(co_occurrence_df$Topic1)
co_occurrence_df$Topic2 <- as.character(co_occurrence_df$Topic2)

# Remover valores redundantes para criar o heatmap triangular
co_occurrence_df <- co_occurrence_df[co_occurrence_df$Topic1 < co_occurrence_df$Topic2, ]

# Criar o heatmap triangular para identificar lacunas de conhecimento
# ggplot(co_occurrence_df, aes(x = factor(Topic1), y = factor(Topic2), fill = CoOccurrence)) +
geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = "Co-occurence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Criar o heatmap triangular para identificar lacunas de conhecimento
ggplot(co_occurrence_df, aes(x = factor(Topic1), y = factor(Topic2), fill = CoOccurrence)) +
  geom_tile() +
  geom_text(aes(label = round(CoOccurrence, 2)), color = "black", size = 4) + # Adicionar os valores de co-ocorr?ncia
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


# GIRANDO 90 graus
ggplot(co_occurrence_df, aes(y = factor(Topic1), x = factor(Topic2), fill = CoOccurrence)) +
  geom_tile() +
  geom_text(aes(label = round(CoOccurrence, 2)), color = "black", size = 4) + # Adicionar os valores de co-ocorrência
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


# Criar o heatmap triangular para identificar lacunas de conhecimento com SIMILARIDADE - não tá na ordem
# article.dist <-dist(as.data.frame(t(M2)), method="euclidean", diag=FALSE, lower=FALSE)
# attr(article.dist, "Labels") <- topic_names

# scale.zero.one<-function(x){x<-x-min(x, na.rm=TRUE); x<-x*100; x<-x/max(x, na.rm=TRUE); return(x)}
# gap.width<-scale.zero.one(article.dist)
# ggcorrplot(as.matrix(gap.width), hc.order = TRUE, type = "upper", lab = TRUE, lab_size = 3.5, method = "square", colors = c("blue", "white", "red"))+
theme(axis.text.x = element_text(size = 10, color = "black"), axis.text.y = element_text(size = 11, color = "black")) +
  scale_fill_gradientn(
    name = "Dissimilarity", colors = c("white", "blue"), values = c(0, 0.25, 0.5, 0.75, 1),
    guide = guide_colorbar(breaks = c(0, 0.5, 1))
  )
