library(igraph)
library(ggcorrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

###data prep
#load data
data <- read.csv("normalized_imputed_pooled.csv")
tasks_del <- read.csv("tasks_threshold_70_to_delete.csv")

#remove designated tasks
data <- data[, !(names(data) %in% tasks_del$Task)]

# Convert to long format
data_long <- data %>%
  pivot_longer(-ID, names_to = "Task", values_to = "Value")

# Convert to wide format with IDs as columns
data_cda <- data_long %>%
  pivot_wider(names_from = ID, values_from = Value)

data_cda <- data_cda %>% select(-Task)

write.csv(data_cda, "cda_data.csv", row.names = FALSE)

###correlations
#correlation
corr_matrix = round(cor(data_cda, method = "spearman", use = "pairwise"),3)

#plot
corr_matrix_plot <- ggcorrplot(corr_matrix, show.legend = FALSE, lab = FALSE) + 
  theme_minimal() +                          # Use a minimal theme
  theme(
    panel.grid.major = element_blank(),      # Remove major grid lines
    panel.grid.minor = element_blank(),      # Remove minor grid lines
    panel.background = element_blank(),      # Remove the panel background
    axis.title = element_blank(),            # Remove axis titles
    axis.text = element_blank(),             # Remove axis text
    axis.ticks = element_blank()             # Remove axis ticks
  )

####ERIN
test = as.matrix(corr_matrix)
test[corr_matrix < 0]  = 0

ggcorrplot(test)

g.test = graph_from_adjacency_matrix(test, "undirected", weighted=TRUE, diag=FALSE) 

plot(g.test)
V(g.test)$name

E(g.test)
louvain.test = cluster_louvain(g.test)

modularity(louvain.test)

plot.igraph(g.test, vertex.label=V(g.test)$name, edge.width=E(g.test)$weight*5, edge.color = "black", vertex.color = louvain.test$membership)
png(filename=paste("edge_betweeness_plot_pos.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
#####Erin

#save files
ggsave(corr_matrix_plot, filename=paste("correlation.png",sep=""),width=10,height=10, bg = 'white')
write.csv(corr_matrix, "corr_matrix.csv")

###threshold
#threshold at the lowest max number
max_values <- apply(corr_matrix, 2, function(x) sort(x, decreasing = TRUE)[1:2])
max_values <- data.frame(max_values)
min_value <- min(max_values)

View(corr_matrix)
View(test)

corr_matrix_thres <- as.matrix(corr_matrix)
corr_matrix_thres[corr_matrix < 0.283]  = 0
ggcorrplot(corr_matrix_thres)
View(corr_matrix_thres)

#Save
write.csv(corr_matrix_thres, "thresholded_corr_matrix.csv")

### Network creation
mat = as.matrix(corr_matrix_thres)
g <- graph_from_adjacency_matrix(mat, "undirected", weighted=TRUE, diag = FALSE)

set.seed(111)

### CDA Network 
weights <- E(g)$weight
h = diversity(g, weights = weights, vids = V(g)) # Node-level entropy-based diversity
degree = degree(g, mode="total") # Node degree (number of connections)
g <- set_vertex_attr(g, "degree", value = degree) # Add degree as vertex attribute

#Basic network plot
network <- plot(g, edge.width=E(g)$weight*25, vertex.size=10, 
                 vertex.label = NA, vertex.color = "black", edge.color="gray55")

#Closeness centrality
plot.igraph(g, vertex.size=closeness(g),  
            vertex.label = NA, vertex.color = "black")   # Re-scaled by multiplying by 500
dev.off()

#Betweenness centrality
plot.igraph(g, vertex.size=betweenness(g)*.1, 
            vertex.label = NA, vertex.color = "black")    # Rescaled by multiplying by .15
dev.off()

## Conducting edge-betweenness clustering 
gn <- cluster_edge_betweenness(g, modularity = TRUE)
head(gn)

set.seed(111)

edge_betweeness_plot <- plot(g, vertex.label=NA, vertex.color = gn$membership)

png(filename=paste("edge_betweeness_plot.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
plot(g, vertex.label=V(g)$name, vertex.color = gn$membership, vertex.label = NA)
dev.off()

plot_dendrogram(gn, mode = igraph_opt("dend.plot.type"))

modularity(gn) ## 0.5578
length(gn) ## n clusters
membership(gn)
sizes(gn)

## Louvain Clustering ##

louvain_clusters <- cluster_louvain(g)
head(louvain_clusters)

constraint.scores = constraint(g, nodes = V(g), weights = E(g)$weight)
edge.attributes(g)

compare(louvain_clusters, gn)


dev.off()

plot(g, vertex.label = NA, edge.width=E(g)$weight*5, edge.color = "black", vertex.color = louvain_clusters$membership)

png(filename=paste("louvain_clusters_optimal_plot.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
plot(g, vertex.label=V(g)$name, edge.width=E(g)$weight*25, vertex.color = louvain_clusters$membership, 
     vertex.label.family = "Arial", vertex.label.color="black", vertex.label.cex=c(1.2))
dev.off()

modularity(louvain_clusters) ##  modularity 0.561642
length(louvain_clusters) ## n clusters
membership(louvain_clusters)
sizes(louvain_clusters)

scoring_functions(g, membership(louvain_clusters), type = "local", weighted = TRUE)
scoring_functions(g, membership(louvain_clusters), type = "global", weighted = TRUE)



#######POSITIVE########################
corr_matrix_thres_pos <- as.matrix(corr_matrix)
corr_matrix_thres_pos[corr_matrix < 0]  = 0
ggcorrplot(corr_matrix_thres_pos)
View(corr_matrix_thres_pos)

#Save
write.csv(corr_matrix_thres_pos, "thresholded_corr_matrix_pos.csv")

### Network creation
mat_pos = as.matrix(corr_matrix_thres_pos)
g_pos <- graph_from_adjacency_matrix(mat_pos, "undirected", weighted=TRUE, diag = FALSE)

set.seed(111)

### CDA Network 
weights_pos <- E(g_pos)$weight
h_pos = diversity(g_pos, weights = weights_pos, vids = V(g_pos)) # Node-level entropy-based diversity
degree_pos = degree(g_pos, mode="total") # Node degree (number of connections)
g_pos <- set_vertex_attr(g_pos, "degree", value = degree_pos) # Add degree as vertex attribute

#Basic network plot
network_pos <- plot(g_pos, edge.width=E(g_pos)$weight*25, vertex.size=10, 
                vertex.label = NA, vertex.color = "black", edge.color="gray55")

#Closeness centrality
plot.igraph(g_pos, vertex.size=closeness(g_pos)*5,  
            vertex.label = NA, vertex.color = "black")   # Re-scaled by multiplying by 5
dev.off()

#Betweenness centrality
plot.igraph(g_pos, vertex.size=betweenness(g_pos)*.1, 
            vertex.label = NA, vertex.color = "black")    # Rescaled by multiplying by .1
dev.off()

## Conducting edge-betweenness clustering 
gn_pos <- cluster_edge_betweenness(g_pos, modularity = TRUE)
head(gn_pos)

set.seed(111)

edge_betweeness_plot_pos <- plot(g_pos, vertex.label=V(g_pos)$name, vertex.color = gn_pos$membership)

png(filename=paste("edge_betweeness_plot_pos.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
plot(g_pos, vertex.label=V(g_pos)$name, vertex.color = gn_pos$membership)
dev.off()

plot_dendrogram(gn_pos, mode = igraph_opt("dend.plot.type"))

modularity(gn_pos) ## 0.5578
length(gn_pos) ## n clusters
membership(gn_pos)
sizes(gn_pos)

## Louvain Clustering ##

louvain_clusters_pos <- cluster_louvain(g_pos)
head(louvain_clusters_pos)

constraint.scores = constraint(g_pos, nodes = V(g_pos), weights = E(g_pos)$weight)
edge.attributes(g_pos)

compare(louvain_clusters_pos, gn_pos)


dev.off()

plot(g_pos, vertex.label=V(g_pos)$name, edge.width=E(g_pos)$weight*5, edge.color = "black", vertex.color = louvain_clusters_pos$membership)

png(filename=paste("louvain_clusters_optimal_plot_pos.png",sep=""), width = 1000, height = 1000, units = "px", bg = 'white')
plot(g_pos, vertex.label=V(g_pos)$name, edge.width=E(g_pos)$weight*25, vertex.color = louvain_clusters_pos$membership, 
     vertex.label.family = "Arial", vertex.label.color="black", vertex.label.cex=c(1.2))
dev.off()

modularity(louvain_clusters_pos) ##  modularity 0.561642
length(louvain_clusters_pos) ## n clusters
membership(louvain_clusters_pos)
sizes(louvain_clusters_pos)

scoring_functions(g_pos, membership(louvain_clusters_pos), type = "local", weighted = TRUE)
scoring_functions(g_pos, membership(louvain_clusters_pos), type = "global", weighted = TRUE)


###
names_gn <- gn$names
membership_gn <- gn$membership

membership_normalized_tmp <- data.frame(names_gn,membership_gn)

membership_normalized <- data %>%
  left_join(membership_normalized_tmp, by = c("ID" = "names_gn"))

write.csv(membership_normalized, "membership_normalized.csv", row.names = FALSE)

names_gn_pos <- gn_pos$names
membership_gn_pos <- gn_pos$membership

membership_normalized_pos_tmp <- data.frame(names_gn_pos,membership_gn_pos)

membership_normalized_pos <- data %>%
  left_join(membership_normalized_pos_tmp, by = c("ID" = "names_gn_pos"))


write.csv(membership_normalized_pos, "membership_normalized_pos.csv", row.names = FALSE)

membership_normalized <- read.csv("membership_normalized.csv")
membership_normalized_pos <- read.csv("membership_normalized_pos.csv")
### Visualize
membership_normalized_names <- names(membership_normalized)
membership_normalized_names <- membership_normalized_names[membership_normalized_names != "ID"]
membership_normalized_names <- membership_normalized_names[membership_normalized_names != "membership_gn"]

# Step 1: Reshape the data from wide to long format
membership_normalized_long <- membership_normalized %>%
  pivot_longer(
    cols = all_of(membership_normalized_names),
    names_to = "task",
    values_to = "score"
  )

# Step 2: Compute average score for each task by membership
membership_normalized_summary <- membership_normalized_long %>%
  group_by(membership_gn, task) %>%
  summarize(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

membership_normalized_summary$membership_gn <- as.factor(membership_normalized_summary$membership_gn)

# Step 3: Plot the sideways bar graph
ggplot(membership_normalized_summary, aes(x = avg_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Task Score by Membership",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()



membership_normalized_pos_names <- names(membership_normalized_pos)
membership_normalized_pos_names <- membership_normalized_pos_names[membership_normalized_pos_names != "ID"]
membership_normalized_pos_names <- membership_normalized_pos_names[membership_normalized_pos_names != "membership_gn_pos"]

# Step 1: Reshape the data from wide to long format
membership_normalized_pos_long <- membership_normalized_pos %>%
  pivot_longer(
    cols = all_of(membership_normalized_pos_names),
    names_to = "task",
    values_to = "score"
  )

# Step 2: Compute average score for each task by membership
membership_normalized_pos_summary <- membership_normalized_pos_long %>%
  group_by(membership_gn_pos, task) %>%
  summarize(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

membership_normalized_pos_summary$membership_gn_pos <- as.factor(membership_normalized_pos_summary$membership_gn_pos)

# Step 3: Plot the sideways bar graph
ggplot(membership_normalized_pos_summary, aes(x = avg_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Task Score by Membership",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()


### Adding cognitive constructs
cda_constructs <- read.csv("cda_constructs.csv")

#Filter tasks where language == 1
tasks_with_language <- cda_constructs %>%
  filter(Language == 1) %>%
  pull(Task)

tasks_with_visual <- cda_constructs %>%
  filter(Visual == 1) %>%
  pull(Task)

tasks_with_executive <- cda_constructs %>%
  filter(Executive == 1) %>%
  pull(Task)

tasks_with_auditory <- cda_constructs %>%
  filter(Auditory == 1) %>%
  pull(Task)

tasks_with_semantic <- cda_constructs %>%
  filter(Semantic == 1) %>%
  pull(Task)

tasks_with_visuomotor <- cda_constructs %>%
  filter(visuomotor == 1) %>%
  pull(Task)

# Step 2: Reshape membership_normalized to long format
long_data_auditory <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_auditory))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

long_data_executive <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_executive))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

long_data_language <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_language))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

long_data_semantic <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_semantic))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

long_data_visual <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_visual))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

long_data_visuomotor <- membership_normalized %>%
  select(all_of(c("membership_gn", tasks_with_visuomotor))) %>%
  pivot_longer(cols = -membership_gn, names_to = "task", values_to = "score")

# Step 3: Calculate average score for each task
avg_scores_auditory <- long_data_auditory %>%
  group_by(membership_gn, task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_executive <- long_data_executive %>%
  group_by(membership_gn,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_language <- long_data_language %>%
  group_by(membership_gn,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_semantic <- long_data_semantic %>%
  group_by(membership_gn,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_visual <- long_data_visual %>%
  group_by(membership_gn,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_visuomotor <- long_data_visuomotor %>%
  group_by(membership_gn,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_auditory$membership_gn <- as.factor(avg_scores_auditory$membership_gn)
avg_scores_executive$membership_gn <- as.factor(avg_scores_executive$membership_gn)
avg_scores_language$membership_gn <- as.factor(avg_scores_language$membership_gn)
avg_scores_semantic$membership_gn <- as.factor(avg_scores_semantic$membership_gn)
avg_scores_visual$membership_gn <- as.factor(avg_scores_visual$membership_gn)
avg_scores_visuomotor$membership_gn <- as.factor(avg_scores_visuomotor$membership_gn)

# Step 4: Plot as sideways barplot
ggplot(avg_scores_auditory, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Audition",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_executive, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Executive Function",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_language, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Language",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_semantic, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Semantic Memory",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()


ggplot(avg_scores_visual, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Visual",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_visuomotor, aes(x = mean_score, y = task, fill = membership_gn)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Visuomotor Integration",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()


####
# Step 2: Reshape membership_normalized to long format
long_data_auditory_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_auditory))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

long_data_executive_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_executive))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

long_data_language_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_language))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

long_data_semantic_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_semantic))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

long_data_visual_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_visual))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

long_data_visuomotor_pos <- membership_normalized_pos %>%
  select(all_of(c("membership_gn_pos", tasks_with_visuomotor))) %>%
  pivot_longer(cols = -membership_gn_pos, names_to = "task", values_to = "score")

# Step 3: Calculate average score for each task
avg_scores_auditory_pos <- long_data_auditory_pos %>%
  group_by(membership_gn_pos, task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_executive_pos <- long_data_executive_pos %>%
  group_by(membership_gn_pos,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_language_pos <- long_data_language_pos %>%
  group_by(membership_gn_pos,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_semantic_pos <- long_data_semantic_pos %>%
  group_by(membership_gn_pos,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_visual_pos <- long_data_visual_pos %>%
  group_by(membership_gn_pos,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_visuomotor_pos <- long_data_visuomotor_pos %>%
  group_by(membership_gn_pos,task) %>%
  summarize(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(mean_score)

avg_scores_auditory_pos$membership_gn_pos <- as.factor(avg_scores_auditory_pos$membership_gn_pos)
avg_scores_executive_pos$membership_gn_pos <- as.factor(avg_scores_executive_pos$membership_gn_pos)
avg_scores_language_pos$membership_gn_pos <- as.factor(avg_scores_language_pos$membership_gn_pos)
avg_scores_semantic_pos$membership_gn_pos <- as.factor(avg_scores_semantic_pos$membership_gn_pos)
avg_scores_visual_pos$membership_gn_pos <- as.factor(avg_scores_visual_pos$membership_gn_pos)
avg_scores_visuomotor_pos$membership_gn_pos <- as.factor(avg_scores_visuomotor_pos$membership_gn_pos)

# Step 4: Plot as sideways barplot
ggplot(avg_scores_auditory_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Audition",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_executive_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Executive Function",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_language_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Language",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_semantic_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Semantic Memory",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()


ggplot(avg_scores_visual_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Visual",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

ggplot(avg_scores_visuomotor_pos, aes(x = mean_score, y = task, fill = membership_gn_pos)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tasks capturing Visuomotor Integration",
    x = "Average Score",
    y = "Task"
  ) +
  theme_bw()

#####PCA
data_numeric <- data[, 1:41]
pca_result <- prcomp(data_numeric, scale. = TRUE)
pca_result$rotation

library(ggplot2)
library(reshape2)

# Extract PCA scores
loadings <- as.data.frame(pca_result$rotation)
loadings$Feature <- rownames(loadings)

# Melt data for ggplot
loadings_melt <- melt(loadings, id.vars = "Feature")

# Plot heatmap
ggplot(loadings_melt, aes(x = variable, y = Feature, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap of PCA Loadings", x = "Principal Components", y = "Features") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########
# Convert the loadings (rotation) into a data frame
loadings_df <- as.data.frame(pca_result$rotation)

# Add the associated tasks (variables) as rownames
loadings_df$Task <- rownames(loadings_df)

# Reshape the data to long format for easier interpretation
loadings_long <- reshape2::melt(loadings_df, id.vars = "Task", variable.name = "Component", value.name = "Loading")

# View the data frame
head(loadings_long)

###CDA results by cluster and by task
#remove designated tasks
data <- imputed_pooled[, !(names(imputed_pooled) %in% tasks_threshold_70_to_delete$Task)]

del <- c("ss_total_perc","avc_total_perc","repetition_perc","nwf_total_perc","reading_totalA",
         "writing_totalA","drawing_perc","calculation_perc","ravens_perc","WAB_AQ")

data <- data[, !(names(data) %in% del)]

data <- data %>%
  left_join(
    membership_normalized %>% select(ID, membership_gn_pos),
    by = "ID"
  )

write.csv(data, "membership_imputed_pooled.csv", row.names = FALSE)

cluster_colors <- c("1" = "orange2", "2" = "skyblue2", "3" = "green4", "4" = "yellow2")

ggplot(data, aes(x = factor(membership_gn_pos), y = symbol_cancel_perc, fill = factor(membership_gn_pos))) +
  geom_boxplot() +
  scale_fill_manual(values = cluster_colors) +
  labs(x = "Cluster", y = "CLQT Symbol Cancellation", fill = "Cluster") +
  theme_minimal()


##by cluster
# Filter for membership_gn_pos == 1
filtered4 <- data %>% filter(membership_gn_pos == 4)

# Convert to long format (assuming task columns start from the 3rd column onward)
filtered4_long <- filtered4 %>%
  pivot_longer(
    cols = -c(ID, membership_gn_pos),  # keep ID and membership_gn_pos, pivot the rest
    names_to = "task",
    values_to = "value"
  )

filtered4_long <- filtered4_long %>%
  mutate(task = factor(task, levels = c("palpa25_total_perc","palpa_8_total_perc",
                                        "rcba_total_perc","rbans_LANGUAGE_total",
                                        "rbans_ATTENTION_total","reading_numbers",
                                        "syntax_aud_filler","syntax_aud_ORC",
                                        "syntax_vis_filler","syntax_vis_OE_clefts",
                                        "syntax_vis_ORC","PEPSC_comp_animal",
                                        "PEPSC_comp_color","MEC_ling_comprehension_Question",
                                        "MEC_ling_comprehension_Statement","MEC_emot_comprehension_sad",
                                        "MEC_emot_comprehension_angry","MEC_emot_comprehension_happy",
                                        "metasyntax_ungrammatical","symbol_cancel_perc",
                                        "rbans_VISUOSPATIAL_total","written_operations_subtraction",
                                        "colorshape_congruent","geometric_matching_matching",
                                        "geometric_matching_notmatching","TOM_long_RUK__FILLER",
                                        "TOM_long_RUK__MC","TOM_long_RUK__TB",
                                        "TOM_long_RUK__FB","TOM_RK__FILLER",
                                        "TOM_RK__TB_MC","TOM_RK__FB",
                                        "music_sour","music_good",
                                        "lowlevel_complex1","lowlevel_complex2",
                                        "lowlevel_pure1","lowlevel_pure2",
                                        "wms_symbol_span_total","digit_comp",
                                        "wms_visualreproduction_total")))

# Plot
ggplot(filtered4_long, aes(x = task, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Cluster 4",
         x = "Task",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

###
# Compute mean for each task
task_means4 <- filtered4_long %>%
  group_by(task) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  mutate(task = factor(task, levels = task[order(-mean_value)]))  # Set factor levels in descending order

# Plot
ggplot(task_means4, aes(x = task, y = mean_value)) +
  geom_col(fill = "yellow2") +
  theme_minimal() +
  labs(title = "Cluster 4",
       x = "Task",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
