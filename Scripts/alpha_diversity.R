library(data.table)
library(ggpubr)
library(ggsci)
library(microbiome)
library(phyloseq)

# Input files: otu, taxa, and meta data table
otu.tbl <- "../Data/otu.csv"
tax.tbl <- "../Data/taxa.csv"
meta.tbl <- "../Data/meta.csv"

# Read input files into a phyloseq object
pseq <- read_phyloseq(otu.file = otu.tbl, 
                      taxonomy.file = tax.tbl, 
                      metadata.file = meta.tbl, 
                      type = "simple")

### Estimate richness and diversity
rich.dt <- estimate_richness(pseq, measures = c("Observed", "Chao1", "Shannon"))

# Extract ocean depth layer category
depth <- sample_data(pseq)$Depth

# Add ocean depth layer category to richness table
rich.dt <- cbind(rich.dt, depth)

# Plot observed species richness as a function of the ocean depth layer
p1 <- ggplot(rich.dt, aes(x = depth, y = Observed, color = depth)) +
  geom_boxplot() +
  scale_color_locuszoom() +
  xlab("Ocean Depth Layer") +
  ylab("Observed Richness") +
  labs(color = "Depth") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# Plot Chao1 richness as a function of the ocean depth layer
p2 <- ggplot(rich.dt, aes(x = depth, y = Chao1, color = depth)) +
  geom_boxplot() +
  scale_color_locuszoom() +
  xlab("Ocean Depth Layer") +
  ylab("Chao1") +
  labs(color = "Depth") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

# Plot Shannon diversity as a function of the ocean depth layer
p3 <- ggplot(rich.dt, aes(x = depth, y = Shannon, color = depth)) +
  geom_boxplot() +
  scale_color_locuszoom() +
  xlab("Ocean Depth Layer") +
  ylab("Shannon Diversity") +
  labs(color = "Depth") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggarrange(p1 + theme(axis.title.x=element_blank(), axis.text.x=element_blank()), 
          p2 + theme(axis.title.x=element_blank(), axis.text.x=element_blank()), 
          p3,
          labels = c("A", "B", "C"),
          common.legend = TRUE,
          legend = "right",
          nrow = 3,
          align = "v")

### Compare mean species richness and diversity
pairwise_comparisons <- list( c("DCM", "MES"), c("MES", "MIX"), c("MIX", "SRF") )

p2 + stat_compare_means()
p3 + stat_compare_means()






