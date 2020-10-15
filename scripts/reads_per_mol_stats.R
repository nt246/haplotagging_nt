library(tidyverse)

stats <- read_tsv("./data/ShadHap1_L002_reads.per.mol.log", col_names = c("reads", "count"))

ggplot(stats, aes(reads, log(count))) +
  geom_line()

# Make list of tick label for y-axis that mirror Marek's plot (log2 scaled axis)
ticks <- c(1, rep(NA, 14))
for (i in 2:length(ticks)){
  ticks[i] = ticks[i-1]*4
}

ggplot(stats, aes(reads, count)) +
  geom_line() +
  scale_y_continuous(trans = "log2", breaks = ticks, labels=function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(1, max(ticks)), expand = c(0, 0)) +
  theme_minimal()

ggsave("plots/reads_per_mol_log2scale.png")


arrange(stats, reads)
sum(filter(stats, reads > 2)$count) / 95
filter(stats, reads > 2) / sum(filter(stats, reads > 2)$count)


stats %>% 
  filter(reads > 2) %>% 
  mutate(reads_on_mols = reads * count) %>% 
  select(reads_on_mols) %>% 
  sum()
