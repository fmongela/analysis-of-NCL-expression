
df_only_shNS <- filter( loaded_table, Cell=='shNS #1')

loaded_table_mutated <- mutate(loaded_table, Cell_Dox=paste(Cell,Dox),sep="_")
plot_c <- ggplot(loaded_table_mutated, aes(NCL, fill=Cell_Dox)) + 
  geom_histogram(binwidth = 100) +
  scale_fill_brewer(palette = "Paired") 

plot_d <- ggplot(df_only_shNS, aes(NCL, fill=Dox)) +
  geom_histogram(binwidth = 250) +
  scale_fill_brewer(palette = "Paired")
plot_b
plot_c
plot_d

df_stats  <- loaded_table %>%
  group_by(Cell) %>%
  t_test(`NCL` ~ `Dox`) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
df_stats