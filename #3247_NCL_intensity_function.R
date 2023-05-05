#  _  _  ____  ____   ___  ____        __ _   ___  __   
# / )( \( __ \(___ \ / _ \(__  )      (  ( \ / __)(  )  
# )    ( (__ ( / __/(__  (  / /       /    /( (__ / (_/\
# \_)(_/(____/(____)  (__/ (_/        \_)__) \___)\____/
  
rm(list=ls())
plot_list <- list()
my_packs <- c("readr","ggplot2","tidyverse","rstatix","ggprism",
              "ggthemes","scales","gridExtra","RColorBrewer")
lapply(my_packs, library, character.only = TRUE)
theme_set(theme_bw())

pathFileList <- list("D:/smyl/Labo/projects/KD NCL HCT116/Analyses HCS/id #3247/NCL intensity/compil/IF shRNA HCT116 clones 230418__2023-04-19T18_04_21-Measurement 1b/Evaluation6/Objects_Population - Nuclei Selected.txt",
"D:\\smyl\\Labo\\projects\\KD NCL HCT116\\Analyses HCS\\id #3247\\NCL intensity\\compil\\#3247 G1H12 B23 restarted with 25% laser__2023-04-21T17_34_05-Measurement 1b\\Evaluation1\\Objects_Population - Nuclei Selected.txt",
"D:\\smyl\\Labo\\projects\\KD NCL HCT116\\Analyses HCS\\id #3247\\NCL intensity\\compil\\#3247__2023-04-21T14_33_50-Measurement 1b\\Evaluation1\\Objects_Population - Nuclei Selected.txt")

BoPlot <- function(pathFile){

loaded_table <- read_tsv(pathFile, skip = 9, col_select = c("Cell Type", "Dox", "Nuclei Selected - NCL intensity Mean","Antibody"))
#get antibody name
antibody <<- loaded_table %>% slice(11) %>% pull("Antibody") %>% as.character()
#rename columns
loaded_table <- loaded_table %>% rename('Cell'=`Cell Type`) %>% rename('NCL'=`Nuclei Selected - NCL intensity Mean`)
#reorder cell lines and dox treatments for plotting
loaded_table$Cell <-factor(loaded_table$Cell,levels=c("shNS #1", "shNS #2", "sh22 #2", "sh22 #6", "sh73 #6", "sh73 #10"))
loaded_table$Dox <-factor(loaded_table$Dox,levels=c("noDox", "+Dox"))
loaded_table <- unite(loaded_table, Cell, Dox, col = "Cell_Dox", sep="_", remove = FALSE)

plot_a <- ggplot(loaded_table, aes(x=Cell, y=NCL, fill=Dox)) +
  geom_violin(position=position_dodge(0.75)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = antibody)
return(plot_a)
}

plots_list <- lapply(pathFileList, BoPlot)

# Arrange the plots in a grid using gridExtra
grid.arrange(grobs = lapply(plots_list, ggplotGrob),
              ncol = 1)

