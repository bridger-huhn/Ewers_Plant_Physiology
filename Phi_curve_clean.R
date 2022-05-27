phi <- read.csv("/Users/bridgerhuhn/Documents/Research/Blowout_Pen/Phi_Curves/Whole dataset to 05192022.csv")


d3 <- phi %>% 
  select(Plant.ID, names(phi)[which(str_detect(names(phi), pattern = "Phi2"))]) %>% 
  pivot_longer(!c(Plant.ID),names_to = 'PAR', values_to = "phi") %>%  
  separate(col = PAR, into = c("trash", "PAR"), sep = "_", remove = FALSE) %>% 
  mutate(PAR = as.numeric(PAR)) %>% 
  select(!trash) %>% 
  filter(phi >0)
plot(d3$PAR, d3$phi)  



# R Code

library("PhotosynQ")
PhotosynQ::login("cguadagn@uwyo.edu")
ID <- 15332
dfs <- PhotosynQ::getProject("cguadagn@uwyo.edu",ID)
