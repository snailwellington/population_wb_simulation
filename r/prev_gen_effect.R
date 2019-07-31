library(tidyverse)


generate_population <- function(size = 100, mean_income = 10000, sd_percent = 0.05){
  
  pop_sd = sd_percent*mean_income
  pop_data <- data.frame("person"  = seq(1,size, 1),
                         "fin_situation" = rnorm(1:size, mean = mean_income, sd = pop_sd))
  return(pop_data)
}




# Loop should start here 

generations <- 100

set.seed(42)

initial_pop <- generate_population(size = 1000, mean_income = 1, sd_percent = 0.05)

tmp <- data.frame("iteration" = 0, "total_value" = sum(initial_pop$fin_situation<1)/nrow(initial_pop))

densities <- data.frame("iteration" = 0, "data" = nest(initial_pop))

for(i in 1:generations){
  initial_pop <- initial_pop %>% 
    mutate(gen_multiplier = case_when(fin_situation<1 ~ sample(rnorm(1:nrow(initial_pop), mean = 0.99, sd = 0.02), nrow(initial_pop), replace = F),
                                      fin_situation == 1 ~ sample(rnorm(1:nrow(initial_pop), mean = 1, sd = 0.02), nrow(initial_pop), replace = F),
                                      TRUE ~sample(rnorm(1:nrow(initial_pop), mean = 1.01, sd = 0.02), nrow(initial_pop), replace = F))) %>% 
    mutate(fin_situation = gen_multiplier*fin_situation)
  

  tmp <- rbind(tmp,data.frame("iteration" = i, "total_value" = sum(initial_pop$fin_situation<1)/nrow(initial_pop)))
  densities <- rbind(densities, data.frame("iteration" = i, "data" = nest(initial_pop)))
}

ggplot(data = initial_pop,aes(fin_situation))+
  geom_density()

ggplot(tmp, aes(x = iteration, y = total_value))+
  geom_line()+
  coord_cartesian(ylim = c(0,max(tmp$total_value)))


den_data <- densities %>% 
  unnest() %>%
  group_by(iteration) %>% 
  mutate(median_value = median(fin_situation),
         mean_value = mean(fin_situation))


ggplot(den_data,aes(x = iteration, y = median_value)) +
  geom_line()

ggplot(den_data,aes(x = iteration, y = mean_value)) +
  geom_line()




plot <- ggplot(den_data,aes(x = fin_situation, fill = as.factor(iteration), colour = as.factor(iteration)))+
  # facet_wrap(.~as.factor(iteration))+
  # scale_fill_continuous(palette = "blues")+
  geom_density(alpha = 0.01)+
  theme_minimal()+
  theme(legend.position = "none")
  

ggsave(plot = plot,"output/prev_gen_effect.png", width = 16, height = 9, dpi = 150)

