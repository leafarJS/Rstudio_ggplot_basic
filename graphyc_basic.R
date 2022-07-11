library(tidyverse)

##data visuaisation

# data
# mapping aesthetics
# geometric representation
# statistics
# facet
# coordinate space
# labels
# theme

###########################################
### 01 Introduction an graphics with R  ###
###########################################

view(BOD)
?BOD

ggplot(data = BOD, 
       mapping = aes(
         x = Time,
         y = demand,
         colour = Time
       ))+
  geom_point(size = 3)+
  geom_line(colour = "Blue")+
  theme_classic()
  
ggplot(BOD, aes(
         x = Time,
         y = demand,
         colour = Time
       ))+
  geom_point(size = 4)+
  geom_smooth(
    
  )+
  theme_classic()

############################################################
view(CO2)
dim(CO2)
head(CO2)
glimpse(CO2)

aire <- CO2

aire %>% 
  ggplot(aes(
    x = conc,
    y = uptake,
    colour = Plant,
    size = conc
  ))+
  geom_point( alpha = 0.5)+
  geom_smooth(method = lm, se = F) + 
  facet_wrap(~ Type)+
  theme_minimal()

aire %>% 
  ggplot(aes(
    x = conc,
    y = uptake,
    colour = Treatment
  ))+
  geom_point(size = 3,  alpha = 0.5)+
  geom_smooth() + 
  facet_wrap(~ Type)+
  labs(title = "Concentración de CO2")+
  theme_bw()

###################################################################
aire %>% 
  ggplot(aes(
    x = Treatment,
    y = uptake
  ))+
  geom_boxplot()+
  geom_point(aes(size = conc,
                 colour = Plant,
                 alpha = 0.5))


aire %>% 
  ggplot(aes(
    x = Treatment,
    y = uptake
  ))+
  geom_boxplot()+
  geom_point(aes(size = conc,
                 colour = Plant,
                 alpha = 0.5))+
  facet_wrap(~Type)+
  coord_flip()+
  theme_bw()+
  labs(title = "chilled vs non chilled")

######################################################################
view(mpg)
glimpse(mpg)
names(mpg)

mpg %>% 
  ggplot(aes(
    x = displ, 
    y = cty
  ))+
  geom_point(aes(color = drv,
                 size = trans,
                 alpha = 0.5))+
  geom_smooth(method = lm)


mpg %>%
  dplyr::filter(cty < 25) %>% 
  ggplot(aes(
    x = displ, 
    y = cty
  ))+
  geom_point(aes(color = drv,
                 size = trans,
                 alpha = 0.5))+
  geom_smooth()+
  facet_wrap(~year, nrow = 1)+
  theme_minimal()+
  labs(x = "Engine size",
       y = "CGM in the city",
       title = "Eficiencia en consumo de combustible")

mpg %>%
  dplyr::filter(cty < 25) %>% 
  ggplot(aes(
    x = displ, 
    y = cty
  ))+
  geom_point(aes(color = drv,
                 size = trans,
                 alpha = 0.5))+
  geom_smooth()+
  facet_wrap(~year, nrow = 1)+
  theme_minimal()+
  labs(x = "Engine size",
       y = "CGM in the city",
       title = "Eficiencia en consumo de combustible")+
  ggsave("graphyc_01.jpg")

#################################################
### 02 create beautiful scatter plots with R  ###
#################################################

mpg %>%
  dplyr::filter(hwy < 35) %>% 
  ggplot(aes(
    x = displ, 
    y = hwy,
    colour = drv
  ))+
  geom_point()+
  geom_smooth( method = lm, se =FALSE)+
  facet_wrap(~year, nrow = 1)+
  theme_minimal()+
  labs(x = "Engine size",
       y = "CGM in the city",
       title = "Eficiencia en consumo de combustible") +
  ggsave("graphyc_02.jpg")

mpg %>%
  dplyr::filter(hwy < 35) %>% 
  ggplot(aes(
    x = displ, 
    y = hwy,
   
  ))+
  geom_point(aes( colour = drv))+
  geom_smooth( method = lm, se =FALSE)+
  facet_wrap(~year, nrow = 1)+
  theme_minimal()+
  labs(x = "Engine size",
       y = "CGM in the city",
       title = "Eficiencia en consumo de combustible") +
  ggsave("graphyc_03.jpg")

mpg %>%
  dplyr::filter(hwy < 35) %>% 
  ggplot(aes(
    x = displ, 
    y = hwy,
    
  ))+
  geom_point(aes( colour = drv))+
  geom_smooth()+
  facet_wrap(~year, nrow = 1)+
  theme_minimal()+
  labs(x = "Engine size",
       y = "CGM in the city",
       title = "Eficiencia en consumo de combustible") +
  ggsave("graphyc_04.jpg")

#################################################
### 03 bar charts and histograms with ggplot R###
#################################################

names(msleep)
glimpse(msleep)

#variable categorica simple
class(msleep$vore)
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = vore
  ))+
  geom_bar(fill = "#EFBA73")+
  theme_minimal()+
  labs(title = "Cantidad de especies por genero",
       x = "Categoria",
       y = NULL)

class(msleep$vore)
msleep %>% 
  ggplot(aes(
    x = vore
  ))+
  geom_bar(fill = "#EFBA73")+
  theme_minimal()+
  labs(title = "Alimento que consumen los animales",
       x = "Categoria",
       y = NULL)

class(msleep$vore)
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = vore
  ))+
  geom_bar(fill = "#EFBA73")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Alimento que consumen los animales",
       x = "Categoria",
       y = NULL)

#use fct_infreq(vore) to creat order

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = fct_infreq(vore)
  ))+
  geom_bar(fill = "#EFBA73")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Alimento que consumen los animales",
       x = "Categoria",
       y = NULL)

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = fct_infreq(vore)
  ))+
  geom_bar(fill = "red")+

  theme_minimal()+
  labs(title = "Alimento que consumen los animales",
       x = "Categoria",
       y = NULL)

#variable numerica simple

msleep %>% 
  ggplot(aes(
    x = awake
  ))+
  geom_histogram(binwidth = 3, fill = "#373717")+
  theme_classic()+
  labs(title = "Histograma total horas dormidas",
       x = "total horas dormidas",
       y = NULL)

msleep %>% 
  ggplot(aes(
    x = awake
  ))+
  geom_histogram(binwidth = 2, fill = "#472717")+
  theme_classic()+
  labs(title = "Histograma total horas dormidas",
       x = "total horas dormidas",
       y = NULL)

msleep %>% 
  ggplot(aes(
    x = awake
  ))+
  geom_histogram(binwidth = 1, fill = "#172737")+
  theme_classic()+
  labs(title = "Histograma total horas dormidas",
       x = "total horas dormidas",
       y = NULL)

#################################################
### 04 create scatter plot with ggplot R      ###
#################################################
names(msleep)
class(msleep$brainwt)
class(msleep$bodywt)

msleep %>% 
  dplyr::filter(bodywt < 2) %>% 
  ggplot(aes(
    x = bodywt,
    y = brainwt
  ))+
  geom_point(aes(colour = sleep_total,
                 size = awake))+
  geom_smooth()+
  labs(title = "Ancho de cuerpo y cerebro",
       x = "body width",
       y = "brain width")+
  theme_minimal()


msleep %>% 
  dplyr::filter(bodywt < 2) %>% 
  ggplot(aes(
    x = bodywt,
    y = brainwt
  ))+
  geom_point(aes(colour = sleep_total,
                 size = awake))+
  geom_smooth(se = FALSE)+
  labs(title = "Ancho de cuerpo y cerebro",
       x = "body width",
       y = "brain width")+
  theme_minimal()


#################################################
### 05 draw a line using ggplot with R        ###
#################################################

#two numeric variblaes and one categorical variable

glimpse(Orange)
view(Orange)

Orange %>% 
  dplyr::filter(Tree != 2) %>% 
  ggplot(aes(
    x = age,
    y = circumference
  ))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Tree)+
  theme_gray()+
  labs(title = "Arboles, años y circunferencia")

Orange %>% 
  dplyr::filter(Tree != 2) %>% 
  ggplot(aes(
    x = age,
    y = circumference
  ))+
  geom_point()+
  geom_smooth()+
  theme_gray()+
  labs(title = "Arboles, años y circunferencia")


Orange %>% 
  dplyr::filter(Tree != 3) %>% 
  ggplot(aes(
    x = age,
    y = circumference
  ))+
  geom_point()+
  geom_smooth(method = "loess")+
  theme_gray()+
  labs(title = "Arboles, años y circunferencia")

Orange %>% 
  dplyr::filter(Tree != 1 & Tree != 2) %>% 
  ggplot(aes(
    x = age,
    y = circumference,
    colour = Tree
  ))+
  geom_point(size = 5, alpha = 0.3)+
  geom_line(size = 2)+
  theme_gray()+
  labs(title = "Arboles, años y circunferencia")

#################################################
### 06 create boxplot using ggplot with R     ###
#################################################

#variable categorical and numeric 
glimpse(msleep)

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = vore,
    y = sleep_total
  ))+
  geom_boxplot(fill = "#91F539")+
  coord_flip()+
  theme_bw()


msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = vore,
    y = sleep_total
  ))+
  geom_boxplot(fill = "#72E7E3")+
  theme_bw()


msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total
  ))+
  geom_density()+
  facet_wrap(~vore)+
  theme_classic()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total
  ))+
  geom_density(aes(colour = vore, alpha = 0.5))+
  theme_classic()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total
  ))+
  geom_histogram(binwidth = 2, fill = "#170037")+
  theme_classic()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total
  ))+
  geom_histogram(binwidth = 2, fill = "#650037")+
  facet_wrap(~vore)+
  theme_classic()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total
  ))+
  geom_density( fill = "#650037", alpha = 0.5)+
  facet_wrap(~vore)+
  theme_classic()


msleep %>% 
  drop_na(vore) %>% 
  dplyr::filter(vore %in% c("carni", "insecti")) %>% 
  ggplot(aes(
    x = sleep_total,
    fill = vore
  ))+
  geom_density(alpha = 0.2)+
  theme_bw()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(
    x = sleep_total,
    fill = vore
  ))+
  geom_density(alpha = 0.2)+
  theme_bw()

msleep %>% 
  drop_na(vore) %>% 
  dplyr::filter(vore == "herbi" | vore == "omni") %>% 
  ggplot(aes(
    x = sleep_total,
    fill = vore
  ))+
  geom_density(alpha = 0.2)+
  theme_bw()


msleep %>% 
  drop_na(vore) %>% 
  dplyr::filter(vore == "herbi" | vore == "omni") %>% 
  ggplot(aes(
    x = sleep_total,
    colour = vore
  ))+
  geom_density(alpha = 0.2)+
  theme_bw()

msleep %>% 
  drop_na(vore) %>% 
  dplyr::filter(vore == "herbi" | vore == "omni") %>% 
  ggplot(aes(
    x = sleep_total,
    fill = vore
  ))+
  geom_density()+
  theme_bw()

########################################################
### 07 create bar charts for 2 categorical variable  ###
########################################################

view(starwars)
names(starwars)
glimpse(starwars)
starwars$sex

starwars %>% 
  dplyr::filter(hair_color == "black" | hair_color == "brown") %>% 
  drop_na(sex) %>% 
  ggplot(aes(
    x = hair_color,
    fill = sex
  ))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Genero y tipo de cabello",
       x =  "hair color",
       y = "number")

starwars %>% 
  dplyr::filter(hair_color == "black" | hair_color == "brown") %>% 
  drop_na(sex) %>% 
  ggplot(aes(
    x = hair_color,
    fill = sex
  ))+
  geom_bar(alpha = 0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Genero y tipo de cabello",
       x =  "hair color",
       y = "number")

starwars %>% 
  dplyr::filter(hair_color == "black" | hair_color == "brown") %>% 
  drop_na(sex) %>% 
  ggplot(aes(
    x = hair_color,
    fill = sex
  ))+
  geom_bar(position = "dodge", alpha = 0.5)+
  facet_wrap(~hair_color)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Genero y tipo de cabello",
       x =  "hair color",
       y = "number")


starwars %>% 
  dplyr::filter(hair_color %in% c("black", "brown") ) %>% 
  drop_na(sex) %>% 
  ggplot(aes(
    x = sex
  ))+
  geom_bar(aes(fill = sex), alpha = 0.5)+
  facet_wrap(~hair_color)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Genero y tipo de cabello",
       x =  "hair color",
       y = "number")




