################
# MODUŁ 0: WSTĘP 
################


####################################
# PODMODUŁ 0.1: POTRZEBNE BIBLIOTEKI 
####################################


library(ggplot2)
library(grid)
library(gridExtra)
library(fields)
library(cowplot)
library(patchwork)
library(scales)


#############################################
# PODMODUŁ 0.2: WYJŚCIOWE WARTOŚCI PARAMETRÓW 
#############################################


delta_t = 1/12
sigma = 0.3
S_0 = 50
r = 0.02
K = 48
czas_T = 2


##################
# MODUŁ 1: FUNKCJE 
##################


###################################
# PODMODUŁ 1.1: WYLICZENIA u ORAZ d
###################################


u_fun = function(sigma = 0.3, delta_t = 1/12){
  u = exp(sigma*sqrt(delta_t)) # wzor z tresci zadania
  return(u)
}


d_fun = function(sigma = 0.3, delta_t = 1/12){
  d = exp(-sigma*sqrt(delta_t)) # wzor z tresci zadania
  return(d)
}


############################
# PODMODUŁ 1.2: WYCENA OPCJI 
############################


wycena = function(r = 0.02, delta_t = 1/12, sigma = 0.3, V_u, V_d){
  # u i d z funkcji
  u = u_fun(sigma, delta_t)
  d = d_fun(sigma, delta_t)
  # korzystamy ze wzoru na p i V_0 z wykladu
  p = (exp(r*delta_t)-d)/(u-d)
  V_0 = exp(-r*delta_t)*(p*V_u+(1-p)*V_d)
  return(V_0)
}


#########################################
# PODMODUŁ 1.3: WYCENA OPCJI EUROPEJSKIEJ
#########################################


wycena_europejska = function(r = 0.02, delta_t = 1/12, sigma = 0.3, S_0 = 50, K = 48, czas_T = 2, opcja = 'call'){
  n = czas_T/delta_t+1 # ile mamy kroków czasowych/wezlow drzewa na koncu
  V = matrix(NA, ncol=n, nrow=n) # tworzymy macierz, ktora bedzie trzymala cene w kolejnych wierzcholkach
  # u i d z funkcji
  u = u_fun(sigma, delta_t)
  d = d_fun(sigma, delta_t)
  # ustalamy payoff w ostatniej kolumnie
  for(i in 1:n){
    if (opcja=="call")
    {
      V[i, n] = max(S_0*u^(n-i)*d^(i-1)-K, 0) # wzor z wykladu na payoff opcji europejskiej call
    }
    else if (opcja=="put")
    {
      V[i, n] = max(-S_0*u^(n-i)*d^(i-1)+K, 0) # wzor z wykladu na payoff opcji amerykanskiej put
    }
  }
  # wyceniamy od przedostatniej chwili do chwili 0
  # idziemy od wierzcholka najwyzej do wierzcholka najnizej
  for(i in (n-1):1){
    for(j in 1:i){
      V[j, i] = wycena(r, delta_t, sigma, V[j, i+1], V[j+1, i+1]) # korzystamy z napisanej funkcji
    }
  }
  return(list(V[1,1], V)) # zwracamy cene opcji w chwili 0 oraz macierz z wycenami w kolejnych wierzcholkach
}


##########################################
# PODMODUŁ 1.4: WYCENA OPCJI AMERYKAŃSKIEJ
##########################################


wycena_amerykanska = function(r, delta_t, sigma, S_0, K, czas_T, opcja){
  n = czas_T/delta_t+1  # ile mamy kroków czasowych/wezlow drzewa na koncu
  V_i = matrix(NA, ncol=n, nrow=n) # macierz wyceny jak opcji europejskiej
  V_ii = matrix(NA, ncol=n, nrow=n) # macierz payoffu w wierzcholkach gdybysmy chcieli wykonac opcje
  V = matrix(NA, ncol=n, nrow=n) # macierz wyceny opcji amerykanskiej w kolejnych wierzcholkach
  wykonanie = matrix(NA, ncol=n, nrow=n) # czy oplaca nam sie wykonac opcje w danym wierzcholku?
  # u i d z funkcji
  u = u_fun(sigma, delta_t)
  d = d_fun(sigma, delta_t)
  for(i in 1:n){
    if (opcja=="call"){
      V_i[i, n] = max(S_0*u^(n-i)*d^(i-1)-K, 0) # payoff europejski w ostatniej kolumnie
      for (j in 1:i){
        V_ii[j, i] = max(S_0*u^(i-j)*d^(j-1)-K, 0) # wypelniamy cala macierz payoffu w wierzcholkach w wypadku wykonania akcji w danym wezle
      }
    }
    else if (opcja=="put"){
      V_i[i, n] = max(-S_0*u^(n-i)*d^(i-1)+K, 0) # payoff europejski w ostatniej kolumnie
      for (j in 1:i){
        V_ii[j, i] = max(-S_0*u^(i-j)*d^(j-1)+K, 0) # wypelniamy cala macierz payoffu w wierzcholkach w wypadku wykonania akcji w danym wezle
      }
    }
  }
  V[, n]=V_i[, n] # payoff w ostatniej kolumnie jest taki sam jak w opcji europejskiej
  wykonanie[, n]=V[, n]>0 # czy oplaca nam sie wykonac opcje w momencie zapadalnosci?
  # ponownie idziemy od konca, od gory do dolu
  for(i in (n-1):1){
    for(j in 1:i){
      V_i[j, i] = wycena(r, delta_t, sigma, V[j, i+1], V[j+1, i+1]) # wycena jak europejska
      V[j, i] = max(V_i[j, i], V_ii[j, i]) # sprawdzamy co nam sie bardziej oplaca - wykonac opcje czy nie?
      wykonanie[j, i] = V_i[j, i] < V_ii[j, i] # czy oplaca nam sie wykonac opcje w tym wierzcholku
    }
  }
  return(list(V[1, 1], V, wykonanie)) # zwracamy cene opcji w chwili 0, macierz z wycenami w kolejnych wierzcholkach oraz macierz okreslajaca, czy oplaca sie wykonac opcje w danym wierzcholku czy nie
}


#####################################################
# PODMODUŁ 1.5: WYKRES WYCENY OPCJI EUROPEJSKIEJ CALL
#####################################################


wycena_europejska_opcji_call_wykres = function(){
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(V_euro_call, 2, rev)),
             zlim = c(0, maksymalna_cena_call), 
             main = "Wycena europejskiej opcji call",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.shrink = 0.85, 
             legend.args = list(text = "Wartość wyceny",
                                side = 4.5, 
                                line = 3, 
                                cex = 1.2)) 
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
}


####################################################
# PODMODUŁ 1.6: WYKRES WYCENY OPCJI EUROPEJSKIEJ PUT
####################################################


wycena_europejska_opcji_put_wykres = function(){
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(V_euro_put, 2, rev)),
             zlim = c(0, maksymalna_cena_put), 
             main = "Wycena europejskiej opcji put",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.shrink = 0.85, 
             legend.args = list(text = "Wartość wyceny",
                                side = 4.5, 
                                line = 3, 
                                cex = 1.2)) 
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
}


######################################################
# PODMODUŁ 1.7: WYKRES WYCENY OPCJI AMERYKAŃSKIEJ CALL
######################################################


wycena_amerykanska_opcji_call_wykres = function(){
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(V_amer_call, 2, rev)),
             zlim = c(0, maksymalna_cena_call), 
             main = "Wycena amerykańskiej opcji call",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.shrink = 0.85, 
             legend.args = list(text = "Wartość wyceny",
                                side = 4.5, 
                                line = 3, 
                                cex = 1.2))
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
}


#####################################################
# PODMODUŁ 1.8: WYKRES WYCENY OPCJI AMERYKAŃSKIEJ PUT
#####################################################


wycena_amerykanska_opcja_put_wykres = function(){
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(V_amer_put, 2, rev)),
             zlim = c(0, maksymalna_cena_put), 
             main = "Wycena amerykańskiej opcji put",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.shrink = 0.85, 
             legend.args = list(text = "Wartość wyceny",
                                side = 4.5, 
                                line = 3, 
                                cex = 1.2)) 
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t + 1, czas_T/delta_t + 1, col = "black")
}

########################################
# PODMODUŁ 1.9: WYKRES RÓŻNICY OPCJI PUT
########################################


roznica_wycen_put_wykres = function(){
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
           t(apply(V_amer_put, 2, rev))-t(apply(V_euro_put, 2, rev)),
           main = "\n\nRóżnica wycen opcji put",
           cex.main = 0.9,
           xlab = "Czas\n",
           ylab = " ",
           axes = FALSE,
           legend.shrink = 0.85, 
           legend.args = list(text = "Wysokość różnicy",
                              side = 4.5, 
                              line = 2.5, 
                              cex = 1)) 
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t + 1, czas_T/delta_t + 1, col = "black")
}


#########################################################
# PODMODUŁ 1.10: ŁĄCZENIE WYKRESÓW WYCENY OPCJI CALL I PUT
#########################################################


wyceny_opcji_wykres = function(){
  par(mfrow = c(2,2))
  wycena_europejska_opcji_call_wykres()
  wycena_europejska_opcji_put_wykres()
  wycena_amerykanska_opcji_call_wykres()
  wycena_amerykanska_opcja_put_wykres()
  par(mfrow = c(1,1))
}


##############################################################
# PODMODUŁ 1.11: WYKRES "CZY WYKONUJEMY OPCJĘ AMERYKAŃSKĄ PUT?"
##############################################################


czy_wykonujemy_amerykanska_put_wykres = function(){
  image(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25),t(apply(wyk_amer_put, 2, rev)),
        col=c("red1","chartreuse3"), zlim=c(0, 1),
        main = "Czy wykonujemy \n opcję amerykańską put", xlab = "Czas", ylab = " ", axes = FALSE)
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t + 1, czas_T/delta_t + 1, col = "black")
}


################################################################
# PODMODUŁ 1.12: WYKRES "CZY WYKONUJEMY OPCJĘ AMERYKAŃSKĄ CALL?"
################################################################


czy_wykonujemy_amerykanska_call_wykres = function(){
  image(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25),t(apply(wyk_amer_call, 2, rev)),
        col=c("red1","chartreuse3"), zlim=c(0, 1),
        main = "Czy wykonujemy \n amerykańską opcję call", xlab = "Czas", ylab = " ", axes = FALSE)
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
}


######################################################################
# PODMODUŁ 1.13: ŁĄCZENIE WYKRESÓW "CZY WYKONUJEMY OPCJĘ AMERYKAŃSKĄ?"
######################################################################


czy_wykonujemy_amerykanska_wykres = function(){
  par(mfrow = c(1,2))
  czy_wykonujemy_amerykanska_put_wykres()
  czy_wykonujemy_amerykanska_call_wykres()
  par(mfrow = c(1,1))
}


########################################################################
# PODMODUŁ 1.14: WEKTORYZACJA FUNKCJI POTRZEBNYCH DO BADANIA WRAŻLIWOŚCI
########################################################################


vec_europejska = Vectorize(wycena_europejska)
vec_amerykanska = Vectorize(wycena_amerykanska)


###################################################################
# PODMODUŁ 1.15: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI K DLA CALL I PUT
###################################################################


wykres_wrazliwosci_K = function(r = 0.02, delta_t = 1/12, sigma = 0.3, S_0 = 50, K_vec, czas_T = 2, opcja = "call") {
  zmiany_K_euro_call = unlist(vec_europejska(r, delta_t, sigma, S_0, K_vec, czas_T, opcja)[1, ])
  zmiany_K_euro_call = as.data.frame(zmiany_K_euro_call)
  zmiany_K_amer_call = unlist(vec_amerykanska(r, delta_t, sigma, S_0, K_vec, czas_T, opcja)[1, ])
  zmiany_K_amer_call = as.data.frame(zmiany_K_amer_call)
  zm = data.frame(zm = zmiany_K_euro_call, zm1 = zmiany_K_amer_call)
  
  wykres = ggplot(aes(x = K_vec, y = zmiany_K_euro_call), data = zm)+
    geom_line(aes(color = "euro"), lwd = 1) +
    geom_line(aes(y = zmiany_K_amer_call, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = 'Cena wykonania K', 
         y = paste('Zmiana ceny opcji ', opcja), 
         title = paste('Zmiana ceny opcji ', opcja, '\nze względu na cenę wykonania K'),
         color = 'Opcja') +
    theme_bw() +
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.6, 0.6), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0,35))
  
  return(wykres) 
}


###################################################################
# PODMODUŁ 1.16: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI T DLA CALL I PUT
###################################################################


wykres_wrazliwosci_T = function(r = 0.02, delta_t = 1/12, sigma = 0.3, S_0 = 50, K = 48, czas_T_vec, opcja = "call") {
  zmiany_T_euro = unlist(vec_europejska(r, delta_t, sigma, S_0, K, czas_T_vec, opcja)[1, ])
  zmiany_T_euro = as.data.frame(zmiany_T_euro)
  zmiany_T_amer = unlist(vec_amerykanska(r, delta_t, sigma, S_0, K, czas_T_vec, opcja)[1, ])
  zmiany_T_amer = as.data.frame(zmiany_T_amer)
  zm = data.frame(zm = zmiany_T_euro, zm1 = zmiany_T_amer)
  
  wykres = ggplot(aes(x = czas_T_vec, y = zmiany_T_euro), data = zm)+
    geom_line(aes(color = "euro"), lwd = 1) +
    geom_line(aes(y = zmiany_T_amer, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = 'Zapadalność T',
         y = paste('Zmiana ceny opcji ', opcja),
         title = paste('Zmiana ceny opcji ', opcja, '\nze wzgłędu na zapadalność T'),
         color = 'Opcja') +
    theme_bw() +
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.6, 0.6), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0,35))
  
  return(wykres) 
}


#####################################################################
# PODMODUŁ 1.17: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI S_0 DLA CALL I PUT
#####################################################################


wykres_wrazliwosci_S_0 = function(r = 0.02, delta_t = 1/12, sigma = 0.3, S_0_vec, K = 48, czas_T = 2, opcja = "call") {
  zmiany_S_0_euro = unlist(vec_europejska(r, delta_t, sigma, S_0_vec, K, czas_T, opcja)[1, ])
  zmiany_S_0_euro = as.data.frame(zmiany_S_0_euro)
  zmiany_S_0_amer = unlist(vec_amerykanska(r, delta_t, sigma, S_0_vec, K, czas_T, opcja)[1, ])
  zmiany_S_0_amer = as.data.frame(zmiany_S_0_amer)
  zm = data.frame(zm = zmiany_S_0_euro, zm1 = zmiany_S_0_amer)
  
  wykres = ggplot(aes(x = S_0_vec, y = zmiany_S_0_euro), data = zm)+
    geom_line(aes(color = "euro"), lwd = 1) +
    geom_line(aes(y = zmiany_S_0_amer, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = expression(paste('Cena spot ', S[0])),
         y = paste('Zmiana ceny opcji ', opcja),
         title = paste('Zmiana ceny opcji ', opcja, '\nze wzgłędu na cenę spot'),
         color = 'Opcja') +
    theme_bw() +
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.5, 0.7), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0,35))
  
  return(wykres) 
}


#######################################################################
# PODMODUŁ 1.18: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI sigma DLA CALL I PUT
#######################################################################


wykres_wrazliwosci_sigma = function(r = 0.02, delta_t = 1/12, sigma_vec, S_0 = 50, K = 48, czas_T = 2, opcja = "call") {
  zmiany_sigma_euro = unlist(vec_europejska(r, delta_t, sigma_vec, S_0, K, czas_T, opcja)[1, ])
  zmiany_sigma_euro = as.data.frame(zmiany_sigma_euro)
  zmiany_sigma_amer = unlist(vec_amerykanska(r, delta_t, sigma_vec, S_0, K, czas_T, opcja)[1, ])
  zmiany_sigma_amer = as.data.frame(zmiany_sigma_amer)
  zm = data.frame(zm = zmiany_sigma_euro, zm1 = zmiany_sigma_amer)
  
  wykres = ggplot(aes(x = sigma_vec, y = zmiany_sigma_euro), data = zm)+
    geom_line(aes(color = "euro"), lwd = 1) +
    geom_line(aes(y = zmiany_sigma_amer, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = expression(paste('Zmienność ', sigma)),
         y = paste('Zmiana ceny opcji ', opcja),
         title =  paste('Zmiana ceny opcji ', opcja, '\nze względu na zmienność \u03C3'),
         color = 'Opcja') +
    theme_bw()  +
    
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.4, 0.7), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0,35))
  
  return(wykres) 
}


###################################################################
# PODMODUŁ 1.19: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI r DLA CALL I PUT
###################################################################


wykres_wrazliwosci_r = function(r_vec, delta_t = 1/12, sigma = 0.3, S_0 = 50, K = 48, czas_T = 2, opcja = "call") {
  zmiany_r_euro = unlist(vec_europejska(r_vec, delta_t, sigma, S_0, K, czas_T, opcja)[1, ])
  zmiany_r_euro = as.data.frame(zmiany_r_euro)
  zmiany_r_amer = unlist(vec_amerykanska(r_vec, delta_t, sigma, S_0, K, czas_T, opcja)[1, ])
  zmiany_r_amer = as.data.frame(zmiany_r_amer)
  zm = data.frame(zm = zmiany_r_euro, zm1 = zmiany_r_amer)
  
  wykres = ggplot(aes(x = r_vec, y = zmiany_r_euro), data = zm)+
    geom_line(aes(color = "euro"), lwd = 1) +
    geom_line(aes(y = zmiany_r_amer, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = 'Stopa wolna od ryzyka r',
         y = paste('Zmiana ceny opcji ', opcja),
         title = paste('Zmiana ceny opcji ', opcja, '\nze wzgłędu na stope wolną od ryzyka r'),
         color = 'Opcja') +
    theme_bw() +
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.4, 0.7), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0,35))
  
  return(wykres) 
}


#########################################################################
# PODMODUŁ 1.20: JEDNOWYMIAROWY WYKRES WRAŻLIWOŚCI delta_t DLA CALL I PUT
#########################################################################


wykres_wrazliwosci_delta_t = function(r = 0.02, delta_t_vec, sigma = 0.3, S_0 = 50, K = 48, czas_T = 2, opcja = "call") {
  zmiany_delta_t_euro = unlist(vec_europejska(r, delta_t_vec, sigma, S_0, K, czas_T, opcja)[1, ])
  zmiany_delta_t_euro = as.data.frame(zmiany_delta_t_euro)
  zmiany_delta_t_amer = unlist(vec_amerykanska(r, delta_t_vec, sigma, S_0, K, czas_T, opcja)[1, ])
  zmiany_delta_t_amer = as.data.frame(zmiany_delta_t_amer)
  
  transformed_x = 1:length(delta_t_vec)
  x_labels = 1/delta_t_vec
  
  
  zm = data.frame(transformed_x = transformed_x, zmiany_delta_t_euro = zmiany_delta_t_euro, zmiany_delta_t_amer = zmiany_delta_t_amer)
  
  wykres = ggplot(data = zm) +
    geom_line(aes(x = transformed_x, y = zmiany_delta_t_euro, color = "euro"), lwd = 1) +
    geom_line(aes(x = transformed_x, y = zmiany_delta_t_amer, color = "amer"), lwd = 1.5, linetype = 'dashed') +
    labs(x = 'Liczba okresów',
         y = paste('Zmiana ceny opcji ', opcja),
         title = paste('Zmiana ceny opcji ', opcja, '\nze względu na liczbę okresów'),
         color = 'Opcja') +
    theme_bw() +
    scale_color_manual(values = c('euro' = 'limegreen', 'amer' = 'sienna4'),
                       labels = c('euro' = 'Europejska', 'amer' = 'Amerykańska')) +
    theme(legend.position = c(0.8, 0.8), legend.key.size = unit(0.6, "cm")) +
    ylim(c(0, 35)) +
    scale_x_continuous(breaks = transformed_x, labels = x_labels)
  
  return(wykres)
}


########################################################################
# PODMODUŁ 1.21: ŁĄCZENIE JEDNOWYMIAROWYCH WYKRESÓW WRAŻLIWOŚCI DLA CALL 
########################################################################


wykresy_wrazliwosci_jednowymiarowe = function(S_0_vec, delta_t_vec, r_vec, K_vec, sigma_vec, T_vec, opcja = 'call'){
  
  wykres_wrazliwosci_S_0 = wykres_wrazliwosci_S_0(S_0_vec = S_0_vec, opcja = opcja)
  wykres_wrazliwosci_delta_t = wykres_wrazliwosci_delta_t(delta_t_vec = delta_t_vec, opcja = opcja)
  wykres_wrazliwosci_r = wykres_wrazliwosci_r(r_vec = r_vec, opcja = opcja)
  wykres_wrazliwosci_K = wykres_wrazliwosci_K(K_vec = K_vec, opcja = opcja)
  wykres_wrazliwosci_sigma = wykres_wrazliwosci_sigma(sigma_vec = sigma_vec, opcja = opcja)
  wykres_wrazliwosci_T = wykres_wrazliwosci_T(czas_T_vec = czas_T_vec, opcja = opcja)
  
  grid = grid.arrange(wykres_wrazliwosci_r, 
               wykres_wrazliwosci_S_0, 
               wykres_wrazliwosci_K, 
               wykres_wrazliwosci_T, 
               wykres_wrazliwosci_sigma,
               wykres_wrazliwosci_delta_t,
               ncol = 3)
  return(grid)
}


#######################################################################
# PODMODUŁ 1.22: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI K I S_0 DLA CALL I PUT
#######################################################################


wykres_wrazliwosci_K_S_0 = function(K_vec1, S_0_vec1){
  df_S_K = data.frame(
    K = rep(K_vec1, times = length(S_0_vec1)),
    S_0 = rep(S_0_vec1, each = length(K_vec1)),
    V_0_eu_call = 0,
    V_0_eu_put = 0,
    V_0_am_call = 0,
    V_0_am_put = 0
  )
  
  for(i in 1:nrow(df_S_K)){
    df_S_K[i, 3] = wycena_europejska(r, delta_t, sigma, df_S_K[i, 2], df_S_K[i, 1], czas_T, "call")[[1]]
    df_S_K[i, 4] = wycena_europejska(r, delta_t, sigma, df_S_K[i, 2], df_S_K[i, 1], czas_T, "put")[[1]]
    df_S_K[i, 5] = wycena_amerykanska(r, delta_t, sigma, df_S_K[i, 2], df_S_K[i, 1], czas_T, "call")[[1]]
    df_S_K[i, 6] = wycena_amerykanska(r, delta_t, sigma, df_S_K[i, 2], df_S_K[i, 1], czas_T, "put")[[1]]
  }
  
  maksymalna_call_S_K = max(max(df_S_K$V_0_eu_call), max(df_S_K$V_0_am_call))
  maksymalna_put_S_K = max(max(df_S_K$V_0_eu_put), max(df_S_K$V_0_am_put))
  
  euro_call_S_K = ggplot(df_S_K, aes(K, S_0, fill = V_0_eu_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_S_K)) +
    labs(x = 'Cena wykonania K', y = 'Cena spot S_0', title = 'Opcja europejska call', fill = 'Cena') +
    theme_bw()
  
  euro_put_S_K = ggplot(df_S_K, aes(K, S_0, fill= V_0_eu_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_S_K)) +
    labs(x = 'Cena wykonania K', y = 'Cena spot S_0', title = 'Opcja europejska put', fill = 'Cena') +
    theme_bw()
  
  amer_call_S_K = ggplot(df_S_K, aes(K, S_0, fill = V_0_am_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_S_K)) +
    labs(x = 'Cena wykonania K', y = 'Cena spot S_0', title = 'Opcja amerykańska call', fill = 'Cena') +
    theme_bw()
  
  amer_put_S_K = ggplot(df_S_K, aes(K, S_0, fill = V_0_am_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_S_K)) +
    labs(x = 'Cena wykonania K', y = 'Cena spot S_0', title = 'Opcja amerykańska put', fill = 'Cena') +
    theme_bw()
  
  grid.arrange(euro_call_S_K, euro_put_S_K, amer_call_S_K, amer_put_S_K, nrow = 2)
  
  tytul = ggdraw() + 
    draw_label("Wycena opcji w chwili 0", fontface = 'bold', x = 0.5, y = 1, vjust = 1.5, hjust = -0.5)
  
  grid_plot = plot_grid(tytul, NULL, euro_call_S_K, euro_put_S_K, amer_call_S_K, amer_put_S_K, nrow = 3, rel_heights = c(0.05, rep(0.475, 2)))
  
  return(grid_plot)
}


#########################################################################
# PODMODUŁ 1.23: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI sigma I r DLA CALL I PUT
#########################################################################


wykres_wrazliwosci_sigma_r = function(sigma_vec1, r_vec1){
  df_sigma_r = data.frame(
    sigma = rep(sigma_vec1, times = length(r_vec1)),
    r = rep(r_vec1, each = length(sigma_vec1)),
    V_0_eu_call = 0,
    V_0_eu_put = 0,
    V_0_am_call = 0,
    V_0_am_put = 0
  )
  
  for(i in 1:nrow(df_sigma_r)){
    df_sigma_r[i,3] = wycena_europejska(df_sigma_r[i, 2], delta_t, df_sigma_r[i, 1], S_0, K, czas_T, "call")[[1]]
    df_sigma_r[i,4] = wycena_europejska(df_sigma_r[i, 2], delta_t, df_sigma_r[i, 1], S_0, K, czas_T, "put")[[1]]
    df_sigma_r[i,5] = wycena_amerykanska(df_sigma_r[i, 2], delta_t, df_sigma_r[i, 1], S_0, K, czas_T, "call")[[1]]
    df_sigma_r[i,6] = wycena_amerykanska(df_sigma_r[i, 2], delta_t, df_sigma_r[i, 1], S_0, K, czas_T, "put")[[1]]
  }
  
  maksymalna_call_sigma_r = max(max(df_sigma_r$V_0_eu_call), max(df_sigma_r$V_0_am_call))
  
  maksymalna_put_sigma_r = max(max(df_sigma_r$V_0_eu_put), max(df_sigma_r$V_0_am_put))
  
  euro_call_sigma_r = ggplot(df_sigma_r, aes(sigma, r, fill= V_0_eu_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_sigma_r)) +
    labs(x = 'Zmiennośc sigma', y = 'Stopa wolna od ryzyka r', title = 'Opcja europejska call', fill = 'Cena') +
    theme_bw()
  
  euro_put_sigma_r = ggplot(df_sigma_r, aes(sigma, r, fill= V_0_eu_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_sigma_r)) +
    labs(x = 'Zmiennośc sigma', y = 'Stopa wolna od ryzyka r', title = 'Opcja europejska put', fill = 'Cena') +
    theme_bw()
  
  amer_call_sigma_r = ggplot(df_sigma_r, aes(sigma, r, fill= V_0_am_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_sigma_r)) +
    labs(x = 'Zmiennośc sigma', y = 'Stopa wolna od ryzyka r', title = 'Opcja amerykańska call', fill = 'Cena') +
    theme_bw()
  
  amer_put_sigma_r = ggplot(df_sigma_r, aes(sigma, r, fill= V_0_am_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_sigma_r)) +
    labs(x = 'Zmiennośc sigma', y = 'Stopa wolna od ryzyka r', title = 'Opcja amerykańska put', fill = 'Cena') +
    theme_bw()
  
  grid.arrange(euro_call_sigma_r, euro_put_sigma_r, amer_call_sigma_r, amer_put_sigma_r, nrow = 2)
  
  tytul = ggdraw() + 
    draw_label("Wycena opcji w chwili 0", fontface = 'bold', x = 0.5, y = 1, vjust = 1.5, hjust = -0.5)
  
  grid_plot = plot_grid(tytul, NULL, euro_call_sigma_r, euro_put_sigma_r, amer_call_sigma_r, amer_put_sigma_r, nrow = 3, rel_heights = c(0.05, rep(0.475, 2)))
  
  
  return(grid_plot)
}


#######################################################################
# PODMODUŁ 1.24: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I S_0 DLA CALL I PUT
#######################################################################


wykres_wrazliwosci_T_S_0 = function(czas_T_vec1, S_0_vec1){
  df_czas_T_S_0 = data.frame(
    czas_T = rep(czas_T_vec1, times = length(S_0_vec1)),
    S_0 = rep(S_0_vec1, each = length(czas_T_vec1)),
    V_0_eu_call = 0,
    V_0_eu_put = 0,
    V_0_am_call = 0,
    V_0_am_put = 0
  )
  
  for(i in 1:nrow(df_czas_T_S_0)){
    df_czas_T_S_0[i, 3] = wycena_europejska(r, delta_t, sigma, df_czas_T_S_0[i, 2], K, df_czas_T_S_0[i, 1], "call")[[1]]
    df_czas_T_S_0[i, 4] = wycena_europejska(r, delta_t, sigma, df_czas_T_S_0[i, 2], K, df_czas_T_S_0[i, 1], "put")[[1]]
    df_czas_T_S_0[i, 5] = wycena_amerykanska(r, delta_t, sigma, df_czas_T_S_0[i, 2], K, df_czas_T_S_0[i, 1], "call")[[1]]
    df_czas_T_S_0[i, 6] = wycena_amerykanska(r, delta_t, sigma, df_czas_T_S_0[i, 2], K, df_czas_T_S_0[i, 1], "put")[[1]]
  }
  
  maksymalna_call_T_S = max(max(df_czas_T_S_0$V_0_eu_call), max(df_czas_T_S_0$V_0_am_call))
  maksymalna_put_T_S = max(max(df_czas_T_S_0$V_0_eu_put), max(df_czas_T_S_0$V_0_am_put))
  
  euro_call_T_S = ggplot(df_czas_T_S_0, aes(czas_T, S_0, fill = V_0_eu_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_T_S)) +
    labs(x = 'Zapadalność T', y = expression(paste('Cena spot ', S[0])), title = 'Opcja europejska call', fill = 'Cena') +
    theme_bw()
  
  euro_put_T_S = ggplot(df_czas_T_S_0, aes(czas_T, S_0, fill= V_0_eu_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_T_S)) +
    labs(x = 'Zapadalność T', y = expression(paste('Cena spot ', S[0])), title = 'Opcja europejska put', fill = 'Cena') +
    theme_bw()
  
  amer_call_T_S = ggplot(df_czas_T_S_0, aes(czas_T, S_0, fill = V_0_am_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_T_S)) +
    labs(x = 'Zapadalność T', y = expression(paste('Cena spot ', S[0])), title = 'Opcja amerykańska call', fill = 'Cena') +
    theme_bw()
  
  amer_put_T_S = ggplot(df_czas_T_S_0, aes(czas_T, S_0, fill = V_0_am_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_T_S)) +
    labs(x = 'Zapadalność T', y = expression(paste('Cena spot ', S[0])), title = 'Opcja amerykańska put', fill = 'Cena') +
    theme_bw()
  
  grid.arrange(euro_call_T_S, euro_put_T_S, amer_call_T_S, amer_put_T_S, nrow = 2)
  
  tytul = ggdraw() + 
    draw_label("Wycena opcji w chwili 0", fontface = 'bold', x = 0.5, y = 1, vjust = 1.5, hjust = -0.5)
  
  grid_plot = plot_grid(tytul, NULL, euro_call_T_S, euro_put_T_S, amer_call_T_S, amer_put_T_S, nrow = 3, rel_heights = c(0.05, rep(0.475, 2)))
  
  return(grid_plot)
}


#####################################################################
# PODMODUŁ 1.25: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I r DLA CALL I PUT
#####################################################################


wykres_wrazliwosci_T_r = function(czas_T_vec1, r_vec1){
  df_czas_T_r = data.frame(
    czas_T = rep(czas_T_vec1, times = length(r_vec1)),
    r = rep(r_vec1, each = length(czas_T_vec1)),
    V_0_eu_call = 0,
    V_0_eu_put = 0,
    V_0_am_call = 0,
    V_0_am_put = 0
  )
  
  for(i in 1:nrow(df_czas_T_r)){
    df_czas_T_r[i, 3] = wycena_europejska(df_czas_T_r[i, 2], delta_t, sigma, S_0, K, df_czas_T_r[i, 1], "call")[[1]]
    df_czas_T_r[i, 4] = wycena_europejska(df_czas_T_r[i, 2], delta_t, sigma, S_0, K, df_czas_T_r[i, 1], "put")[[1]]
    df_czas_T_r[i, 5] = wycena_amerykanska(df_czas_T_r[i, 2], delta_t, sigma, S_0, K, df_czas_T_r[i, 1], "call")[[1]]
    df_czas_T_r[i, 6] = wycena_amerykanska(df_czas_T_r[i, 2], delta_t, sigma, S_0, K, df_czas_T_r[i, 1], "put")[[1]]
  }
  
  maksymalna_call_T_r = max(max(df_czas_T_r$V_0_eu_call), max(df_czas_T_r$V_0_am_call))
  maksymalna_put_T_r = max(max(df_czas_T_r$V_0_eu_put), max(df_czas_T_r$V_0_am_put))
  
  euro_call_T_r = ggplot(df_czas_T_r, aes(czas_T, r, fill = V_0_eu_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_T_r)) +
    labs(x = 'Zapadalność T', y = 'Stopa wolna od ryzyka r', title = 'Opcja europejska call', fill = 'Cena') +
    theme_bw()
  
  euro_put_T_r = ggplot(df_czas_T_r, aes(czas_T, r, fill= V_0_eu_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_T_r)) +
    labs(x = 'Zapadalność T', y = 'Stopa wolna od ryzyka r', title = 'Opcja europejska put', fill = 'Cena') +
    theme_bw()
  
  amer_call_T_r = ggplot(df_czas_T_r, aes(czas_T, r, fill = V_0_am_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_T_r)) +
    labs(x = 'Zapadalność T', y = 'Stopa wolna od ryzyka r', title = 'Opcja amerykańska call', fill = 'Cena') +
    theme_bw()
  
  amer_put_T_r = ggplot(df_czas_T_r, aes(czas_T, r, fill = V_0_am_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_T_r)) +
    labs(x = 'Zapadalność T', y = 'Stopa wolna od ryzyka r', title = 'Opcja amerykańska put', fill = 'Cena') +
    theme_bw()
  
  grid.arrange(euro_call_T_r, euro_put_T_r, amer_call_T_r, amer_put_T_r, nrow = 2)
  
  tytul = ggdraw() + 
    draw_label("Wycena opcji w chwili 0", fontface = 'bold', x = 0.5, y = 1, vjust = 1.5, hjust = -0.5)
  
  grid_plot = plot_grid(tytul, NULL, euro_call_T_r, euro_put_T_r, amer_call_T_r, amer_put_T_r, nrow = 3, rel_heights = c(0.05, rep(0.475, 2)))
  
  print(grid_plot)
}


###########################################################################
# PODMODUŁ 1.26: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I delta_t DLA CALL I PUT
###########################################################################


wykres_wrazliwosci_T_delta_t = function(delta_t_vec = delta_t_vec, czas_T_vec = czas_T_vec){
  posortowane = sort(delta_t_vec^(-1), decreasing = TRUE)
  df_delta_t_T = data.frame(
    delta_t = rep(delta_t_vec, times = length(czas_T_vec)),
    czas_T = rep(czas_T_vec, each = length(delta_t_vec)),
    V_0_eu_call = 0,
    V_0_eu_put = 0,
    V_0_am_call = 0,
    V_0_am_put = 0
  )
  
  
  for(i in 1:nrow(df_delta_t_T)){
    df_delta_t_T[i,3] = wycena_europejska(r, df_delta_t_T[i, 1], sigma, S_0, K, df_delta_t_T[i, 2], "call")[[1]]
    df_delta_t_T[i,4] = wycena_europejska(r, df_delta_t_T[i, 1], sigma, S_0, K, df_delta_t_T[i, 2], "put")[[1]]
    df_delta_t_T[i,5] = wycena_amerykanska(r, df_delta_t_T[i, 1], sigma, S_0, K, df_delta_t_T[i, 2], "call")[[1]]
    df_delta_t_T[i,6] = wycena_amerykanska(r, df_delta_t_T[i, 1], sigma, S_0, K, df_delta_t_T[i, 2], "put")[[1]]
  }
  
  maksymalna_call_delta_t_T = max(max(df_delta_t_T$V_0_eu_call), max(df_delta_t_T$V_0_am_call))
  
  maksymalna_put_delta_t_T = max(max(df_delta_t_T$V_0_eu_put), max(df_delta_t_T$V_0_am_put))
  
  euro_call_delta_t_T = ggplot(df_delta_t_T, aes(as.factor(delta_t), czas_T, fill= V_0_eu_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_delta_t_T)) +
    labs(x = 'Liczba okresów w roku', y = 'Zapadalność T',title = 'Opcja europejska call', fill = 'Cena') +
    scale_x_discrete(labels = posortowane) +
    theme_bw()
  
  euro_put_delta_t_T = ggplot(df_delta_t_T, aes(as.factor(delta_t), czas_T, fill= V_0_eu_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_delta_t_T)) +
    labs(x = 'Liczba okresów w roku', y = 'Zapadalność T',title = 'Opcja europejska put', fill = 'Cena') +
    scale_x_discrete(labels = posortowane) +
    theme_bw()
  
  amer_call_delta_t_T = ggplot(df_delta_t_T, aes(as.factor(delta_t), czas_T, fill= V_0_am_call)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_call_delta_t_T)) +
    labs(x = 'Liczba okresów w roku', y = 'Zapadalność T',title = 'Opcja amaerykańska call', fill = 'Cena') +
    scale_x_discrete(labels = posortowane) +
    theme_bw()
  
  amer_put_delta_t_T = ggplot(df_delta_t_T, aes(as.factor(delta_t), czas_T, fill= V_0_am_put)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue", limits = c(0, maksymalna_put_delta_t_T)) +
    labs(x = 'Liczba okresów w roku', y = 'Zapadalność T', title = 'Opcja amerykańska put', fill = 'Cena') +
    scale_x_discrete(labels = posortowane) +
    theme_bw() 
  
  
  tytul = ggdraw() + 
    draw_label("Wycena opcji w chwili 0", fontface = 'bold', x = 0.5, y = 1, vjust = 1.5, hjust = -0.5)
  
  grid_plot = plot_grid(tytul, NULL, euro_call_delta_t_T, euro_put_delta_t_T, amer_call_delta_t_T, amer_put_delta_t_T, nrow = 3, rel_heights = c(0.05, rep(0.475, 2)))
  
  return(grid_plot)
}


########################################
# PODMODUŁ 1.27: PORTFEL ZABEZPIECZAJĄCY
########################################


portfel_zabezpieczajacy = function(r, delta_t, sigma, czas_T, V){
  n = czas_T/delta_t+1 # ile mamy kroków czasowych/wezlow drzewa na koncu
  S = matrix(NA, ncol=n, nrow=n) # tworzymy macierz z cenami instrumentu bazowego
  # u i d z funkcji
  u = u_fun(sigma, delta_t)
  d = d_fun(sigma, delta_t)
  # wypelniamy macierz cen instrumentu bazowego
  for(i in 1:n){
    for (j in 1:i){
      S[j, i] = S_0*u^(i-j)*d^(j-1)
    }
  }
  delta = matrix(NA, ncol=n, nrow=n) # macierz wartosci delta w kolejnych wierzcholkach
  alfa = matrix(NA, ncol=n, nrow=n) # macierz wartosci alfa w kolejnych wierzcholkach
  #spr1 = matrix(NA, ncol=n, nrow=n) # sprawdzamy czy obliczenia sie zgadzaja
  #spr2 = matrix(NA, ncol=n, nrow=n)
  # idziemy wiersz po wierszu kolumna po kolumnie
  for (i in 1:(n-1)){
    for (j in 1:i){
      delta[j, i] = (V[j, i+1]-V[j+1, i+1])/(S[j, i+1]-S[j+1, i+1]) # na podstawie wzoru z wykladu
      alfa[j, i] = V[j+1, i+1]*exp(-r*delta_t)-delta[j, i]*S[j+1, i+1]*exp(-r*delta_t) # na podstawie wzoru z wykladu
      #spr1[j, i]=delta[j, i]*S[j, i+1]+alfa[j, i]*exp(r*delta_t)
      #spr2[j, i]=delta[j, i]*S[j+1, i+1]+alfa[j, i]*exp(r*delta_t)
    }
  }
  return(list(delta, alfa)) # zwracamy macierz wartosci delta i macierz wartosci alfa
}


##################################################
# PODMODUŁ 1.28: PORTFEL ZABEZPIECZAJĄCY DLA delta
##################################################


porfel_zabezpieczajacy_delta = function() {
  par(mfrow=c(2,2))
  
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(delta_euro_call, 2, rev)),
             zlim = c(min_delta, maks_delta), 
             main = expression(paste("Wartość ", Delta, " dla europejskiej opcji call")),
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.lab = expression(paste("Wartość parametru ", Delta)))
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")

  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(delta_euro_put, 2, rev)),
             zlim = c(min_delta, maks_delta), 
             main = "Wartość delta dla europejskiej opcji put",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.lab = "Wartość parametru delta")
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")

  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(delta_amer_call, 2, rev)),
             zlim = c(min_delta, maks_delta), 
             main = "Wartość delta dla amerykanskiej opcji call",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.lab = "Wartość parametru delta")
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")

  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(delta_amer_put, 2, rev)),
             zlim = c(min_delta, maks_delta), 
             main = "Wartość delta dla amerykanskiej opcji put",
             xlab = "Czas",
             ylab = " ",
             axes = FALSE,
             legend.lab = "Wartość parametru delta")
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
  par(mfrow=c(1,1))
}


########################################
# PODMODUŁ 1.29: PORTFEL ZABEZPIECZAJĄCY
########################################


porfel_zabezpieczajacy_alpha = function() {
  par(mfrow=c(2,2))
  
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(alfa_euro_call, 2, rev)), zlim = c(min_alfa, maks_alfa), 
             main = "Wartość alfa dla europejskiej opcji call", xlab = "Czas", ylab = " ", axes = FALSE, legend.lab = "Wartość parametru alfa")
  
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
  
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(alfa_euro_put, 2, rev)), zlim = c(min_alfa, maks_alfa), 
             main = "Wartość alfa dla europejskiej opcji put", xlab = "Czas", ylab = " ", axes = FALSE, legend.lab = "Wartość parametru alfa")

  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
  
  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(alfa_amer_call, 2, rev)), zlim = c(min_alfa, maks_alfa), 
             main = "Wartość alfa dla amerykanskiej opcji call", xlab = "Czas", ylab = " ", axes = FALSE, legend.lab = "Wartość parametru alfa")
  
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")

  image.plot(seq(0, 2, length.out = 25), seq(1, 25, length.out = 25), 
             t(apply(alfa_amer_put, 2, rev)), zlim = c(min_alfa, maks_alfa), 
             main = "Wartość alfa dla amerykanskiej opcji put", xlab = "Czas", ylab = " ", axes = FALSE, legend.lab = "Wartość parametru alfa")
  
  axis(side = 1, at = seq(0, 2, length.out = 25), labels = seq(0, 2, length.out = 25))
  grid(czas_T/delta_t+1, czas_T/delta_t+1, col="black")
  par(mfrow=c(1,1))
}


##############################
# MODUŁ 2: RYSOWANIE WYKRESÓW 
##############################


##########################################
# PODMODUŁ 2.0: DANE POTRZEBNE DO WYKRESÓW
##########################################

euro_call = wycena_europejska()
euro_put = wycena_europejska(opcja = "put")
V_0_euro_call = euro_call[[1]]
V_0_euro_put = euro_put[[1]]
V_euro_call = euro_call[[2]]
V_euro_put = euro_put[[2]]


amer_call = wycena_amerykanska(r, delta_t, sigma, S_0, K, czas_T, "call")
amer_put = wycena_amerykanska(r, delta_t, sigma, S_0, K, czas_T, "put")
V_0_amer_call = amer_call[[1]]
V_0_amer_put = amer_put[[1]]
V_amer_call = amer_call[[2]]
V_amer_put = amer_put[[2]]
wyk_amer_call = amer_call[[3]]
wyk_amer_put = amer_put[[3]]

maksymalna_cena_call = max(V_euro_call, V_amer_call, na.rm=TRUE)
maksymalna_cena_put = max(V_euro_put, V_amer_put, na.rm=TRUE)

portfel_euro_call = portfel_zabezpieczajacy(r, delta_t, sigma, czas_T, V_euro_call)
delta_euro_call = portfel_euro_call[[1]]
alfa_euro_call = portfel_euro_call[[2]]

portfel_euro_put = portfel_zabezpieczajacy(r, delta_t, sigma, czas_T, V_euro_put)
delta_euro_put = portfel_euro_put[[1]]
alfa_euro_put = portfel_euro_put[[2]]

portfel_amer_call = portfel_zabezpieczajacy(r, delta_t, sigma, czas_T, V_amer_call)
delta_amer_call = portfel_amer_call[[1]]
alfa_amer_call = portfel_amer_call[[2]]

portfel_amer_put = portfel_zabezpieczajacy(r, delta_t, sigma, czas_T, V_amer_put)
delta_amer_put = portfel_amer_put[[1]]
alfa_amer_put = portfel_amer_put[[2]]

min_delta = min(delta_amer_call, delta_euro_call, delta_amer_put, delta_euro_put, na.rm=T)
maks_delta = max(delta_amer_call, delta_euro_call, delta_amer_put, delta_euro_put, na.rm=T)

min_alfa = min(alfa_amer_call, alfa_euro_call, alfa_amer_put, alfa_euro_put, na.rm=T)
maks_alfa = max(alfa_amer_call, alfa_euro_call, alfa_amer_put, alfa_euro_put, na.rm=T)


######################################################
# PODMODUŁ 2.1: WYKRES OBLICZEŃ WYCEN WSZYSTKICH OPCJI
######################################################


wyceny_opcji_wykres()
system.time({wyceny_opcji_wykres()}) # 0


#############################################################
# PODMODUŁ 2.2: CZY WYKONUJEMY AMERYKAŃSKIE OPCJE PUT I CALL?
#############################################################


czy_wykonujemy_amerykanska_wykres()
system.time({czy_wykonujemy_amerykanska_wykres()}) # 0.09


#######################################################################
# PODMODUŁ 2.3: ŁĄCZENIE JEDNOWYMIAROWYCH WYKRESÓW WRAŻLIWOŚCI DLA CALL 
#######################################################################


K = 48
delta_t = 1/12
S_0 = 50
delta_t_vec = c(1, 1/2, 1/4, 1/8, 1/12, 1/16, 1/20, 1/40)
sigma_vec = seq(0.1,1,by = 0.1)
S_0_vec = seq(S_0-25, S_0+25, 1)
czas_T_vec = seq(delta_t, 5, 0.25)
K_vec = seq(K-25, K+25, 1) 
r_vec = seq(-0.1,0.1,by = 0.005)

wykresy_wrazliwosci_jednowymiarowe(S_0_vec = S_0_vec, delta_t_vec = delta_t_vec, r_vec = r_vec, K_vec = K_vec, sigma_vec = sigma_vec, T_vec = T_vec)
system.time({wykresy_wrazliwosci_jednowymiarowe(S_0_vec = S_0_vec, delta_t_vec = delta_t_vec, r_vec = r_vec, K_vec = K_vec, sigma_vec = sigma_vec, T_vec = T_vec)
}) # 1.99


######################################################################
# PODMODUŁ 2.4: ŁĄCZENIE JEDNOWYMIAROWYCH WYKRESÓW WRAŻLIWOŚCI DLA PUT 
######################################################################


K = 48
delta_t = 1/12
S_0 = 50
delta_t_vec = c(1, 1/2, 1/4, 1/8, 1/12, 1/16, 1/20, 1/40)
sigma_vec = seq(0.1,1,by = 0.1)
S_0_vec = seq(S_0-25, S_0+25, 1)
czas_T_vec = seq(delta_t, 5, 0.25)
K_vec = seq(K-25, K+25, 1) 
r_vec = seq(-0.1,0.1,by = 0.005)

wykresy_wrazliwosci_jednowymiarowe(opcja = 'put', S_0_vec = S_0_vec, delta_t_vec = delta_t_vec, r_vec = r_vec, K_vec = K_vec, sigma_vec = sigma_vec, T_vec = T_vec)
system.time({wykresy_wrazliwosci_jednowymiarowe(opcja = 'put', S_0_vec = S_0_vec, delta_t_vec = delta_t_vec, r_vec = r_vec, K_vec = K_vec, sigma_vec = sigma_vec, T_vec = T_vec)
}) # 4.53



######################################################################
# PODMODUŁ 2.5: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI K I S_0 DLA CALL I PUT
######################################################################


K_vec1 = seq(45, 55, 1)
S_0_vec1 = seq(45, 55, 1)
wykres_wrazliwosci_K_S_0(K_vec1 = K_vec1, S_0_vec1 = S_0_vec1)
system.time({wykres_wrazliwosci_K_S_0(K_vec1 = K_vec1, S_0_vec1 = S_0_vec1)
}) # 2.65

########################################################################
# PODMODUŁ 2.6: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI r I sigma DLA CALL I PUT
########################################################################


sigma_vec1 = seq(0.1, 0.5, 0.05)
r_vec1 = seq(0.01, 0.05, 0.005)
wykres_wrazliwosci_sigma_r(sigma_vec1 = sigma_vec1, r_vec1 = r_vec1)
system.time({wykres_wrazliwosci_sigma_r(sigma_vec1 = sigma_vec1, r_vec1 = r_vec1)
}) #1.96

######################################################################
# PODMODUŁ 2.7: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I S_0 DLA CALL I PUT
######################################################################


czas_T_vec1 = seq(1, 3.5, 0.25)
S_0_vec1 = seq(25, 125, 10)
wykres_wrazliwosci_T_S_0(czas_T_vec1 = czas_T_vec1, S_0_vec1 = S_0_vec1)
system.time({wykres_wrazliwosci_T_S_0(czas_T_vec1 = czas_T_vec1, S_0_vec1 = S_0_vec1)
}) # 9.55


#####################################################################
# PODMODUŁ 2.8: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I r DLA CALL I PUT
#####################################################################


czas_T_vec1 = seq(1, 11, 1)
r_vec1 = seq(0.01, 0.05, 0.005)
wykres_wrazliwosci_T_r(czas_T_vec1 = czas_T_vec1, r_vec1 = r_vec1)
system.time({wykres_wrazliwosci_T_r(czas_T_vec1 = czas_T_vec1, r_vec1 = r_vec1)
}) # 11.95


###########################################################################
# PODMODUŁ 2.9: DWUWYMIAROWY WYKRES WRAŻLIWOŚCI T I delta_t DLA CALL I PUT
###########################################################################


delta_t_vec = c(1/2, 1/4, 1/8, 1/16, 1/32, 1/64) 
czas_T_vec = seq(1, 6, 1)
wykres_wrazliwosci_T_delta_t(delta_t_vec = delta_t_vec, czas_T_vec = czas_T_vec)
system.time({wykres_wrazliwosci_T_delta_t(delta_t_vec = delta_t_vec, czas_T_vec = czas_T_vec)
}) # 7.35


#############################################################
# PODMODUŁ 2.10: WYKRESY PORTFELi ZABEZPIECZAJĄCYCH DLA delta
#############################################################


porfel_zabezpieczajacy_delta()
system.time({porfel_zabezpieczajacy_delta()
}) # 0.08


#############################################################
# PODMODUŁ 2.11: WYKRESY PORTFELi ZABEZPIECZAJĄCYCH DLA alpha
#############################################################


porfel_zabezpieczajacy_alpha()
system.time({porfel_zabezpieczajacy_alpha()
}) # 0.81

