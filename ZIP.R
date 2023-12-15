# Advanced Examples
library(sf)
Jabar_map <- read_sf('[geosai.my.id]Jawa_Barat_Kab/Jawa_Barat_ADMIN_BPS.shp')
Jabar_map <- Jabar_map[,-c(1:4,6)]
Jabar_map <- Jabar_map[-28,]
plot(Jabar_map)
head(Jabar_map)
tail(Jabar_map)
Jabar_map$Kabupaten
library(readxl)
Filariasis <- read_excel('Filariasis Jabar 2019.xlsx',sheet = "Perempuan Mati")
head(Filariasis)

Filariasis_df <- merge(Filariasis,Jabar_map,by="Kabupaten")
head(Filariasis_df)

# Modelling
library(INLA)
library(spdep)
nb <- poly2nb(Jabar_map)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


Filariasis_df$idarea <- match(Filariasis_df$Kabupaten,unique(Filariasis_df$Kabupaten))
Filariasis_df$idarea
Filariasis_df$idarea1 <- Filariasis_df$idarea
Filariasis_df$idtime <- 1 + Filariasis_df$year - min(Filariasis_df$year)


formula <- Y ~ f(idarea, model = "bym", graph = g) +
  f(idarea1, model = "iid") + idtime

res <- inla(formula,
            family = "poisson", data = Filariasis_df,
            control.predictor = list(compute = TRUE)
)
summary(res)

# Joint model
n = length(Filariasis_df$Y)

Z <- as.vector(matrix(0,ncol = 1,nrow = length(Filariasis_df$Y)))
O <- as.vector(matrix(0,ncol = 1,nrow = length(Filariasis_df$Y)))

for(i in 1:length(Filariasis_df$Y)){
  if (Filariasis_df$Y[i]>0){
    Z[i] = 1
    O[i] = Filariasis_df$Y[i]
  } else {
    Z[i] = 0
    O[i] = NA
  }
}

print(Z)
print(O)

Y <- matrix(NA,ncol = 2,nrow = 2*n)
Y[1:n,1] <- Z
Y[(n+1):(2*n),2] <- O

df <- as.list(Filariasis_df)
df$Y <- Y

library(SpatialEpi)
n.strata <- 1
E <- expected(
  population = Filariasis_df$x2,
  cases = Filariasis_df$Y,
  n.strata = n.strata
)
E <- rep(E,2)
E

df$mu.z = rep(1:0,c(n,n))

df$mu.o = rep(0:1,c(n,n))

df$idarea <- rep(df$idarea,2)
df$idarea1 <- rep(df$idarea1,2)
df$idtime <- rep(df$idtime,2)

formula = Y ~ 1 + mu.z + mu.o +
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2 <- inla(formula,family = c('binomial','poisson'),
               data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
               control.compute = list(dic=TRUE,cpo=TRUE))


round(r.bym2$summary.fix, 4)
summary(r.bym2)
df$re <- r.bym2$summary.random$idarea[1:54, "mean"]
# Plotting
df$Y <- df$Y[1:27]
df$idarea <- df$idarea[1:27]
df$idarea1 <- df$idarea1[1:27]
df$idtime <- df$idtime[1:27]
df$mu.z <- df$mu.z[1:27]
df$mu.o <- df$mu.o[1:27]
df$re <- df$re[1:27]
df <- as.data.frame(df)
df <- st_as_sf(df)
library(ggplot2)
hist(Filariasis_df$Y,main="Sebaran kasus Filariasis di Jawa Barat")
ggplot(df) + geom_sf(aes(fill = re)) +
  scale_fill_gradient2(
    midpoint = 0, low = "green", mid = "white", high = "red"
  ) +
  theme_bw()

# Plus X
# Joint model
n = length(Filariasis_df$Y)

Z <- as.vector(matrix(0,ncol = 1,nrow = length(Filariasis_df$Y)))
O <- as.vector(matrix(0,ncol = 1,nrow = length(Filariasis_df$Y)))

for(i in 1:length(Filariasis_df$Y)){
  if (Filariasis_df$Y[i]>0){
    Z[i] = 1
    O[i] = Filariasis_df$Y[i]
  } else {
    Z[i] = 0
    O[i] = NA
  }
}

print(Z)
print(O)

Y <- matrix(NA,ncol = 2,nrow = 2*n)
Y[1:n,1] <- Z
Y[(n+1):(2*n),2] <- O

df <- as.list(Filariasis_df)
df$Y <- Y

library(SpatialEpi)
n.strata <- 1
E <- expected(
  population = Filariasis_df$x2,
  cases = Filariasis_df$Y,
  n.strata = n.strata
)
E <- rep(E,2)
E

df$mu.z = rep(1:0,c(n,n))

df$mu.o = rep(0:1,c(n,n))

df$idarea <- rep(df$idarea,2)
df$idarea1 <- rep(df$idarea1,2)
df$idtime <- rep(df$idtime,2)
df$x1 <- rep(df$x1,2)
df$x1 <- df$x1/100
df$x2 <- rep(df$x2,2)
df$x2 <- df$x2/1000
df$x3 <- rep(df$x3,2)
df$x3 <- df$x3/100
df$x4 <- rep(df$x4,2)
df$x4 <- df$x4/100
formula_x1_x2 = Y ~ 1 + mu.z + mu.o + x1+x2+
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x1_x2 <- inla(formula_x1_x2,family = c('binomial','poisson'),
               data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
               control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x1_x2)

formula_x2_x3 = Y ~ 1 + mu.z + mu.o + x2+x3+
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x2_x3 <- inla(formula_x2_x3,family = c('binomial','poisson'),
                     data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
                     control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x2_x3)

formula_x1 = Y ~ 1 + mu.z + mu.o + x1 +
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x1 <- inla(formula_x1,family = c('binomial','poisson'),
                     data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
                     control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x1)

formula_x2 = Y ~ 1 + mu.z + mu.o + x2 +
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x2 <- inla(formula_x2,family = c('binomial','poisson'),
                  data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
                  control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x2)

formula_x4 = Y ~ 1 + mu.z + mu.o + x4 +
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x4 <- inla(formula_x4,family = c('binomial','poisson'),
                  data=df,E=E,verbose=F,control.predictor = list(compute=TRUE,link=TRUE),
                  control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x4)

formula_x2_x4 = Y ~ 1 + mu.z + mu.o + x2+ x4 +
  f(idarea, model = "bym2", graph = g, scale.model = TRUE,
    constr = TRUE,
    hyper = list(phi=list(prior="pc",
                          param = c(0.5,2/3),initial=3),
                 prec = list(prior="pc.prec",
                             param = c(1,0.01),
                             initial=1.5)))+
  f(idarea1,copy = 'idarea',fixed = FALSE)

r.bym2_x2_x4 <- inla(formula_x2_x4,family = c('binomial','poisson'),
                  data=df,E=E,verbose=T,control.predictor = list(compute=TRUE,link=TRUE),
                  control.compute = list(dic=TRUE,cpo=TRUE))
summary(r.bym2_x2_x4)

df$re <- r.bym2_x2_x4$summary.random$idarea[1:54, "mean"]

# Plotting
df$Y <- Filariasis_df$Y
df$idarea <- df$idarea[1:27]
df$idarea1 <- df$idarea1[1:27]
df$idtime <- df$idtime[1:27]
df$mu.z <- df$mu.z[1:27]
df$mu.o <- df$mu.o[1:27]
df$re <- df$re[1:27]
df$x1 <- df$x1[1:27]
df$x2 <- df$x2[1:27]
df$x3 <- df$x3[1:27]
df$x4 <- df$x4[1:27]
df <- as.data.frame(df)
df <- st_as_sf(df)

# Plotting 1:27
library(ggplot2)
ggplot(df) + geom_sf(aes(fill = re)) +
  scale_fill_gradient2(
    midpoint = 0, low = "green", mid = "white", high = "red"
  ) +
  theme_bw()+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(df,"Spatial"))[,1], y = coordinates(as(df,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 4,
    check_overlap = TRUE
  )

# 28-54
df$re <- r.bym2_x2_x4$summary.random$idarea[28:54, "mean"]
# Plotting
ggplot(df) + geom_sf(aes(fill = re)) +
  scale_fill_gradient2(
    midpoint = 0, low = "green", mid = "white", high = "red"
  ) +
  theme_bw()+
  geom_text(
    aes(label = Kabupaten, x = coordinates(as(df,"Spatial"))[,1], y = coordinates(as(df,"Spatial"))[,2]),
    vjust = -0.5,
    color = "black",
    size = 4,
    check_overlap = TRUE
  )
