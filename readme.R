## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:1]]
library(MASS)
k = 4
cr <- matrix(c(1,0.9,0,0,
               0.9,1,0,0,
               0,0,1,.8,
               0,0,.8,1),k, byrow = T)
sd.mat <- as.matrix(rep(1,k))
sig = cr *sd.mat%*%t(sd.mat)
d <- mvrnorm(n = 100, mu = rep(0,k), sig)
## made up data:1 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:2]]
pairs(d)
## made up data:2 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:3]]
df <- data.frame(d)

  gm <- lm(X1 + X2 + X3 + X4 ~ X1 + X2 + X3 + X4, df, na.action = "na.fail")
  dm <- dredge(gm, extra = c("R^2"))

dtt <- model.sel(dm) %>%
    dplyr::select(X1, X2, X3, X4, `R^2`) %>%
    arrange(-`R^2`)
## made up data:3 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:4]]
dtt %>% org
## made up data:4 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:6]]
n_total_preds <- 4

  dtt <-  dtt %>%
      mutate(n_preds = n_total_preds - rowSums(is.na(.))) %>%
      arrange(-n_preds, -`R^2`)


  dttt <- dtt %>%
      group_by(n_preds) %>%
      summarize(max_R2 = max(`R^2`)) %>%
    data.frame()

dttt %>% org(digits = 4)
## made up data:6 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:8]]
ggplot(dttt, aes(x = n_preds, y = max_R2)) + geom_col()
## made up data:8 ends here

## [[file:~/git/trait_variation/readme.org::*made%20up%20data][made up data:9]]
dtttt <- left_join(dttt, dtt, by = c("max_R2" = "R^2", "n_preds"))
dtttt
## made up data:9 ends here

## [[file:~/git/trait_variation/readme.org::*trait%20data][trait data:1]]
library(dplyr)
  library(ggplot2)
  library(GGally)
  library(ascii)
  options(asciiType = "org")
org <- function(x,...) {
         suppressWarnings(print(ascii(x,...)))

  }
## trait data:1 ends here

## [[file:~/git/trait_variation/readme.org::*trait%20data][trait data:2]]
d <- read.csv("data/NEON_pixel_traits_7domains_new_trait.csv") %>%
  sample_n(500)
## trait data:2 ends here

## [[file:~/git/trait_variation/readme.org::*trait%20data][trait data:3]]
d <- d %>% dplyr::select(-Domain, -Domain_Site)
ds <- as.data.frame(scale(d))

colnames(ds) <- colnames(d)
## trait data:3 ends here

## [[file:~/git/trait_variation/readme.org::*trait%20data][trait data:4]]
ggcorr(ds,  nbreaks = 7)
## trait data:4 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:1]]
d.sub1 <- dplyr::select(ds, Carbon, LMA, Nitrogen, Zinc, d13C, EWT)
#d.sub2 <- select(d, Carbon, LMA, Nitrogen, Sugar, starch, Potassium, Lignin, Zinc, d13C, EWT)
d.sub <- d.sub1
## Test on a subset of traits:1 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:2]]
ggcorr(d.sub,  nbreaks = 7)
## Test on a subset of traits:2 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:3]]
ggpairs(d.sub)
## Test on a subset of traits:3 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:4]]
library(MuMIn)
library(broom)
library(parallel)

#gm <- lm(Carbon + LMA + Nitrogen + Sugar + starch + Potassium + Lignin + Zinc + d13C + EWT ~ Carbon + LMA + Nitrogen + Sugar + starch + Potassium + Lignin + Zinc + d13C + EWT, data = d.sub, na.action = "na.fail")
gm <- lm(Carbon + LMA + Nitrogen +  Zinc + d13C + EWT ~ Carbon + LMA + Nitrogen + Zinc + d13C + EWT, data = d.sub, na.action = "na.fail")
#dm <- dredge(gm, extra = c("R^2"))


cores<-4
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", cores), type = clusterType))
myclust<-clust

clusterExport(myclust, c("d.sub"), envir=environment())
gm <- lm(Carbon + LMA + Nitrogen +  Zinc + d13C + EWT ~ Carbon + LMA + Nitrogen + Zinc + d13C + EWT, data = d.sub, na.action = "na.fail")
dm <- pdredge(gm, cluster=myclust, extra = c("R^2"))
## Test on a subset of traits:4 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:5]]
dtt <- model.sel(dm) %>%
#      select(Carbon, LMA, Nitrogen, Sugar, starch, Potassium, Lignin, Zinc, d13C, EWT, `R^2` ) %>%
      dplyr::select(Carbon, LMA, Nitrogen, Zinc, d13C, EWT, `R^2` ) %>%
      arrange(-`R^2`)

  dtt %>% head%>%  org(digits = 4)
#  dtt %>%  org(digits = 4)
## Test on a subset of traits:5 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:7]]
n_total_preds <- 10
  n_total_preds <- 6

  dtt <-  dtt %>%
      mutate(n_preds = n_total_preds - rowSums(is.na(.))) %>%
      arrange(-n_preds, -`R^2`)

#  dtt %>% head %>% org(digits = 4)
  dtt %>% org(digits = 4)
## Test on a subset of traits:7 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:9]]
dttt <- dtt %>%
      group_by(n_preds) %>%
      summarize(max_R2 = max(`R^2`)) %>%
    data.frame()

dttt %>% org(digits = 4)
## Test on a subset of traits:9 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:11]]
dtttt <- left_join(dttt, dtt, by = c("max_R2" = "R^2", "n_preds"))
dtttt
## Test on a subset of traits:11 ends here

## [[file:~/git/trait_variation/readme.org::*Test%20on%20a%20subset%20of%20traits][Test on a subset of traits:12]]
ggplot(dttt, aes(x = n_preds, y = max_R2)) + geom_col()
## Test on a subset of traits:12 ends here

## [[file:~/git/trait_variation/readme.org::*All%20traits][All traits:4]]
dm <- readRDS("data/dredge22traits.rds")

dtt <- model.sel(dm) %>%
        dplyr::select(-`(Intercept)`, -logLik, -AICc, -delta, -weight) %>%
      arrange(-`R^2`)

  dtt <-  dtt %>%
      mutate(n_preds = df - 2) %>%
      group_by(n_preds) %>%
      summarize(max_R2 = max(`R^2`)) %>%
    data.frame()

dtt %>% org(digits = 4)
## All traits:4 ends here

## [[file:~/git/trait_variation/readme.org::*All%20traits][All traits:6]]
ggplot(dtt, aes(x = n_preds, y = max_R2)) + geom_col() +
  scale_y_continuous(breaks = seq(0,1,.1))
## All traits:6 ends here

## [[file:~/git/trait_variation/readme.org::*All%20traits][All traits:7]]
dtttt <- left_join(dttt, dtt, by = c("max_R2" = "R^2", "n_preds"))
dtttt
## All traits:7 ends here
