## [[file:~/git/trait_variation/readme.org::*All%20traits][All traits:1]]
library(MuMIn)
  library(broom)
  library(parallel)
  library(dplyr)

  d <- read.csv("data/NEON_pixel_traits_7domains_new_trait.csv") %>%
    sample_n(500)


  d <- d %>% dplyr::select(-Domain, -Domain_Site)
  ds <- as.data.frame(scale(d))
  colnames(ds) <- colnames(d)

  cores <- 32

  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", cores), type = clusterType))
  myclust<-clust

  clusterExport(myclust, c("ds"), envir=environment())

  gm <- lm(Carbon +
           Aluminum +
           Carotenoids_area +
           Carotenoids_mass +
           Cellulose +
           Chlorophyll_area +
           Chlorophyll_mass +
           d13C +
           EWT +
           Flavonoids +
           Iron +
           Lignin +
           LMA +
           Manganese +
           Nitrogen +
           Phosphorous +
           Potassium +
           Sugar +
           starch +
           Phenolics +
           Water +
           Zinc ~
           Carbon +
           Aluminum +
           Carotenoids_area +
           Carotenoids_mass +
           Cellulose +
           Chlorophyll_area +
           Chlorophyll_mass +
           d13C +
           EWT +
          Flavonoids +
          Iron +
          Lignin +
          LMA +
          Manganese +
          Nitrogen +
           Phosphorous +
           Potassium +
           Sugar +
           starch +
           Phenolics +
           Water +
           Zinc, data = ds, na.action = "na.fail")


  dm <- pdredge(gm, cluster=myclust, extra = c("R^2"))

saveRDS(dm, "data/dredge22traits.rds")
## All traits:1 ends here
