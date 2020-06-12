# BNWeatherGen
## Weather Generators with Bayesian Networks.

Implements Bayesian networks as discrete multivariate weather generators in [R](https://www.r-project.org). A small tutorial with several examples is provided in the [companion notebook](https://github.com/MNLR/BNWeatherGen/blob/master/2019_BNWeatherGen_WRR.ipynb).

This is a novel methodology in the multisite stochastic weather generation game, which we proposed and thoroughly analyzed in the article [*Multisite Weather Generators using Bayesian Networks: An illustrative case study for precipitation occurrence*](https://doi.org/10.1029/2019WR026416), published in **Water Resources Research**. It also has some experimental functionality not restricted to weather generators, like predictive networks for downscaling purposes.

This package relies on [**bnlearn**](https://www.bnlearn.com), plus the packages [**gRain**](https://cran.r-project.org/web/packages/gRain/index.html) (for exact inference) and  [**iGraph**](https://igraph.org/c/) (for visualization). 

---
The installation process is as follows.

Firstly, some dependencies of gRain are installed through [Bioconductor](https://www.bioconductor.org). It can be installed with the following commands:

```
install.packages("BiocManager")
BiocManager::install(version = "3.10")
```

Afterwards, we install the dependencies of `gRain` from Bioconductor and then `gRain` itself from CRAN:

```
BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
install.packages("gRain", dependencies = TRUE)
```

With `gRain` installed, we can install the package `BNWeatherGen` directly from Github using `devtools` library, with the following command:

```
devtools::install_github("MNLR/BNWeatherGen")
```

Note that, if `devtools` is not already installed, it can be installed from CRAN using the command `install.packages("devtools")`. 
