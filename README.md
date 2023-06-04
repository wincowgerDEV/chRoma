# chRoma
An R package for a Chroma-like vector database for managing large collections of high-dimensional vectors and their associated metadata. 

## Installation

You can install the package from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("wincowgerDEV/chRoma")
```

## Usage
Here is a basic example:

```r
library(vectorDB)
#> Loading required package: vectorDB

# Create a collection
db <- create_collection()

# Add vectors and metadata
vectors <- list(c(1.2, 2.3, 4.5), c(6.7, 8.2, 9.2))
metadatas <- list(list(text = "This is a document", file = "source1"), list(text = "This is another document", file = "source2"))

db <- add_collection(db, vectors, metadatas)

## Testing
You can run the package tests with:

```r
devtools::test()
```

## Contributing
Please note that the vectorDB project is released with a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.

## Funding
This work was funded in part by the National Renewable Energy Laboratory through the Water PACT project. 
