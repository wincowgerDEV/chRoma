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
library(chRoma)
#> Loading required package: chRoma

# Vector database opperations when you have your own vector embeddings ----
## Create a collection
db <- create_collection()

## Format your vectors and metadata
vectors <- list(c(1.2, 2.3, 4.5), c(6.7, 8.2, 9.2))
metadata <- list(list(text = "This is a document", file = "source1"), list(text = "This is another document", file = "source2"))

## Add your vectors and metadata to the database
db <- add_collection(db, vectors, metadata)
db
## Check that object is a vector database, should return TRUE
is_vectorDB(db)

## Compare the embeddings in the database to themselves, change query_embeddings if you want to use a different database.
similarity_internal <- query_collection(db, query_embeddings = db, top_n = 2)
similarity_internal

## Compare the database to another by first using a structured filter and then providing an embedding instead of a database and return the top 1. 
similarity <- query_collection(db,
                                filter = "file == 'source1'",
                                query_embeddings = c(1.5, 2.7, 4.9),
                                top_n = 1)
similarity

# Vector database opperations when you only have text you want to analyze ----

## Read in your OpenAI API Key
Sys.setenv(OPENAI_API_KEY = readLines("openai.txt"))

## Setup the words you want to build the database for. 
words_to_retrieve <- data.table(text = c("test", "experiment", "elephant"))

## Create a vector database using the words found in the text column of the metadata input. You'll notice we didn't specify a db, it created a new empty one by default. It also grabs the OPENAI_API_KEY you set previously so no need to call that again. 
db_2 <- add_collection(metadata = words_to_retrieve)

```
## Testing
You can run the package tests with:

```r
devtools::test()
```

## Contributing
Please note that the vectorDB project is released with a Contributor Code of Conduct. By contributing to this project, you agree to abide by its terms.

## Funding
This work was funded in part by the National Renewable Energy Laboratory through the Water PACT project. 
