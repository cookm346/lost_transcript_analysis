library(rmarkdown)

render("lost_transcript_analysis.Rmd", 
       md_document(variant = "markdown_github"), 
       output_file = "README.md")