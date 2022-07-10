# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Load documentation
variables <- read_csv("codebook/csv/variables.csv")
datasets <- read_csv("codebook/csv/datasets.csv")

# Save documentation
save(variables, file = "data/variables.RData")
save(datasets, file = "data/datasets.RData")

# Document data
codebookr::document_data(
  file_path = "R/",
  variables_input = variables,
  datasets_input = datasets,
  include_variable_type = TRUE,
  author = "Joshua C. Fjelstul, Ph.D.",
  package = "worldcup"
)

# Create package documentation
devtools::document()

# Create a codebook
codebookr::create_codebook(
  file_path = "codebook/pdf/world-cup-codebook.tex",
  datasets_input = datasets,
  variables_input = variables,
  title_text = "The Fjelstul World Cup Database",
  version_text = "1.0",
  footer_text = "The Fjelstul World Cup Database \\hspace{5pt} | \\hspace{5pt} Joshua C. Fjelstul, Ph.D.",
  author_names = "Joshua C. Fjelstul, Ph.D.",
  theme_color = "#4B94E6",
  heading_font_size = 32,
  subheading_font_size = 14,
  title_font_size = 16,
  table_of_contents = TRUE,
  include_variable_type = TRUE
)
