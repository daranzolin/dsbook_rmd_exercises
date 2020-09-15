library(glue)
library(tidyverse)

system("git clone https://github.com/rafalab/dsbook.git dsbook")

rmds <- rmd_dirs %>%
  map(~dir(glue("dsbook/{.x}"), full.names = TRUE, all.files = TRUE, pattern = "Rmd$")) %>%
  set_names(rmd_dirs)

header_yml <- c(
  "---",
  "author: Your Name",
  "date: '`r format(Sys.Date())`'",
  "title: Homework Assignment",
  "---"
)

rmd_dirs <- c("R", "dataviz", "inference", "prob", "regression", "wrangling", "ml")
walk(rmd_dirs, dir.create)

extract_exercise_lines <- function(x) {
  lines <- readLines(x)
  n_lines <- length(lines)
  header_lines <- grep("^##", lines)
  exercise_lines <- grep("^## Exercises", lines)
  lines_to_extract <- map(exercise_lines, ~{
    ind <- grep(glue("^{.x}$"), header_lines)
    if (ind == length(header_lines)) stop <- n_lines
    else stop <- header_lines[ind + 1]
    out <- seq(.x, stop)
    out
    })
  lines[flatten_dbl(lines_to_extract)]
}

lines_to_rmd <- function(x, chapter, i) {
  f <- glue("{chapter}/Exercises {i}.Rmd")
  filecon <- file(f)
  cat("Writing file ", f, "\n")
  writeLines(c(header_yml, x), con = filecon)
  close(filecon)
}

safe_extract_lines <- safely(extract_exercise_lines)
safe_lines_to_rmd <- safely(lines_to_rmd)

walk(rmd_dirs, function(chapter) {
  ch <- rmds[[chapter]]
  i <- 1
  walk(ch, function(file) {
    lines <- extract_exercise_lines(file)
    if (length(lines) == 0) cat("doing nothing", "\n")
    else {
      lines_to_rmd(lines, chapter, i)
      i <- i + 1
    }
  })
})


