#' Categorize Ages into Medical Age Classes
#'
#' This function assigns each input age to a meaningful medical age category
#' based on life-stage classifications derived from
#' [Geifman et al. (2013)](https://pmc.ncbi.nlm.nih.gov/articles/PMC3825015/)
#' *Redefining meaningful age groups in the context of disease*,
#' *Age (Dordr)*, 35(6): 2357–2366.
#'
#' @param ages Numeric vector of ages to categorize.
#'
#' @return A factor vector of categorized age ranges, with an attribute
#' `'age_class_info'` providing the mapping table.
#'
#' @seealso
#' Full text available at:
#' <https://pmc.ncbi.nlm.nih.gov/articles/PMC3825015/>
#'
#' @examples
#' categorize_medical_age(c(10, 25, 70))
#' @export
categorize_medical_age <- function(ages) {
  # Validate input --------------------------------------------------------------
  if (missing(ages) || !is.numeric(ages)) {
    stop("'ages' must be a numeric vector.", call. = FALSE)
  }

  # Define reference table ------------------------------------------------------
  age_class_table <- read.table(
    header = TRUE,
    sep = ';',
    text =
      "Range;ColumnName;Description
0–2 years;Infancy;Rapid physical growth, early development
3–5 years;Early childhood;Language, sensory, and motor development
6–13 years;Childhood (school age);Steady growth and cognitive development
14–18 years;Adolescence;Onset of puberty, increased independence, mental health risks
19–33 years;Young adulthood;Peak physical health, reproductive health focus
34–48 years;Early middle age;Early signs of chronic disease risk
49–64 years;Late middle age;Routine screenings for cardiovascular and cancer risk
65–78 years;Early old age;Higher risks for chronic illness, cognitive decline
79+ years;Advanced age;Complex chronic conditions, geriatrics"
  )

  # Define class breaks ---------------------------------------------------------
  age_breaks <- c(0, 2, 5, 13, 18, 33, 48, 64, 78, Inf)

  # Categorize ages -------------------------------------------------------------
  classified_age <- cut(ages, breaks = age_breaks, include.lowest = TRUE)
  levels(classified_age) <- age_class_table$ColumnName

  # Attach data dictionary ------------------------------------------------------
  attr(classified_age, "age_class_info") <- age_class_table

  return(classified_age)
}
