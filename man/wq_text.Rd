% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yaml_writer.R
\name{wq_text}
\alias{wq_text}
\title{Template for text question}
\usage{
wq_text(output = "explain the question here",
  correct_answer = "answer",
  answer_tests = "omnitest(correctVal='answer')", hint = "hint")
}
\arguments{
\item{output}{Text that is displayed to the user.}

\item{correct_answer}{The answer to the question in the form of a string.}

\item{answer_tests}{An internal function from \code{swirl} for testing the 
user's choice. See \code{\link[swirl]{AnswerTests}}.}

\item{hint}{A string that is printed to the console if the user answers this
question incorrectly.}
}
\description{
Template for text question
}
\examples{
\dontrun{
# While writing a new lesson by hand just use:
wq_text()

# If converting from another format to a swirl course you may want to sue the
# API:
wq_text("Where is the Johns Hopkins Bloomberg School of Public Health located?",
 "Baltimore", "omnitest(correctVal='Baltimore')", "North of Washington, south of Philadelphia.")
}
}
