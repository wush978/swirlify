% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yaml_writer.R
\name{wq_multiple}
\alias{wq_multiple}
\title{Template for multiple choice question}
\usage{
wq_multiple(output = "ask the multiple choice question here",
  answer_choices = c("ANS", "2", "3"), correct_answer = "ANS",
  answer_tests = "omnitest(correctVal= 'ANS')", hint = "hint")
}
\arguments{
\item{output}{Text that is displayed to the user.}

\item{answer_choices}{A vector of strings containing a user's choices.}

\item{correct_answer}{A string that designates the correct answer.}

\item{answer_tests}{An internal function from \code{swirl} for testing the 
user's choice. See \code{\link[swirl]{AnswerTests}}.}

\item{hint}{A string that is printed to the console if the user answers this
question incorrectly.}
}
\description{
Template for multiple choice question
}
\examples{
\dontrun{
# While writing a new lesson by hand just use:
wq_multiple()

# If converting from another format to a swirl course you may want to sue the
# API:
wq_multiple("Which of the following is not a planet in our solar system?",
 c("Venus", "Saturn", "Pluto"), "Pluto", "omnitest(correctVal= 'Pluto')",
   "It's the smallest celestial body you can choose.")
}
}
