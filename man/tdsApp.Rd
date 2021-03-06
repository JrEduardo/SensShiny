% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tds.R
\name{tdsApp}
\alias{tdsApp}
\title{Web Interface for a Temporal Dominance of Sensations
    Experiment}
\usage{
tdsApp(attributes, max_time = 60, shiny_dir = tempdir())
}
\arguments{
\item{attributes}{A vector names representing the attributes that
compose a sensory analysis.}

\item{max_time}{A numeric representing a max time, in seconds,
allowed for each consumer.}

\item{shiny_dir}{Path name of the directory that will be used to save
the source files from shiny interface (\code{ui.R} and
\code{server.R}). Use to customize the application, set the
directory and edit the \code{ui.R} adn \code{server.R} files. By
default, source files are saved in temporary directories
(\code{\link[base]{tempdir}()}).}
}
\value{
Open the web browser for show the shiny interface.
}
\description{
This function builds a consumer web interface for
    Temporal Dominance of Sensations experiment.
}
\author{
Eduardo E. R. Junior <edujrrib@gmail.com>.
}
