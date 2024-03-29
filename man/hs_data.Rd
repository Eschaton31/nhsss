% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hs_data.R
\name{hs_data}
\alias{hs_data}
\title{NHSSS Dataset Retreiver}
\usage{
hs_data(sys = NULL, type = "reg", yr = NULL, mo = NULL, file_type = "dta")
}
\arguments{
\item{sys}{coded name of surveillance system
\itemize{
\item \code{"harp_dx"}: HARP Diagnosis
\item \code{"harp_tx"}: HARP Treatmentt
\item \code{"harp_dead"}: HARP Mortality
\item \code{"harp_vl"}: HARP VL Subsmission
\item \code{"prep"}: PrEP
}}

\item{type}{type of dataset to be returned, By default, returns registries.
\itemize{
\item \code{"reg"}: line-list registry
\item \code{"outcome"}: line-list outcomes/status
\item \code{"ml"}: (harp_vl only) vl masterlist
\item \code{"naive_dx"}: (harp_vl only) first/last vl results among dx
\item \code{"naive_tx"}: (harp_vl only) first/last vl results among tx
\item \code{"all"}: (harp_vl only) all historical vl data
}}

\item{yr}{reporting year of data}

\item{mo}{reporting month of data}

\item{file_type}{file extension of the data to retreived}
}
\value{
returns the full path of the latest version of the monthly dataset
}
\description{
This function retreives the datasets of the NHSSS Unit based on the naming
convention established and in-use.
}
