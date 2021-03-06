% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_to_xlsx.R
\name{write_to_xlsx}
\alias{write_to_xlsx}
\title{Write content to an Excel workbook}
\usage{
write_to_xlsx(x, wb_name = NULL, open_wb = TRUE)
}
\arguments{
\item{x}{An R object or list of R objects}

\item{wb_name}{(Optional) The name of the workbook to be created (e.g. 'output.xlsx' or 'output/results.xlsx'). If no value is specified, a new workbook is created in the working directory.}

\item{open_wb}{Whether to open the workbook using Excel once it's been created}
}
\value{
Creates an Excel workbook at the path provided by `wb_name` or as a new file in the working directory. If `open_wb = TRUE`, the workbook will be opened in Excel if possible.
}
\description{
Write content to an Excel workbook
}
\examples{
df_list <- list('iris' = iris, mtcars, 'Arrests' = USArrests)

# With a specified output path
write_to_xlsx(df_list, wb_name = "output/r-default-datasets.xlsx")

# With an automatically-generated output path
write_to_xlsx(df_list)

}
