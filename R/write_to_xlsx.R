#' Write content to an Excel workbook
#'
#' @param x An R object or list of R objects
#' @param wb_name (Optional) The name of the workbook to be created (e.g. 'output.xlsx' or 'output/results.xlsx'). If no value is specified, a new workbook is created in the working directory.
#' @param open_wb Whether to open the workbook using Excel once it's been created
#'
#' @return Creates an Excel workbook at the path provided by `wb_name` or as a new file in the working directory. If `open_wb = TRUE`, the workbook will be opened in Excel if possible.
#' @export
#'
#' @examples
#' df_list <- list('iris' = iris, mtcars, 'Arrests' = USArrests)
#'
#' # With a specified output path
#' write_to_xlsx(df_list, wb_name = "output/r-default-datasets.xlsx")
#'
#' # With an automatically-generated output path
#' write_to_xlsx(df_list)
#'
write_to_xlsx <- function(x, wb_name = NULL, open_wb = TRUE) {

  wb <- openxlsx::createWorkbook()

  if (is.data.frame(x)) {
    content_list <- list(x)
  } else if (is.list(x)) {
    content_list <- x
  }

  for (content_index in seq_along(content_list)) {
    # Establish name of new worksheet
    sheet_name <- names(content_list)[content_index]
    is_bad_sheet_name <- is.null(sheet_name) || is.na(sheet_name) || sheet_name == ""
    if (is_bad_sheet_name) {
      sheet_name <- sprintf("Sheet_%s", content_index)
    }
    # Add new worksheet
    openxlsx::addWorksheet(wb, sheetName = sheet_name)

    # Write the data (If the content is a dataframe, write as a table)
    if (is.data.frame(content_list[[content_index]])) {
      openxlsx::writeDataTable(wb, sheet = sheet_name,
                               x = content_list[[content_index]])
    } else {
      openxlsx::writeData(wb, sheet = sheet_name,
                          x = content_list[[content_index]])
    }
  }

  # Save the resulting workbook
  if (is.null(wb_name) || is.na(wb_name) || wb_name == "") {
    wb_filepath <- tempfile(pattern = "temp-wb-",
                            fileext = ".xlsx",
                            tmpdir = getwd())
    wb_filepath <- suppressWarnings(normalizePath(wb_filepath, winslash = "/"))
  } else {
    if (!endsWith(wb_name, ".xlsx")) {
      stop("`wb_name` must end in `.xlsx`")
    }
    wb_filepath <- wb_name
  }

  # Create new directories as necessary
  split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
  new_folders <- rev(setdiff(split_path(wb_filepath), c(basename(wb_filepath), ".")))

  if (length(new_folders) > 0) {
    folder_to_create <- new_folders[1]
    i <- 1L
    while (i <= length(new_folders)) {
      if (!dir.exists(folder_to_create)) {
        dir.create(folder_to_create)
      }
      folder_to_create <- file.path(folder_to_create, new_folders[i + 1L])
      i <- i + 1L
    }
  }

  openxlsx::saveWorkbook(wb, file = wb_filepath, overwrite = TRUE)
  message(sprintf("Output written to `%s`", wb_filepath))
  if (open_wb) {
    sys_cmd <- sprintf('open excel "%s"', wb_filepath)
    shell(sys_cmd)
  }
}
