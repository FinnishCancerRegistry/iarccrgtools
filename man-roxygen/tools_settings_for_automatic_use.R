
#'
#'
#' This pertains to using \code{\link{use_tools_automatically}}.
#' You need to have a pre-defined settings file for the IARC CRG Tools program
#' you want to use before using \code{\link{use_tools_automatically}}.
#'
#' Each program must have its own settings file. The file extension will be
#' either .dfi or .frm. The name of the file must be the name of the program
#' (one of the items given by \code{\link{tools_program_names}}). E.g.
#' \code{"iarc_check.dfi"}. The settings files must be stored in the
#' working directory set by \code{\link{set_tools_working_dir}}.
#'
#' This R package has pre-defined "sensible defaults" for certain programs,
#' which you can fetch into a specific folder using
#' \code{\link{get_tools_settings_template}}.
#'
#'
