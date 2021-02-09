modalBody_saveFile <- function(id,
                               include_inputs,
                               nameLabel = "Name",
                               descriptionLabel = "Description",
                               sourceLabel = "Source",
                               sourceTitleLabel = "Title",
                               sourcePathLabel = "URL",
                               licenseLabel = "License",
                               tagsLabel = "Tags",
                               tagsPlaceholderLabel = "Type tag(s) and press enter after each one",
                               categoryLabel = "Category",
                               categoryChoicesLabels = c("No category"),
                               categoryChoicesIDs = c("no-category")){

  ns <- NS(id)

  include_inputs_lst <- as.list(include_inputs)
  filter_by <- unlist(lapply(include_inputs_lst, function(x) if(x == "sources") c("source_title", "source_path") else x))

  if(!length(categoryChoicesIDs) == length(categoryChoicesLabels))
    stop("Category ids and category labels must be vectors of the same length.")

  names(categoryChoicesIDs) <- categoryChoicesLabels

  input_options <- list(name = textInput(ns("name"), nameLabel),
                        description = textInput(ns("description"), descriptionLabel),
                        source_title = textInput(ns("source_title"), sourceLabel, value = "", placeholder = sourceTitleLabel),
                        source_path = textInput(ns("source_path"), " ", value = "", placeholder = sourcePathLabel),
                        license = selectInput(ns("license"), licenseLabel, choices = c("CC0", "CC-BY")),
                        tags = chipsInput(inputId = ns("tags"), label = tagsLabel, placeholder = tagsPlaceholderLabel),
                        category = selectizeInput(ns("category"), categoryLabel, choices = categoryChoicesIDs))

  input_options[filter_by]
}

modalFunction_saveFile <- function(...) {

  args_orig <- list(...)
  element <- args_orig$element
  user_name <- args_orig$user_name
  org_name <- args_orig$org_name

  elementType <- args_orig$elementType

  if(!elementType %in% c("dsviz", "fringe", "drop")) stop("Element must be of type 'fringe', 'dsviz', or 'drop'.")

  element <- dsmodules:::eval_reactives(element)
  user_name <- dsmodules:::eval_reactives(user_name)

  # extract string of updateTextInput field that needs to be updated if name is empty
  name_field <- names(args_orig)[grepl('name$',names(args_orig))]

  # remove empty string arguments
  args <- args_orig[args_orig != ""]

  # extract input field names from args
  names(args) <- sub('.*\\-', '', names(args))

  name <- args$name

  if (is.null(name)) {
    name <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
  }

  tags <- args$tags
  if(!is.null(tags)){
    if(length(tags) == 1){
      tags <- list(tags)
    }
  }

  element_params <- list(element,
                         name = name,
                         description = args$description,
                         sources = list(list(title = args$source_title,
                                             path = args$source_path)),
                         license = args$license,
                         tags = tags,
                         category = args$category)

  # run dsviz(), fringe(), or drop()
  el <- do.call(elementType, element_params)

  # save pin (if org_name is not NULL, saved in org_name, otherwise in user_name)
  pins <- dspin_urls(element = el, user_name = user_name, org_name = org_name)
}
