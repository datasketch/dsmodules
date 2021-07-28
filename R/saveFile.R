modalBody_saveFile <- function(id,
                               include_inputs,
                               plan = "basic",
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
                               categoryChoicesIDs = c("no-category"),
                               accessLabel = "Visibility",
                               accessChoicesLabels = c("Public", "Private"),
                               upgradeButtonLabel = "Upgrade now",
                               upgradeText = "To use this feature please upgrade to our Pro plan."
                               ){

  ns <- NS(id)

  include_inputs_lst <- as.list(include_inputs)
  filter_by <- unlist(lapply(include_inputs_lst, function(x) if(x == "sources") c("source_title", "source_path") else x))

  if(!length(categoryChoicesIDs) == length(categoryChoicesLabels))
    stop("Category ids and category labels must be vectors of the same length.")

  names(categoryChoicesIDs) <- categoryChoicesLabels

  accessChoicesIDs <- c("public", "private")
  names(accessChoicesIDs) <- accessChoicesLabels

  access <- div(div(class = "control-label", accessLabel),
                p( style = "margin-bottom: 15px;margin-top: 10px;",
                   upgradeText),
                shiny::actionButton(inputId='url_upgrade', label = upgradeButtonLabel,
                                    onclick ="window.open('https://datasketch.co/dashboard/upgrade', '_blank')"))

  if(!plan == "basic") access <- radioButtons(ns("access"), label = accessLabel, choices = accessChoicesIDs, selected = "public", inline = TRUE)

  input_options <- list(name = textInput(ns("name"), nameLabel),
                        description = textInput(ns("description"), descriptionLabel),
                        source_title = textInput(ns("source_title"), sourceLabel, value = "", placeholder = sourceTitleLabel),
                        source_path = textInput(ns("source_path"), " ", value = "", placeholder = sourcePathLabel),
                        license = selectInput(ns("license"), licenseLabel, choices = c("CC0", "CC-BY")),
                        tags = shinyinvoer::chipsInput(inputId = ns("tags"), label = tagsLabel, placeholder = tagsPlaceholderLabel),
                        category = selectizeInput(ns("category"), categoryLabel, choices = categoryChoicesIDs),
                        access = access
                        )

  input_options[filter_by]
}

modalFunction_saveFile <- function(...) {

  args_orig <- list(...)
  element <- args_orig$element
  type <- args_orig$type
  user_name <- args_orig$user_name
  org_name <- args_orig$org_name
  bucket_id <- args_orig$bucket_id

  if(is.null(element)) stop("Need element to save.")
  if(is.null(type)) stop("Need type to save file.")
  if(is.null(user_name) & is.null(org_name)) stop("Need user_name or org_name to save file.")
  if(is.null(bucket_id)) bucket_id <- "user"

  if(!type %in% c("dsviz", "fringe", "drop")) stop("Element must be of type 'fringe', 'dsviz', or 'drop'.")

  element <- dsmodules:::eval_reactives(element)
  user_name <- dsmodules:::eval_reactives(user_name)

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

  access <- args$access
  if(is.null(access)) access <- "public"

  element_params <- list(element,
                         name = name,
                         description = args$description,
                         sources = list(list(title = args$source_title,
                                             path = args$source_path)),
                         license = args$license,
                         tags = tags,
                         category = args$category,
                         access = access)

  # add namespace to dsviz(), fringe(), or drop() function
  element_function_ns <- "dspins::"
  if(type == "fringe") element_function_ns <- "homodatum::"

  element_function <- paste0(element_function_ns, type)

  # run dsviz(), fringe(), or drop() with namespace
  el <- do.call(getfun(element_function), element_params)

  # save pin (if org_name is not NULL, saved in org_name, otherwise in user_name)
  board <- dspins::ds_board_s3(user_name = user_name, org_name = org_name, bucket_id = bucket_id)

  board %>% dspins::dspin_urls(element = el)
}
