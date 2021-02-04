modalBody_saveFile <- function(include_inputs,
                               nameLabel = "Name",
                               descriptionLabel = "Description",
                               sourceLabel = "Source",
                               sourceTitleLabel = "Title",
                               sourcePathLabel = "URL",
                               licenseLabel = "License",
                               tagsLabel = "Tags",
                               categoryLabel = "Category"){

  include_inputs <- c("name", "description", "sources")

  include_inputs_lst <- as.list(include_inputs)
  filter_by <- unlist(lapply(include_inputs_lst, function(x) if(x == "sources") c("source_title", "source_path") else x))

  input_options <- list(name = textInput("name", nameLabel),
                        description = textInput("description", descriptionLabel),
                        source_title = textInput("source_title", sourceLabel, value = "", placeholder = sourceTitleLabel),
                        source_path = textInput("source_path", " ", value = "", placeholder = sourcePathLabel),
                        license = selectInput("license", licenseLabel, choices = c("CC0", "CC-BY")),
                        tags = selectizeInput("tags", tagsLabel, choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
                        # chipsInput(inputId = "tags", label = i_("gl_tags", lang()), placeholder = i_("gl_type_tags", lang())),
                        # chipsInput(inputId = "tags", label = "Tags", placeholder = "Type tags"),
                        category = selectizeInput("category", categoryLabel, choices = list("No category" = "no-category")))

  input_options[filter_by]
}
