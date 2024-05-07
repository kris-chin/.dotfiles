#!/usr/bin/env ruby
require "yaml"

require_relative "edit_utils"

#Helper Function that recurses into a hash and returns path strings for its keys
def get_leaf_paths(hash_or_string, path, path_list)
  #base case: a string was provided
  if (hash_or_string.is_a?(String))
    return [*path_list, "#{path}/#{hash_or_string}"]
  end

  #recursive case: a hash was provided
  results = []
  hash_or_string.collect.each do |key, value|
    new_path = "#{path}/#{key}"
    new_path_list = get_leaf_paths(value, new_path, path_list)
    results.concat(new_path_list)
  end

  return results
end

# Sets themes
# themes_config - config yml that determines which theme to use per app
# file_mapping - config yml that maps applications to config files
# theme_directory - main directory where all theme data is stored
def set_themes(themes_config, file_mapping_config, theme_directory)
  #1. Read configs
  themes = YAML.load_file(themes_config)
  file_mapping = YAML.load_file(file_mapping_config)

  #2. For each section of active-themes.yml, get the respective theme data
  # also trims leading "/"
  paths = get_leaf_paths(themes, "", [])
    .map { |path_name| path_name[1..-1] }

  #3. Call our util to write the contents of the specially-formatted section of the config file
  #   we use the path_name as the ID
  paths.each do |path_name|
    #determine the app name by getting only the first subpath
    app_name = path_name.split("/").first
    #determine the id by getting everything except for the last subpath
    id_name = path_name.split("/")[0..-2].join("/")

    input_file_name = "#{theme_directory}/#{path_name}"
    output_file_name = File.expand_path(file_mapping[app_name])
    write_file_into_commented(id_name, input_file_name, output_file_name)
  end
end

set_themes("./active-themes.yml", "./config-mapping.yml", File.expand_path("~/.themes"))
