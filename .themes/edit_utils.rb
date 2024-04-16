#!/usr/bin/env ruby

#This file will contain utils for finding a specifically-formatted comment in a file, and replacing the contents of our theme file within the comment
#eg. Look for #[GENERATED_CODE: ALACRITTY_THEME] and [END_GENERATED_CODE] and replace the whole contents underneath it
#This doesn't look for any sort of specific type of comment. (ie. "--", "//", "#" all work).

#Returns the line numbers of generated comments in a section.
#Also returns the indentations of the comments respectively
#[beginning_index, ending_index, whitespace_count]
def find_formatted_comment(filename, id)
  beginning_index = nil
  ending_index = nil
  whitespace_count = nil

  begin
    File.open(filename, "r") do |file|

      file.each_line.with_index do |line, line_number|
        if line.include?("[GENERATED_CODE: #{id}]")
          beginning_index = line_number
          #get whitespace count leading up to first non-whitespace
          whitespace_count = 0
          line.each_char do |char|
            #break if non-whitespace
            break if !char.match(/\s/)
            whitespace_count += 1
          end
        end

        if line.include?("[END_GENERATED_CODE: #{id}]")
          ending_index = line_number
        end
      end

      #If we found both formatted comments
      if beginning_index != nil and
          ending_index != nil and
          whitespace_count != nil
        return [beginning_index, ending_index, whitespace_count]
      else
        raise "Couldn't find comment with id=\"#{id}\" in filename=\"#{filename}\""
      end
    end
  end
end

#Takes a multi-line string and writes it in between the given line numbers of file-b
#Adds and removes lines as necessary, also accounts for whitespace indentation
def write_in_sections(filename, input_string, begin_line, end_line, indentation_string)
  input_lines = input_string.split("\n").map { |line| "#{indentation_string}#{line}\n" }

  #First, get the content of the file as lines
  file_content = File.readlines(filename)

  #Filter the file content by removing the lines within the generated code comments
  #Then turn it into a single string
  new_content = file_content
    .each
    .with_index
    #remove lines that fall between our input indices
    .reject { |line, line_index|
      line_index > begin_line and
        line_index < end_line
    }
    #get rid of the index and keep only the line content
    .map { |line| line[0] }
    #insert our string into the beginning of the code comment
    .insert(begin_line + 1, input_lines)
    #turn our array into a single string
    .join("")
  #2. Replace the content of the file with our new content
  File.open(filename, "w") { |file| file.write(new_content) }
end

#Reads all of the contents of a file and returns it as a multi-line string
def read_file_data_as_string(filename)
  begin
    File.open(filename, "r") do |file|
      return file.each_line.to_a.join("")
    end
  end
end

#Takes the content in input_file and writes it within the formatted comments of output_file
def write_file_into_commented(id, input_file, output_file)
  begin
    #1. Get the content from the input file
    new_content = read_file_data_as_string(input_file)
    #2. Find the formatted comments of the output file
    beginning, ending, whitespace_count = *find_formatted_comment(output_file, id)
    #3. Write the input file into the output file
    write_in_sections(output_file, new_content, beginning, ending, " " * whitespace_count)
  rescue StandardError => e
    puts("An error occurred: #{e}")
  end
end
