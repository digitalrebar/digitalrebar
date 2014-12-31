# Copy21, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  
  def list_from_hashes(hashes, key, options={:del => ". "})
    a_list = ""
    return a_list if (hashes.nil? or key.nil?)
    a_list
  end

  def dl_item(term, definition, options={})
    unless definition.blank? && options[:show_if_blank] != true
      # need to add non-breaking space entity if we are showing blanks otherwise layout
      # is broken when rendered.
      if definition.blank?
        definition = "&nbsp;" 
        options[:escape_html] = false
      end
      html  = "<dt>#{options[:escape_html] != false ? (h term) : (term)}</dt>"
      dd = "<dd" + (options[:class].nil? ? "" : " class='"+options[:class]+"'") + (options[:title].nil? ? "" : " title='" + options[:title]+"'") + ">"
      html += "#{dd}#{options[:escape_html] != false ? (h definition) : (definition)}</dd>"
      raw html
    end
  end

  def cb_column_class(current_column, total)
    if (current_column % total) == 0
      "first"
    elsif (current_column % total) == (total-1)
      "last"
    end
  end
  
  def format_memory(kB)
    mem = (kB.to_f / 1024 / 1024)
    "#{sprintf("%#1.2f", mem)} GB"
  end
  
  def hash_to_ul(hash)
      result = "<ul>"
      hash.each do |key,value|
          result << "<li>"
          if key.is_a?(Hash)
              result << hash_to_ul(key)
          else
              result << "<em>#{key}</em>"
          end
          if value.is_a?(Hash)
              result << hash_to_ul(value)
          else
              result << ( value == nil ? "" : ": #{value}" )
          end
          result << "</li>"
      end
      result << "</ul>"
  end
  
end
