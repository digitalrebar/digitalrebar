# Copyright 2011, Dell
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


def lock(new_resource)
  filename = "/tmp/#{new_resource.file.gsub("/","_")}.lock"
  f = ::File.new(filename, ::File::RDWR|::File::CREAT, 0644)
  rc = false
  count = 0
  while rc == false do
    count = count + 1
    rc = f.flock(::File::LOCK_EX|::File::LOCK_NB)
    sleep 1 if rc == false
  end

  return f
end

def unlock(f)
  f.flock(::File::LOCK_UN)
  f.close
end


####
#  add and remove lines to a file
# - file - the file to operate on
# - add - the string to make sure is in the file
# - filter_re - a regular expression (forced between ^$) to remove
#
# if a line matching the filter_re also matches the add line, then do neither.
def add_and_filter(file, add, filter_re)
  lock = lock(@new_resource)
  need_to_add = !add.nil?
  need_to_remove = false
  lines = []
  re = Regexp.compile("^#{add}\s*$")
  ll = [] 
  ll = IO.readlines(file) if ::File.exists?(file)
  ll.each { |l|
    need_to_add = false if l.match(re)
    unless (filter_re.nil?)
      if md=l.match(/^#{filter_re}$/)
	  if md[0].match(re)
	    # we're adding and removing the same thing...
	    need_to_remove = false
	    need_to_add = false
	    lines << l
	  else
	    need_to_remove = true
	  end
      else
	lines << l
      end
    else
      lines << 1
    end
  }

  puts "need to add: #{need_to_add} remove: #{need_to_remove}"
  updated =  need_to_add or need_to_remove

  if need_to_remove
    lines << add if need_to_add
    need_to_add = false
    open(file, "w+") do |f|
      lines.each do |l|
        f.write(l)
      end
    end
  end
  open(file, "a") do |f|
    f.puts "#{add}"
  end if need_to_add

  return updated

ensure
  unlock(lock)
end


action :add do
  updated = add_and_filter(@new_resource.file,
			   @new_resource.name,
                           @new_resource.regexp_exclude)
  @new_resource.updated_by_last_action(true) if updated
end

action :remove do
  updated = add_and_filter(@new_resource.file,nil,
			   "^#{@new_resource.name}$")
  @new_resource.updated_by_last_action(true) if updated
end
