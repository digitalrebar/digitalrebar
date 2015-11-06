# Copyright 2015, RackN
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
class PcmsDelete < ActiveRecord::Migration

  def self.up

    execute "CREATE RULE pcms_delete AS ON DELETE TO node_role_all_pcms 
      DO INSTEAD
        DELETE FROM node_role_pcms 
        WHERE parent_id=OLD.parent_id OR child_id=OLD.child_id;"
  end

  def self.down

    execute "DROP RULE pcms_delete;"

  end

end
