# Copyright 2013, Dell
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

# Extend RecordNotFound to let us attach some useful error to the exception.
module ActiveRecord
  class RecordNotFound < ActiveRecordError
    attr_accessor :rebar_model, :rebar_column, :rebar_key
  end
end

# This class AUTOMATICALLY extends the ActiveRecord base class
# so that we can add AR helpers for Rebar
module ApiHelper
#/lib/api_helper.rb

  # for the top level classes (finders, etc)
  module ClassMethods
    UUID_RE = /^[\h]{8}-[\h]{4}-[1-5][\h]{3}-[89abAB][\h]{3}-[\h]{12}$/

    # Helper to allow API to use ID or name
    def find_key(key)
      col,key = case
                when db_id?(key) then [:id, key.to_i]
                when UUID_RE.match(key) then [:uuid, key]
                when key.is_a?(ActiveRecord::Base) then [:id, key.id]
                when self.respond_to?(:name_column) then [name_column, key]
                else [:name, key]
                end
      begin
        find_by!(col => key)
      rescue ActiveRecord::RecordNotFound => e
        e.rebar_model = self
        e.rebar_column = col
        e.rebar_key = key
        raise e
      end
    end

    def find_key_cap(key, cap_name, user_id)
      begin
        visible(cap_name, user_id).find_key(key)
      rescue ActiveRecord::RecordNotFound
        raise RebarNotFoundError.new(key, self) if cap_name.end_with?("_READ")
        raise RebarForbiddenError.new(key, self)
      end
    end

    def visible(cap_name, user_id)
      if !Capability.exists?(name: cap_name)
        # For now, if there is no capability, then there is no visible filter.
        Rails.logger.warn("No capability #{cap_name}, falling back to dangerous allow everything")
        return all
      end
      case self.table_name
      when "tenants"
        # Tenants just look up by the tenant ID itself
        return where(["id in (select tenant_id from utc_mapping where capability = ? AND user_id = ?)",cap_name, user_id])
      when "deployment_roles"
        # deployment_roles use their deployment
        return where(["deployment_id in (select id from deployments where deployments.tenant_id in (select tenant_id from utc_mapping where capability = ? AND user_id = ?))",cap_name, user_id])
      when "hammers"
        # Hammers use their node
        return where(["node_id in (select id from nodes where nodes.tenant_id in (select tenant_id from utc_mapping where capability = ? AND user_id = ?))",cap_name, user_id])
      when "runs"
        # Runs use their node
        return where(["node_id in (select id from nodes where nodes.tenant_id in (select tenant_id from utc_mapping where capability = ? AND user_id = ?))",cap_name, user_id])
      when "user_tenant_capabilities"
        # UserTenantCapabilities are... fun, and should probably be normalized by adding more
        # caps.
        #
        # For now, the rule I am using is that users can see their own
        # UTCs, and users with USER_TENANT_CAPABILITY_CREATE and
        # USER_TENANT_CAPABILITY_DESTROY caps can see all UTCs in
        # tenants where they have those caps,
        return where(["id in (
                       select id from user_tenant_capabilities where user_id = ?
                       UNION
                       select id from user_tenant_capabilities
                       where user_tenant_capabilities.tenant_id in (select tenant_id from utc_mapping where user_id = ? AND
                                           capability in (?, ?)))",
                      user_id,
                      user_id,
                      "USER_TENANT_CAPABILITY_CREATE",
                      "USER_TENANT_CAPABILITY_DESTROY"])
      end
      if columns_hash.has_key?("tenant_id")
        # Anything with a tenant id should respond to this rule.
        return where(["#{self.table_name}.tenant_id in (select tenant_id from utc_mapping where capability = ? AND user_id = ?)",cap_name, user_id])
      end
      # If we got here, we are dealing with an untenated object.
      # Perform global capability check instead
      u = User.find_by!(id: user_id)
      u.capable(cap_name,u.tenant_id) ? all : where('false')
    end

    TRANSACTION_MAX_RETRIES = 3
    def retriable_transaction(options = {},&block)
      options[:isolation] ||= :read_committed
      read_only = !!options.delete(:read_only)
      unless connection.open_transactions.zero?
        return yield
      end
      retries = 0
      begin
        simple_transaction(options) do
          ActiveRecord::Base.connection.execute("SET TRANSACTION READ ONLY") if read_only
          yield
        end
      rescue ActiveRecord::StatementInvalid => error
        # Serialization errors should be immediately retried,
        # and we expect that things will eventually serialize.
        # At least, postgres ensures that at least 1 out of n transactions
        # that can trigger a serialization failure will be committed.
        if error.message =~ /PG::TRSerializationFailure/
          Rails.logger.error("Immediately retrying serialization failure")
          retry
        end
        raise error unless (retries <= TRANSACTION_MAX_RETRIES) &&
          connection.open_transactions.zero? &&
          (error.message =~ /(deadlock detected)|(The transaction might succeed if retried)/)
        retries += 1
        logger.error("Deadlock detected, retrying transaction (#{retries})")
        retry
      end
    end

    def unsafe_locked_transaction(&block)
      retriable_transaction do
        ActiveRecord::Base.connection.execute("LOCK TABLE #{table_name}")
        yield if block_given?
      end
    end

    # Run a transaction with a lock on the table this class uses
    def locked_transaction(&block)
      unless connection.open_transactions.zero?
        raise "locked_transaction cannot be called from within another transaction!"
      end
      unsafe_locked_transaction do
        yield if block_given?
      end
    end

    # Helper to determine if a given key is an ActiveRecord DB ID
    def db_id?(key)
      key.is_a?(Fixnum) or key.is_a?(Integer) or key =~ /^[0-9]+$/
    end
  end

  # for each instance (so we can use self)
  module InstanceMethods

  end

  def self.included(base)
    base.extend(ClassMethods)
    base.extend(InstanceMethods)
    base.class_eval do
      class <<self
        alias_method :simple_transaction, :transaction
        alias_method :transaction, :retriable_transaction
      end
    end
  end

end
ActiveRecord::Base.send :include, ApiHelper
