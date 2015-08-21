#
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

require 'securerandom'
require 'base64'
require 'rbnacl/libsodium'

class PasswordChangeToken < ActiveRecord::Base

  belongs_to :user
  before_create :gen_key
  
  def as_json(opts = nil)
    opts ||= {}
    opts[:only] = [:token]
    super(opts)
  end

  def decode(decoder,nonce,payload)
    key = RbNaCl::PrivateKey.new(Base64.decode64(self.key))
    pubkey = RbNaCl::PublicKey.new(Base64.decode64(decoder))
    box = RbNaCl::Box.new(pubkey,key)
    if created_at < (Time.now - 3600)
      destroy
      raise "Password change token expired"
    end
    res = box.decrypt(Base64.decode64(nonce),Base64.decode64(payload))
    destroy
    JSON.parse(res)
  end

  private

  def gen_key
    k = RbNaCl::PrivateKey.generate
    pk = k.public_key

    self.key = Base64.encode64(k.to_bytes).strip
    self.token = Base64.encode64(pk.to_bytes).strip
  end
  
end
