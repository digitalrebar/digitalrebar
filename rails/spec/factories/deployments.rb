# spec/factories/network.rb

require 'faker'

FactoryGirl.define do
  factory :deployment do |f|
    f.parent_id 1
    f.name 'squeaky'
  end
end
