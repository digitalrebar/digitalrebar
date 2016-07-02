# Copyright 2013-4, Dell
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
Rebar::Application.routes.draw do

  # UI scope

  # special case items that allow IDs to have .s 
  constraints(:id => /.*/ ) do
    resources :nodes do
      resources :node_roles
      resources :attribs
    end
  end

  # UI resources (should generally match the API paths)
  get "annealer", :to => "node_roles#anneal", :as => :annealer
  resources :attribs
  resources :barclamps do
    get :upload
  end
  resources :deployment_roles do
    resources :node_roles
    put :propose
    put :commit
  end
  resources :deployments do
    resources :roles
    resources :node_roles
    get :graph
    get :cohorts
    put :propose
    put :commit
    put :recall

  end

  get "monitor(/:id)" => "deployments#monitor", :as => :monitor, :defaults => {:format => 'html'}
  get 'docs/eula' => 'docs#eula', as: :eula

  resources :groups
  resources :hammers
  resources :available_hammers
  resources :jigs
  resources :node_roles
  resources :roles
  resources :providers
  resources :capabilities
  resources :tenants

  resources :interfaces
  resources :networks do
    resources :network_ranges
    resources :network_routers
    # special views
  end
  resources :network_ranges
  resources :network_routers
  get 'network_map' => "networks#map", :as=> :network_map
  resources :dns_name_filters
  resources :dns_name_entries

  # UI only functionality to help w/ visualization
  scope 'dashboard' do
    get 'list(/:deployment)'  => 'dashboard#list', :as => :bulk_edit
    put 'list'                => 'dashboard#list', :as => :bulk_update
    get 'getready'            => 'dashboard#getready', :as => :getready
    post 'getready'           => 'dashboard#getready', :as => :post_getready
    get 'layercake'           => 'dashboard#layercake', :as => :layercake
    get 'families(/:id)'      => 'dashboard#families', :as => :families
  end
  
  # UI only functionality to help w/ visualization
  scope 'utils' do
    get '/'             => 'support#index', :as => :utils
    get 'i18n/:id'      => 'support#i18n', :as => :utils_i18n, :constraints => { :id => /.*/ }
    get 'marker/:id'    => 'support#marker', :as => :utils_marker
    get 'files/:id'     => 'support#index', :as => :utils_files
    get 'import(/:id)'  => 'support#import', :as => :utils_import
    get 'upload/:id'    => 'support#upload', :as => :utils_upload
    get 'restart/:id'   => 'support#restart', :as => :restart
    get 'digest'        => "support#digest"
    get 'fail'          => "support#fail"
    get 'settings'      => "support#settings", :as => :utils_settings
    put 'settings(/:id/:value)' => "support#settings_put", :as => :utils_settings_put
  end

  # UI scope - legacy methods
  scope 'support' do
    get 'logs', :controller => 'support', :action => 'logs'
    get 'get_cli', :controller => 'support', :action => 'get_cli'
  end

  devise_for :users, { :path_prefix => 'my', :module => :devise, :class_name=> 'User' }
  resources :users, :except => :new

  # API routes (must be json and must prefix v2)()
  scope :defaults => {:format => 'json'} do

    constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :api_version => /v[2-9]/) do

      # framework resources pattern (not barclamps specific)
      scope 'api' do
        match '(*session)' => "users#options", via: [:options]
        get 'license' => 'docs#eula'
        scope 'status' do
          get "nodes(/:id)" => "nodes#status", :as => :nodes_status
          get "deployments(/:id)" => "deployments#status", :as => :deployments_status
          get "heartbeat" => "support#heartbeat", :as => :heartbeat_status
          get "inventory(/:id)" => "inventory#index"
          match "active" => "node_roles#status", via: [:get, :put]
        end
        scope "utils" do 
          post "wizard" => "dashboard#wizard"
        end
        scope 'test' do
          put "nodes(/:id)" => "nodes#test_load_data"
        end
        scope ':api_version' do
          # These are not restful.  They poke the annealer and wait if you pass "sync=true".
          get "anneal", :to => "node_roles#anneal", :as => :anneal
          resources :attribs, :as=>:attribs_api do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :available_hammers do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :barclamps do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :tenants do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :capabilities do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :providers do
            collection do
              get 'sample'
              post 'match'
              get 'templates'
            end
            resources :nodes
          end
          resources :deployment_roles do
            collection do
              get 'sample'
              post 'match'
            end
            resources :roles
            resources :nodes
            resources :attribs
            put :propose
            put :commit
          end
          resources :deployments do
            collection do
              get 'sample'
              post 'match'
            end
            resources :node_roles
            resources :deployment_roles
            resources :roles
            resources :nodes
            resources :attribs
            put :propose
            put :commit
            put :recall
            put :redeploy
          end
          resources :events do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :event_selectors do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :event_sinks do
            collection do
              get 'sample'
              post 'match'
            end
            resources :event_selectors
          end
          resources :groups do
            resources :nodes
          end
          resources :network_ranges do
            collection do
              get 'sample'
              post 'match'
            end
            resources :network_allocations
          end
          resources :network_routers do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :network_allocations do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :networks do
            collection do
              get 'sample'
              post 'match'
            end
            resources :network_ranges
            resources :network_routers
            resources :network_allocations
            member do
              match 'ip', via: [:get, :post, :delete]
              post 'allocate_ip'
              delete 'deallocate_ip'
              get 'allocations'
              get 'auto_ranges/:node_id', to: :auto_ranges
            end
          end
          resources :dns_name_filters do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :dns_name_entries do
            collection do
              get 'sample'
              post 'match'
            end
          end
          resources :interfaces
          resources :runs
          resources :jigs do
            collection do
              get 'sample'
              post 'match'
            end
            put :activate
            put :flush
          end
          resources :nodes do
            collection do
              get 'sample'
              post 'match'
            end
            resources :node_roles
            resources :hammers
            resources :attribs
            resources :roles
            resources :network_allocations
            put :propose
            put :commit
            match :power, via: [:get, :put]
            put :debug
            put :undebug
            put :redeploy
            put :scrub
            get 'addresses'
          end
          resources :hammers do
            collection do
              get 'sample'
              post 'match'
            end
            post :perform
          end
          resources :node_roles do
            collection do
              get 'sample'
              post 'match'
            end
            put :retry
            put :propose
            put :commit
            get :parents
            get :children
            resources :attribs
          end
          resources :roles do
            collection do
              get 'sample'
              post 'match'
            end
            resources :attribs
            resources :deployment_roles
            resources :node_roles
            resources :nodes
          end
          resources :users do
            collection do
              get 'sample'
              post 'match'
            end
            member do
              post "admin", to: :make_admin
              get "start_password_reset"
              post "complete_password_reset"
              delete "admin", to: :remove_admin
              post "lock"
              delete "lock", to: :unlock
              put "reset_password"
              get "digest", to: :digest_password
              get "capabilities"
            end
          end
          match 'digest' => "users#digest", as: :digest_url, via: [:get, :post, :head, :delete]
        end # version
      end # api
    end # id constraints
  end # json

  # Install route from each root barclamp (should be done last so CB gets priority).
  Dir.glob("/opt/digitalrebar/**/rebar_engine/barclamp_*/config/routes.rb") do |routefile|
    bc = routefile.split('/')[-3].partition('_')[2]
    bc_engine = "#{routefile.split('/')[-3].camelize}::Engine"
    bc_mount = "mount #{bc_engine}, at: '#{bc}'"
    eval(bc_mount, binding)
  end

  root :to => 'dashboard#layercake'
end
