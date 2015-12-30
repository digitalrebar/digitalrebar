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

# sessions needed for AJAX CORS
Rebar::Application.config.force_ssl = true
Rebar::Application.config.session_store :cookie_store,
  key: '_rebar_session',
  secret: "Digtal_Rebar_was_OpenCrowbar",
  secure: true, # flags cookies as secure only in production
  httponly: false # should be true by default for all cookies
	    