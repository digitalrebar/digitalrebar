% Copyright 2013, Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  eurl://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 

% REST DEFINITIONS
-record(list, {namespace = not_set, type = unknown, data, url = unknown, ids = [], count = -1 }).
  % note: the ids field is for backward compatability against the legacy 2.0 api

-record(array, {namespace = not_set, type = unknown, data, url = unknown, ids = -1, count = -1 }).

-record(obj,  {namespace = not_set, type = unknown, data, url = unknown, id = -1 }).

-record(item, {namespace = unknown, type = unknown, data, url = unknown}).

% return generic data from a call
-record(http, {data = "error", code = 500, url = "/", datatype = "unknown", version="0.0", namespace = bdd_restrat,  details = [] }).

% return generic data from a call
-record(grep, {data = "error"}).

% step definitions!
-record(stepgiven,    {scenario = {not_set,not_set}, step = []}).
-record(stepwhen,     {scenario = {not_set,not_set}, step = []}).
-record(stepthen,     {scenario = {not_set,not_set}, step = []}).
-record(stepfinally,  {scenario = {not_set,not_set}, step = []}).
-record(stepsetup,    {scenario = {not_set,not_set}, step = []}).
-record(stepteardown, {scenario = {not_set,not_set}, step = []}).