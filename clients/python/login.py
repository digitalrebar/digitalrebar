# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# login to Rebar Example

import urllib2

URL      = 'https://127.0.0.1:3000'
Realm    = 'Rebar'
Username = 'developer'
Password = 'Cr0wbar!'

authhandler = urllib2.HTTPDigestAuthHandler()
authhandler.add_password(Realm, URL, Username, Password)
opener = urllib2.build_opener(authhandler)
urllib2.install_opener(opener)
page_content = urllib2.urlopen(URL)
print page_content.getcode()

page_content = urllib2.urlopen(URL + '/api/v2/barclamps')
print page_content.getcode()
