## Barclamp Roles - User Interface Development and Testing

When a deployment role is in proposed state the user should be able to modify the role attributes defined for that role.  

In all other states the attributes should be displayed as read-only. Please use the Messaging Barclamp as a reference and this guide for developing and testing other custom role user interfaces.

### Development

The steps for building the UI (view) to edit and display node role attributes are as follows

1. Define the attributes for the Role in the Barclamp's crowbar.yml if they are not already present. These definitions will create Attrib objects for the Role in Crowbar.

    Sample attributes:
```yaml
roles:
  - name: messaging-server
    jig: chef-solo
    requires:
      - openstack-base
        attribs:
          # name used as id field in haml input tags.  Needs to follow this convention: [barclamp]-[role]_rest_of_id
          - name: 'messaging-openstack_endpoints_mq_host' 
          # Plain English description of attrib
            description: 'Messaging Host'
          # map to value in hash: hsh['openstack']['endpoints']['mq']['host'] for example
            map: 'openstack/endpoints/mq/host'
          - name: 'messaging-openstack_endpoints_mq_port'
            description: 'Messaging Port'
            map: 'openstack/endpoints/mq/port'
          - name: 'messaging-crowbar_messaging_mq_user'
            description: 'Messaging User'
            map: 'crowbar_messaging/mq/user'
          - name: 'messaging-crowbar_messaging_mq_password'
            description: 'Messaging Password'
            map: 'crowbar_messaging/mq/password'
```
2. Create the view for the role

    Crowbar expects the view file location and name to follow the convention:
    ```
    [barclamp_name]/crowbar_engine/barclamp_[barclamp_name]/app/views/barclamp_[barclamp_name]/node_roles/_[@role.name].html.haml
    ```

    For the messaging server role the following Rails partial was created:
    ```
    messaging/crowbar_engine/barclamp_messaging/app/views/barclamp_messaging/node_roles/_messaging-server.name.html.haml
    ```
    The partial is made up of four main components, the form, the validation rules, the validation messages and the read-only view. The template for the overall partial should follow this pattern:

  ```haml
  - data_nil_empty = (data.nil? || data=={})
  - if @node_role.proposed? # The node role can be edited in this state, show the form
    %dl.attribs
      %dt= t('.[label_key]')
      %dd= text_field_tag 'data_[@role.name]_[map_to_value]', (data_nil_empty || data["map"]["to"]["value"].nil?) ? template["map"]["to"]["value"] : data["map"]["to"]["value"] , :size => 30
      ...
    = hidden_field_tag :dataprefix, "data_"

    :javascript
      var rules = new Array();
      // cannot use regular json syntax because you cannot have hyphen in key names and attribs have hyphen, TODO should re-factor attribs to use json friendly key names
      rules["data_[@role.name]_[map_to_value]"] = new Object({
        required: true,
        minlength: 8
        //... 
      });
      //... 
      var messages = new Array();
      messages["data_[@role.name]_[map_to_value]"] = new Object({
        required: "#{t('.[field_label_required]', size: 8)}"
        //... 
      });
      //..

  - else # Not in proposed state, show read-only page.
    %dl.attribs
     %dt= t('.[label_key]')
     %dd= (data_nil_empty) ? template["map"]["to"]["value"] : data["map"]["to"]["value"]
     ...
  ```
  * *IMPORTANT*: The ids used in the form fields *MUST* match the ids used to build the rules and messages javascript arrays.
    
3. Add localization for all labels and validation messages. This follows the conventions mentioned in the [localization](localization.md) documentation and the general localization pattern for the view above is as follows:
```
en:
  barclamp_[barclamp_name]:
    node_roles:
      [role_name]:
        label_key: Label Value
        message_key: Message with parameter: %{parameter}
```

4. Override Role hooks if needed
    
    If any special actions need to take place prior to sending the data down to the node after the Deployment is committed you can override one of the hooks declared in [Role](https://github.com/crowbar/barclamp-crowbar/blob/master/crowbar_framework/app/models/role.rb).  
    
    For example in the messaging barclamp an encrypted databag needed to be created on the admin node populated with the user and password from the form.  The data bag is then copied downstream to the node prior to a chef run.
    
    Create a class that extends Role and use the following name/location convention:
    ```
    class Barclamp[Role::Name} < BarclampChef::Role
    
    [barclamp_name]/crowbar_engine/barclamp_[barclamp_name]/app/models/barclamp_[barclamp_name]/[role_name].rb

    ```
    For example, in the messaging barclamp the class used for the server role hook override is:
    ```
    class BarclampMessaging::Server < BarclampChef::Role
    
    messaging/crowbar_engine/barclamp_messaging/app/models/barclamp_messaging/server.rb
    ```
    The hook override used in the encrypted data bag use case is the *on_todo* hook which is called when the node role is moved into the to_do state once all, if any, blocking parent roles make it to active state, but before the data is pushed down to the target node, so this hook is ideal for this use case.  Sample code from Messaging Barclamp below:
    ```ruby
    def on_todo(node_role, *args)
       nrd= node_role.data
       if(!nrd.nil? && nrd != {} && !nrd["crowbar_messaging"]["mq"]["user"].nil? \
       && !nrd["crowbar_messaging"]["mq"]["password"].nil?)
      messaging_user_id = nrd["crowbar_messaging"]["mq"]["user"]
      messaging_password = nrd["crowbar_messaging"]["mq"]["password"]
      store_credential( "messaging", "user", messaging_user_id, messaging_password)
    end
  end
    ```

### Testing

A typical front-to-back testing scenario is outlined below, using the Messaging Barclamp as an example:

1. Start the Admin node, log in and create new Deployment.
2. Start a new test node, either a VM or actual hardware.
3. Validate the test node has PXE booted and is the discovered state in the UI
4. Create a new Deployment and add the single role you are trying to test, messaging-server for example.
5. Add the newly discovered node to the Deployment
6. At the intersection of the role and node click the green + icon to expand all the parent roles.  
7. At this point the very last role, from left-to-right, should be the role you are testing with a blue diamond icon at the intersection of the node and role. The blue diamond indicates the node role is in the Proposed state. Click this icon, this will bring you to the Node Role view that contains the functionality you are testing.
8. Before proceeding copy the ID of the node role you are editing to be used later on. This can be found by looking at the URL of the page. For instance the following  http://192.168.124.10:3000/node_roles/84, shows that the node role is 84
8. Validate the form fields and labels are correct that the form validation is working properly. Validation error messages should be displayed to the right of the field in question. The tester should know what each field's validation rules are supposed to be to validate the rules.
  1. Test required fields by clearing them all and attempt to save the node role. You should see required messages for every field in the messaging server role for example as every field is required
  2. Validate and field length rules are working correctly, there are on-key-up event handlers on each field and when the length doesn't meet the defined max/min length you should be notified.
  3. Validate special case fields like password and email.  In messaging there is a custom validator defined that will not allow special characters in the password.  If you enter % you should see a validation error message.
  4. Enter all required information in the correct format and save the node role.  You should see a notification, in the standard global notification section of the page, that the node role has been saved successfully.
  5. Navigate through the deployments menu to get back to the deployment node role list page again.  Click the blue icon for the role you are testing and validate the information you previously changed repopulates the form.
  6. Make additional changes and repeat previous step the validate the additional update was successful. The reason for this is the first time you edited the node role you were overwriting the defaults, creating a new object. This second pass is an update of that object.
9. Testing of the rendered form is done at this point. It may be worthwhile to validate model data itself is correct prior to committing the deployment. This can be easily done through the Rails console:
  1. SSH into the admin node navigate to the crowbar_framework director
  ```
  :~$ cd /opt/dell/crowbar_framework
  ```
  2. Start the rails console
  ```
  :~$ RAILS_ENV=development bundle exec rails c
  ```
  3. Use the Rails console to retrieve the node role object
  ```
  irb(main):001:0> nr = NodeRole.find(84)
  ```
  4. Verify the model matches the changes made in UI
  ```
  irb(main):001:0> y nr.data # This prints out a yaml version of the data that was modified in the UI
  ```
  It should look something like:
  
    ```yaml
      openstack:
        endpoints:
          mq:
            port: 5532
            host: 127.0.0.1
        crowbar_messaging:
          mq:
            user: the_user
            password: the_password
    ```

11. If the information looks correct in the model commit the Deployment in the UI.  While the parent node is executing parent roles like installing the operating system etc, you can take a look at the read-only node role view by clicking the grey circle icon (indicating blocked state) at the intersection of the node and role.  This will take you to the read-only node role view. Validate the fields and data correct.
12. When the Deployment is finished and active the last step is to verify the settings set in the UI actually made it to the target node and configured the service correctly.  The validation steps will be different for each role. For the Messaging Server role the following should be verified:
  1. SSH into the target node and verify that the service is running
  ```
  :~$ sudo rabbitmqctl status
  ```
  2. Verify the settings are correct in the RabbitMQ config and environment files 
  ```
  :~$ sudo less /etc/rabbitmq/rabbitmq.config
  :~$ sudo less /etc/rabbitmq/rabbitmq-env.conf
  ```
13.  This completes testing and verification of the entire life-cyle, from the UI to the actual deployed service.