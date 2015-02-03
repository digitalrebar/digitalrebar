#### BDD Config

The BDD configuration file contains information that tells BDD how to execute.  While BDD adds to this file during execution, users choose which values it starts with.

BDD selects the `default.config` file automatically.  You can choose which configuration to load by passing it into the `bdd:test(config)` or `bdd:feature(config, feature)` methods.

<table>
  <tr>
    <th>Item</th>
    <th>Required</th>
    <th>Default</th>
    <th>Comment</th>
  </tr>
  <tr>
    <td>url</td>
    <td>yes</td>
    <td>none</td>
    <td>This is the URL that BDD will use for testing</td>
  </tr>
  <tr>
    <td>user</td>
    <td>no</td>
    <td>none</td>
    <td>If your site requires auth, then this is required</td>
  </tr>
  <tr>
    <td>password</td>
    <td>no</td>
    <td>none</td>
    <td>If your site requires auth, then this is required.  WARNING: Retained in clear text!  Do not store production passwords here!</td>
  </tr>
  <tr>
    <td>log</td>
    <td>no</td>
    <td>[true, puts, info, warn, error]</td>
    <td>used by bdd_utils:log printouts.  Create list with none, some or all of the following: [puts, trace, debug, info, warn]</td>
  </tr>
  <tr>
    <td>titles</td>
    <td>no</td>
    <td>[pass, fail, skip, header, result, feature, step, step_pass, step_fail]</td>
    <td>used by bdd_utils:log printouts.</td>
  </tr>
  <tr>
    <td>environment</td>
    <td>no</td>
    <td>undefined</td>
    <td>used by Unless step prefix to skip tests</td>
  </tr>
  <tr>
    <td>results_out</td>
    <td>no</td>
    <td>/tmp/bdd_results.out</td>
    <td>stores the detailed results of the tests.  Used by bdd:failed().</td>
  </tr>
  <tr>
    <td>coverage_out</td>
    <td>no</td>
    <td>/tmp/bdd.html</td>
    <td>HTML version of test results</td>
  </tr>
  <tr>
    <td>marker_url</td>
    <td>no</td>
    <td>undefined</td>
    <td>If undefined, this behavior is turned off.  If defined, BDD does a web request to URL with debug information to make it easier to find matching steps in the log.  For OpenCrowbar, the url is `utils/marker`</td>
  </tr>
  <tr>
    <td>marker_log/td>
    <td>no</td>
    <td>/var/log/crowbar/development.log</td>
    <td>Should point to the path where you log API calls</td>
  </tr>
  <tr>
    <td>cli</td>
    <td>no</td>
    <td>undefined</td>
    <td>Used by bdd_clirat for the command to the CLI if not in the given</td>
  </tr>
  <tr>
    <td>cli_user_key</td>
    <td>no</td>
    <td>--username</td>
    <td>Used by bdd_clirat to pass the username into the CLI</td>
  </tr>
  <tr>
    <td>cli_password_key</td>
    <td>no</td>
    <td>--password</td>
    <td>Used by bdd_clirat to pass the password into the CLI</td>
  </tr>
  <tr>
    <td>cli_url_key</td>
    <td>no</td>
    <td>--url</td>
    <td>Used by bdd_clirat to pass the URL into the CLI</td>
  </tr>
  <tr>
    <td>system_phantom</td>
    <td>no</td>
    <td>system-phantom.internal.local</td>
    <td>Used by Crowbar to set the name of the phantom</td>
  </tr>
  <tr>
    <td>system_phantom_roles</td>
    <td>no</td>
    <td>["dns-service", "ntp-service","dns-mgmt_service"]</td>
    <td>Used by Crowbar to set the name of the phantom roles</td>
  </tr>
</table>


##### Example Config

    %%-*-erlang-*- 
    {url, "http://192.168.124.10:3000"}.
    {user, "developer"}.
    {password,"Cr0wbar!"}.
    {feature_path,"features/"}.
    {extension, "feature"}.
    {global_setup, crowbar}.
    {secondary_step_files, [crowbar_rest, crowbar, bdd_webrat, bdd_restrat, bdd_catchall]}.
    {translation_error, "translation_missing"}.
