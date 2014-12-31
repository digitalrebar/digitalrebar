#### What are the BDD *rats?

The BDD system honors the Cucumber pattern of adding the suffix "rat" to the base step processors.  

These base step processors are:

* Webrat (`bdd_webrat.erl`) which handles all the of the standard HTML page and forms processing
* RESTrat (`bdd_restrat.erl`) which handles REST API calls
* CLIrat (`bdd_clirat.erl`) which handles CLI calls

In many cases, good tests will combine steps from multiple rat files.

For example, a CLI test that creates a user should validate the return from the CLI and may also choose to use the REST API steps to confirm that the call created the correct result in the database.  Along the same lines, a test may use the API to create items that it wants to appear on the HTML UI for testing.  This is a desired practice!

> The use of "rat" as a suffix likely refers to Cucumber's use of the Capybara rendering engine.  A Capybara is a giant rat.

Serious test developers are strongly encouraged to submit new *rat steps!  The more good steps the better the ftamework; however, you are also encouraged to start with steps in the most narrow scope and gradually move them into larger scopes.  Consequently, we expect that steps in the *rat routines have been throughly tested and vetted for general goodness.