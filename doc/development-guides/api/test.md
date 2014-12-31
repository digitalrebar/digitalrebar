# Test APIs

OpenCrowbar provides a comprehensive testing framework.  While we make every effort to use the API for all test functions, some activities require speciailized testing hooks.  It is not desired to change primary APIs to serve testing use-cases; consequently, the API has a reserved area for specialized test interfaces.

WARNING: The Test APIs (=/api/test/...=) are considered to be tightly coupled to the code base and have *no contract* for consistency between versions.

> Test APIs should _never_ be called except from the testing framework!  They are not considered stable or public.

## Load Node Data from File (api/test/nodes/[:id]?source)

| Verb | URL | Comments |
|:-----|:---------------------|:---------------------|
| PUT  | /api/test/node/[:id] | returns Node json | 

Options
   * source = specify the file to be loaded.  All files should be in the =[app base]/test/data= path.
