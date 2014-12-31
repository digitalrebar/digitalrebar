# Development Guide

Welcome to the amazing fuzziness of OpenCrowbar!  

This guide is targeted at people who want to _contribute and extend_ OpenCrowbar.  You should review the architectural and operator instructions as part of the learning process.

## Data Model and API

Information about Crowbar's data models and methods is covered in the [model](./model/README.md) section of this guide.  We have intentionally split design and methods (in the model section) from more general [API](./api/README.md) usage guides.  Our intention is to keep the API documentation focused just on using the API and leave more it to curious readers to review the model and principles areas.

## Dev Environment

Our development environments include a _working_ administrative server for testing.  It is very important in our process that developers are able to run deployments in their environment as part of the testing cycle.  

While we have invested in BDD and system tests to catch core logic errors, most changes require performing a deployment to test correctness!

## Let's get started!

Follow the amazing [docs to setup your dev environment!](/doc/development-guides/dev-systems/README.md)
