### BDD Tips and Tricks

#### Usefule Steps in the BDD_Catchall Module

The catchall module has some useful steps for special cases and debugging code that did not fit in other places.  In fact, the step generator function is really a BDD catchall!

Here are some useful steps found is the BDD_Catchall module:

* HTTP Server Logging
  * _I mark the logs with "Mark"_: handy to mark the logs so you can find a failure

* Time Lapse
  * _pause "Time" seconds to "Message"_: if you have a time dependant issue that you cannot fix the right way
  * _after", Time, "seconds_: same as pause
  * _after", Time, "minutes_: same as pause
  * _after", Time, "milliseconds_: same as pause

* Logging 
  * _I debug BDD_: dumps log information into the BDD stream the the debug level
  * _I "puts" BDD_: same as above but at puts
  * _I "[debug | trace | info |... ]" BDD_: same as above but user chooses the level

* Play with pass/fail
  * _I do nothing to "Text"_: does nothing
  * _I always pass_: does what it says
  * _I always fail_: does what it says
