## Tips & Tricks (UI Developer)

### Dashboard No Polling

When you are troubleshooting the UI or REST APIs, the Node Dashboard (`dashboard`) polling can be a pain because it generates log traffic.  You can disable polling for debug by using the `nopoll` parameter.

For example, `http://192.168.124.10:3000/dashboard/89?nopoll`

### Stop Rendering Navigation 

If you are in developer mode, then you can pass in `?nav=false` and the template will not render the menu.

For example, `http://192.168.124.10:3000/docs?nav=false`

### Force/Stop Documentation reindexing

You can use the `?rebuild=[true/false]` to force the documentation index to rebuild (also happens on web startup).  In developer mode, the documentation will reindex everytime the /doc page is hit.

* in production mode, force reindexing, use `?rebuild=[anything]`
* in developer mode, stop reindex on every load, use `?rebuild=false`