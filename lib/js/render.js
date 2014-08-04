var port, server, service,
    system = require('system');

port = "1337";
server = require('webserver').create();

service = server.listen(port, function (request, response) {

    console.log('Request at ' + new Date());
    console.log(JSON.stringify(request, null, 4));

    response.statusCode = 200;
    response.headers = {
        'Cache': 'no-cache',
        'Content-Type': 'text/html'
    };

	var page = require('webpage').create();
	var system = require("system");

	page.settings.resourceTimeout = 3000;

	page.open(request.postRaw + ".html", function () {
	    page.render(request.postRaw + ".png");
	    console.log(request.postRaw + ".png");
	    response.write("OK")
	    response.close();
	});
});

if (service) {
    console.log('Web server running on port ' + port);
} else {
    console.log('Error: Could not create web server listening on port ' + port);
    phantom.exit();
}
