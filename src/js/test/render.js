 var page = require('webpage').create();
 var system = require("system");

page.open('test.html', function () {
    page.render(system.args[1]);
    phantom.exit();
});