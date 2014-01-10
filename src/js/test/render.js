 var page = require('webpage').create();
 var system = require("system");

page.open(system.args[1], function () {
    page.render(system.args[2]);
    phantom.exit();
});