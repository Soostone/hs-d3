 var page = require('webpage').create();
 var system = require("system");

page.settings.resourceTimeout = 3000;

page.open(system.args[1], function () {
    page.render(system.args[2]);
    phantom.exit();
});