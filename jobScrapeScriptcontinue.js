// scrape_techstars.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'reed.html'

page.open('http://www.reed.co.uk/jobs?keywords=data%20scientist&sortby=DisplayDate&jobtitleonly=True&isnewjobssearch=True&pagesize=100', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});
