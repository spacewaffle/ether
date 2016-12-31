
var http = require('http');
var fs = require('fs');
var Rx = require('rx');
var readline = require('readline');

var RxNode = require('rx-node');

var rl = readline.createInterface({
  input: process.stdin
});
var input = RxNode.fromReadLineStream(rl)

var server = http.createServer(function(req, res){
  if (req.url != '/events') return res.end();
  res.writeHead(200, { 'Content-Type': 'text/event-stream' });
  /*
  var id = setInterval(function(){
    res.write('data: ' + Date.now() + '\n\n');
  }, 500);
  */
  var sub = input.subscribe(function(x) {
    res.write('data: ' + x.toString() + '\n\n');
  });
  req.on('end', function(){
    clearInterval(id);
    sub.dispose();
  });
});

server.listen(3000);

