
var http = require('http');

var server = http.createServer(function(req, res){
  if (req.url != '/events') return res.end();
  res.writeHead(200, { 'Content-Type': 'text/event-stream' });
  var id = setInterval(function(){
    res.write('data: ' + Date.now() + '\n\n');
  }, 500);
  req.on('end', function(){
    clearInterval(id);
  });
});

server.listen(3000);

