#!/usr/bin/node
process.stdin.resume();
process.stdin.setEncoding('utf8');
process.stdin.on('data', function(d){
  process.stdout.write(d.replace(/\b./g, function(a){return a.toUpperCase();}));
});
