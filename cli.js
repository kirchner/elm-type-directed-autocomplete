#!/usr/bin/env node

const readline = require('readline');

const program = require('./dist/cli.js').Elm.Cli.init({});


program.ports.receiveSuggestions.subscribe(function(suggestions) {
  console.log(JSON.stringify(suggestions));
});


var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function(line) {
  var params = JSON.parse(line);

  if (params) {
    program.ports.requestSuggestions.send(params);
  }
});
