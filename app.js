var express = require('express');
var path = require('path');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');

var util = require('util');

var routes = require('./routes/index');
var scatterplotRoutes = require('./routes/scatterplot');

var app = express();

app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use(function(req, res, next) {
  res.header("Access-Control-Allow-Origin", "*");
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
  next();
});

app.use('/', routes);
app.use('/scatterplot', scatterplotRoutes);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  console.log("!Four-Oh-Four!");
  var err = new Error('Not Found');
  err.errorDescription = 'Path ['+req.url +'] not found.';
  err.status = 404;
  next(err);
});

// error handlers

app.use(function(err, req, res, next) {
  console.log("****ERROR HANDLER****");
  console.log("ENV ==", app.get('env'));
  console.log(util.inspect(err, {showHidden:true, depth:null}))

  errjson = {
    error: err.message,
  }

  if(err.errorDescription) {
    errjson.errorDescription = err.errorDescription;
  } else if(err.body) {
    errjson.errorDescription = err.body;
  }
  errjson.status = err.status

  if(app.get('env') === 'development') {
    errjson.stack = err.stack;
  }

  res.status(err.status || 500);
  res.json(errjson);
});

/*
// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
      message: err.message,
      error: err
    });
  });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
  res.status(err.status || 500);
  res.render('error', {
    message: err.message,
    error: {}
  });
});
*/

module.exports = app;
