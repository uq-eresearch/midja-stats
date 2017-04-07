var express = require('express');
var router = express.Router();

var rio = require('rio');

/* GET home page. */
router.get('/', function(req, res, next) {
  res.setHeader('Content-Type', 'text/plain');
  res.write('Midja stats scatterplot...');
  res.end();
});


var rResultProcess = function(err, res) {
  if(!err) {
    return res;
  } else {
    throw new Error("Rserve call failed: "+err);
  }
}

var validate_structure = function(inp) {
  if(typeof(inp.dataset)!=='string' || typeof(inp.xvar)!=='string' || typeof(inp.yvar)!=='string' || typeof(inp.useRemoteness)!=='boolean') {
    err = new TypeError("Invalid input!");
    console.log(err);
    throw(err);
  } else {
    console.log("everything is peachy!");
  }
}

/* POST to service */
router.post('/', function(req, res, next) {
  console.log("received body:", req.body);
  outputjson = req.body;
  if(req.body.expr) {
    rio.evaluate(req.body.expr, {callback: function(e, r){answer = rResultProcess(e, r); console.log("answer=",answer); outputjson.answer=answer; res.json(outputjson); res.end();}});
  } else {
    validate_structure(req.body);
    rexprjson = req.body;
    rexprjson.plotfilesdir = process.cwd()+'/public';
    rexprstr = JSON.stringify(JSON.stringify(rexprjson));
    rio.evaluate('source("'+process.cwd()+'/scatterplot.R"); blah <- midjaNewScatterplot('+rexprstr+')', {callback: function(e,r){
      answer=rResultProcess(e,r);
      outputjson=JSON.parse(answer);
      res.json(outputjson);
      res.end();
    }});
  }
});

module.exports = router;
