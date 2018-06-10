var express = require('express');
var app = express();
var bodyParser = require('body-parser');
var   request = require('request');

//set body-parser options
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));

var mysql = require('mysql');

//input mysql connect
var db, resultdb;

const child_process = require('child_process');

var fs = require('fs');//file system module

var BPMtimer;
var isBPMRunning = false;

var line = 1; //input id


app.get('/run', function(req, res){
	if (isBPMRunning) //if BPM is running -> don't run again
		res.send('One BPM has been running.');
	else { //else, BPM is not running -> run BPM
		//input mysql connect
		db = mysql.createConnection({
			host : '10.96.75.205',
			user : 'autolab',
			password : 'auto63906',
			database : 'CPA',
			port : 3306
		}); 
		// result mysql connect
		resultdb = mysql.createConnection({
			host : '10.96.75.205',
			user : 'autolab',
			password : 'auto63906',
			database : 'CPA',
		});
		
		db.connect();
		//resultdb.connect();
		
		isBPMRunning = true;
		BPMrun();
		
		console.log('BPM is now running!');
		res.send('BPM is now running!');
	}
});

app.post('/stop', function(req, res){
	
	isBPMRunning = false;
	//end db connection
	resultdb.end();
	db.end();
	
	console.log('BPM is now stopped!');
	res.send('BPM is now stopped!');
});


function BPMrun(){
	if (isBPMRunning == false)
		return;
	
	console.log('input_id: ' + line);
	db.query('SELECT * FROM input2 where input_id = ' + line++ , function(err,result){
		if (err) {
			//console.log("mysql error");
			console.log(err);
		} else {
			//console.log(result[0]['BF1_Mean']);
			fs.writeFile("Y.txt", result[0]['BF1_Mean'], function(err) {
				if(err) 
					console.log(err);
				else{
					console.log("The Yfile was saved!");
					fs.readFile('Y.txt', 'utf8', function (err,ydata) {
						if (err) return console.log(err);					
						console.log(ydata);	
						request.get("http://127.0.0.1:3000/BPM/ydataReply/cpa4/" + ydata, function(){} );
					});	
				}
			});
			fs.writeFile("X.txt", result[0]['NH31_Mean']+" "+result[0]['Power1_Mean']+" "+result[0]['Press1_Mean']+" "+result[0]['SiH41_Mean'], function(err) {
				if(err) 
					console.log(err);
				else {
					console.log("The Xfile was saved!");
					fs.readFile('X.txt', 'utf8', function (err,xdata) {
						if (err) return console.log(err);					
						//console.log(xdata);	
						var splitx = xdata.split(" ");
						var eachxdata = {
							NH31_Mean : splitx[0],
							Power1_Mean : splitx[1],
							Press1_Mean : splitx[2],
							SiH41_Mean : splitx[3],
						};
						console.log(eachxdata);
			
						//request.get("http://127.0.0.1:3000/BPM/xdataReply/cpa4/" + splitx[0] + "/" + splitx[1] + "/" + splitx[2] + "/" + splitx[3], function(){} );
					});
					//start bpm

					var workerProcess = child_process.exec('Rscript main_BPM.R',
					//var workerProcess = child_process.exec('sudo R --vanilla --slave < main_BPM.R',
						function (error, stdout, stderr) {
							if (error) {
								console.log(error.stack);
								console.log('Error code: '+error.code);
								console.log('Signal received: '+error.signal);
							}
						}
					);
					
					workerProcess.on('exit', function (code) {
						console.log('Child process exited with exit code ' + code);
						// write DHI to db
						fs.readFile('DHI.txt', 'utf8', function (err,data) {
							if (err) 
								return console.log(err);
							
							var resultdata = {
								_id : '5760d331e7c3c3214d636085',
								HI : data
							};
							
							console.log(resultdata);
							
							//request.get("http://140.116.234.174:1113/BPM/HIReply/" +  + "/" + (Math.round(data*100)/100), function(){} );
							request.get("http://140.116.234.166:30231/BPM/HIReply/57849b66e7c3c3214d63608e/" + (Math.round(data*100)/100), function(){} );
							BPMrun();
						});	
					});	
				}
			});
		}
	});
	
}


var server = app.listen(8054);
