var express = require('express');
var app = express();
var mysql = require('mysql');
var fs = require('fs');//file system module
const child_process = require('child_process');
//input mysql connect
var db = mysql.createConnection({
	host : '192.168.43.253',
	user : 'weilun',
	password : 'auto63906',
	database : 'bpminput',
	port : 3306
});
// result mysql connect
var resultdb = mysql.createConnection({
	host : 'localhost',
	user : 'root',
	password : 'auto63906',
	database : 'bpm',
});

db.connect();

db.query('SELECT * FROM input1 where input_id = 2' , function(err,result){
	
	if (err) {
		//console.log("mysql error");
		console.log(err);
	} else {
		//console.log(result[0]['BF1_Mean']);
		fs.writeFile("Y.txt", result[0]['BF1_Mean'], function(err) {
		if(err) {
			console.log(err);
		} else {
			console.log("The Yfile was saved!");
		}
		});
		fs.writeFile("X.txt", result[0]['NH31_Mean']+" "+result[0]['Power1_Mean']+" "+result[0]['Press1_Mean']+" "+result[0]['SiH41_Mean'], function(err) {
		if(err) {
			console.log(err);
		} else {
			console.log("The Xfile was saved!");
			
			//start bpm

			var workerProcess = child_process.exec('sudo R --vanilla --slave < main_BPM.R',
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
					if (err) {
						return console.log(err);
					}
					
					var resultdata = {
						cpa_id : '57577db3e7c3c3214d636081',
						DHI : data
					};
					console.log(resultdata);
					resultdb.connect();
					resultdb.query('INSERT INTO bpm_result SET ? ' , resultdata, function(err){
						//console.log("mysql error");
						if (err) console.log(err);	
						resultdb.end();
					});
				});	
			});
					
		}
		});
	}
});

db.end();



