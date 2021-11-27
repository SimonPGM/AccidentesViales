// Getting holidays in Colombia
var module = require('colombia-holidays');

var obj = {
    table: []
};

for (let year = 2014; year < 2022; year++) {
    var temp = module.getColombiaHolidaysByYear(year);
    for (let i = 0; i < temp.length; i++) {
        obj.table.push(temp[i]);
    }
}

var json = JSON.stringify(obj);

var fs = require('fs')
fs.writeFile('./holidays.json', json, (err) => {
    if (err)
        console.log(err);
    else {
        console.log("File written successfully\n")
    }
});