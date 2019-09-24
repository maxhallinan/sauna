require('dotenv').config();
const fs = require('fs');
const sqlite3 = require('sqlite3');

const dropSql = fs.readFileSync('./sql/drop.sql', {encoding: 'utf8'});
const schemaSql = fs.readFileSync('./sql/schema.sql', {encoding: 'utf8'});

const db = new sqlite3.Database(process.env.DB_FILENAME);

db.run(dropSql);
db.run(schemaSql);
