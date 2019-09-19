const sqlite3 = require('sqlite3');

exports._queryDB = function (db, query, params, eb, cb) {
  db.all(query, params, function (err, rows) {
    if (err) {
      err.name = err.code;
      eb(err);
    } else {
      cb(rows);
    }
  });
};
