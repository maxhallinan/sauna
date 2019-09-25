CREATE TABLE IF NOT EXISTS accounts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  privkey TEXT,
  pubkey TEXT
);
CREATE TABLE IF NOT EXISTS activities (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  activity_id TEXT,
  activity_type TEXT
);
CREATE TABLE IF NOT EXISTS account_activities (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  account_id TEXT,
  activity_id TEXT,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (activity_id) REFERENCES activities(id)
);
