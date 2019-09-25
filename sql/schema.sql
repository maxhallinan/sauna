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
  account_id TEXT NOT NULL,
  activity_id TEXT NOT NULL,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (activity_id) REFERENCES activities(id),
  PRIMARY KEY (account_id, activity_id)
);
