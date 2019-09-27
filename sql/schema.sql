CREATE TABLE IF NOT EXISTS accounts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  privkey TEXT,
  pubkey TEXT
);
CREATE TABLE IF NOT EXISTS activities (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  activity_id TEXT,
  activity_type TEXT,
  activity_blob TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS account_activities (
  account_id TEXT NOT NULL,
  activity_id TEXT NOT NULL,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (activity_id) REFERENCES activities(id),
  PRIMARY KEY (account_id, activity_id)
);
CREATE TABLE IF NOT EXISTS actors (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  uri TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS followers (
  account_id TEXT NOT NULL,
  actor_id TEXT NOT NULL,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (actor_id) REFERENCES actors(id),
  PRIMARY KEY (account_id, actor_id)
);
CREATE TABLE IF NOT EXISTS following (
  account_id TEXT NOT NULL,
  actor_id TEXT NOT NULL,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (actor_id) REFERENCES actors(id),
  PRIMARY KEY (account_id, actor_id)
);
