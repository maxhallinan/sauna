CREATE TABLE IF NOT EXISTS account_activities (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  account_id TEXT,
  activity_id TEXT,
  FOREIGN KEY (account_id) REFERENCES accounts(id),
  FOREIGN KEY (activity_id) REFERENCES activities(id)
);
