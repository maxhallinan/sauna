#!/usr/bin/env bash
cat sql/drop.sql | sqlite3 $DB_FILENAME
cat sql/schema.sql | sqlite3 $DB_FILENAME
