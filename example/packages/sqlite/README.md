Sqlite3 interface for Slogan
============================

Sample code
-----------

load_package("sqlite");

let db = sqlite_open("db");

let sql = "create table a(i)";
let stmt = sqlite_prepare(db, sql);
sqlite_step(db, stmt);
sqlite_finalize(db, stmt);

sql = "insert into a values(?)";
stmt = sqlite_prepare(db, sql);
sqlite_bind_int(db, stmt, 1, 100);
sqlite_step(db, stmt);
sqlite_reset(db, stmt);
sqlite_bind_int(db, stmt, 1, 200);
sqlite_step(db, stmt);
sqlite_finalize(db, stmt);

sql = "select * from a";
stmt = sqlite_prepare(db, sql);
let loop (r = sqlite_step(db, stmt))
  if (r == 1) { showln(sqlite_column_int(db, stmt, 0));
                loop(sqlite_step(db, stmt)) };

sqlite_finalize(db, stmt);
sqlite_close(db);