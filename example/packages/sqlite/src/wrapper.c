#include <stdio.h>
#include <stdlib.h>
#include "sqlite3.h"

#define DB_CONTEXT_MAX_PREP_STMTS 100

typedef struct db_context
{
  sqlite3 *db;
  sqlite3_stmt *stmts[DB_CONTEXT_MAX_PREP_STMTS];
  int free_stmts[DB_CONTEXT_MAX_PREP_STMTS];
} db_context;

db_context *c_sqlite_db_open(const char *dbname)
{
  db_context *ctx = (db_context *)malloc(sizeof(db_context));
  int rc;

  if (ctx == NULL)
    {
      fprintf(stderr, "c_sqlite_db_open - failed to allocate db_context.\n");
      return NULL;
    }

  for (rc = 0; rc < DB_CONTEXT_MAX_PREP_STMTS; ++rc)
    ctx->free_stmts[rc] = 0;
  
  rc = sqlite3_open(dbname, &ctx->db);
  if (rc)
    {
      fprintf(stderr, "sqlite3_open failed - %d\n", rc);
      sqlite3_close(ctx->db);
      return NULL;
    }
  return ctx;
}

void c_sqlite_db_close(db_context *ctx)
{
  if (ctx != NULL)
    {
      sqlite3_close(ctx->db);
      free(ctx);
    }
}

static int find_free_stmt(db_context *ctx)
{
  int i;
  for (i = 0; i < DB_CONTEXT_MAX_PREP_STMTS; ++i)
    {
      if (ctx->free_stmts[i] == 0)
        return i;
    }
  return -1;
}

int c_sqlite_db_prepare(db_context *ctx, const char *sql, int nsql)
{
  int rc;
  int i = find_free_stmt(ctx);

  if (i < 0)
    {
      fprintf(stderr, "sqlite3_db_prepare - no free statements, limit of %d exceeded.\n",
              DB_CONTEXT_MAX_PREP_STMTS);
      return -1;
    }

  rc = sqlite3_prepare(ctx->db, sql, nsql, &ctx->stmts[i], NULL);
  if (rc)
    {
      fprintf(stderr, "sqlite3_prepare failed - %d.\n", rc);
      return -1;
    }
  ctx->free_stmts[i] = 1;
  return i;
}

static int check_statement_index(db_context *ctx, int stmtid)
{
  if (stmtid < 0 || stmtid >= DB_CONTEXT_MAX_PREP_STMTS)
    {
      fprintf(stderr, "sqlite3_db - invalid statement index - %d.\n", stmtid);
      return 1;
    }
  if (ctx->free_stmts[stmtid] == 0)
    {
      fprintf(stderr, "sqlite3_db - statement at index %d is not initialized.\n", stmtid);
      return 1;
    }
  return 0;
}

int c_sqlite_db_step(db_context *ctx, int stmtid)
{
  if (check_statement_index(ctx, stmtid)) return -1;
  else
    {
      int rc = sqlite3_step(ctx->stmts[stmtid]);
      if (rc == SQLITE_DONE)
        return 0;
      else if (rc == SQLITE_ROW)
        return 1;
      else
        {
          fprintf(stderr, "sqlite3_step failed - %d.\n", rc);
          return -1;
        }
    }
}

int c_sqlite_db_reset(db_context *ctx, int stmtid)
{
  if (check_statement_index(ctx, stmtid)) return -1;
  else
    {
      int rc = sqlite3_reset(ctx->stmts[stmtid]);
      if (rc)
        {
          fprintf(stderr, "sqlite3_step failed - %d.\n", rc);
          return -1;
        }
      return 0;
    }
}

int c_sqlite_db_bind_int(db_context *ctx, int stmtid, int icol, int val)
{
  if (check_statement_index(ctx, stmtid)) return -1;
  else
    {
      int rc = sqlite3_bind_int(ctx->stmts[stmtid], icol, val);
      if (rc)
        {
          fprintf(stderr, "sqlite3_bind_int failed - %d.\n", rc);
          return -1;
        }
      return 0;
    }
}

int c_sqlite_db_column_int(db_context *ctx, int stmtid, int icol)
{
  if (check_statement_index(ctx, stmtid)) return -1;
  else return sqlite3_column_int(ctx->stmts[stmtid], icol);
}

int c_sqlite_db_finalize(db_context *ctx, int stmtid)
{
  if (check_statement_index(ctx, stmtid)) return -1;
  else
    {
      int rc = sqlite3_finalize(ctx->stmts[stmtid]);
      if (rc)
        {
          fprintf(stderr, "sqlite3_finalize failed - %d.\n", rc);
          return -1;
        }
      ctx->free_stmts[stmtid] = 0;
      return 0;
    }
}
      
