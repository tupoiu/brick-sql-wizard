-- Made with SQL Wizard
\c testDB2

GRANT USAGE ON SCHEMA public TO joebiden;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO joebiden;
\c testDB1

GRANT USAGE ON SCHEMA public TO joebiden;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO joebiden;
GRANT CONNECT ON DATABASE testDB2 TO joebiden;
GRANT CONNECT ON DATABASE testDB1 TO joebiden;
CREATE USER joebiden WITH PASSWORD 'joebidenspassword';