<br>

<p align="center">
    <a href="https://erlang.org/">
      <img alt="Erlang/OTP 26+" src="https://img.shields.io/badge/Erlang%2FOTP-26%2B-green?style=flat-square">
    </a>
    <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img alt="Apache-2.0" src="https://img.shields.io/github/license/shortishly/msec?style=flat-square">
    </a>
</p>

# MySQL/MariaDB Edge Cache (MSEC)

msec replicates data from MySQL/MariaDB, accessed with a Redis API with
persistent storage.

## Features

- Acts as a replica to MySQL databases [using replication with Global
  Transaction Identifiers][mysql-gtid-replication].
- Acts as a replica to MariaDB databases, [using replication with
  Global Transaction Identifiers][mariadb-gtid].
- Replicated Data is peristed using
  [Leveled][github-martinsumner-leveled], a simple Key-Value store
  based on the concept of Log-Structured Merge Trees.
- Redis Compatible API, data can be accessed using the
  [hget][redis-hget] and [hgetall][redis-hgetall] commands.

## Get Started

Follow the [Docker installation
instructions][docker-engine-installation] to get Docker running on
your system.

Clone this repository for the docker [compose.yaml](compose.yaml) with
sample data installed.

```shell
git clone https://github.com/shortishly/msec.git
```

Alternatively, with the [Github CLI][cli-github-com] installed use:

```shell
gh repo clone shortishly/msec
```

Change to the newly cloned directory:

```shell
cd msec
```

Start everything up with:

```shell
./bin/up
```

### Redis API

The keys used by the Redis API are of the form:

```shell
database.table.key
```

In this example, we get all the hash members in the `shortishly`
database, from the `grades` table, identified by `234-56-7890` (Betty
Rubble).

```shell
redis-cli hgetall shortishly.grades.234-56-7890

 1) "test4"
 2) "90.0"
 3) "test3"
 4) "80.0"
 5) "test2"
 6) "90.0"
 7) "test1"
 8) "44.0"
 9) "ssn"
10) "234-56-7890"
11) "last"
12) "Rubble"
13) "grade"
14) "C-"
15) "first"
16) "Betty"
17) "final"
18) "46.0"
```

Or just Betty's grade:

```shell
redis-cli hget shortishly.grades.234-56-7890 grade

"C-"
```

A 'C-' seems harsh, lets give her a 'C' instead:

```shell
./bin/db-sql --execute="update shortishly.grades set grade='C' where ssn='234-56-7890'"
```

The cache is automatically updated:

```shell
redis-cli hget shortishly.grades.234-56-7890 grade

"C"
```

[cli-github-com]: https://cli.github.com
[docker-engine-installation]: https://docs.docker.com/engine/installation/
[github-martinsumner-leveled]: https://github.com/martinsumner/leveled
[mariadb-gtid]: https://mariadb.com/kb/en/gtid/
[mysql-gtid-replication]: https://dev.mysql.com/doc/mysql-replication-excerpt/8.0/en/replication-gtids.html
[redis-hget]: https://redis.io/commands/hget/
[redis-hgetall]: https://redis.io/commands/hgetall/
