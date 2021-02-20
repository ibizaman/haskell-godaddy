# Haskell Godaddy

This project is a command-line interface to configure a domain managed
by Godaddy. It also provides a library that can be integrated into
other projects. It uses [Godaddy's
API](https://developer.godaddy.com/getstarted).


# Example Usage

## List existing records

List all domains:

```bash
$ godaddy
Domains:
  example1.com
  DomainID:  1000001
  Status:    CANCELLED
  CreatedAt: 2020-01-01 12:00:00 UTC
  RenewAuto: False

  example2.com
  DomainID:  1000002
  Status:    ACTIVE
  CreatedAt: 2020-01-02 15:00:00 UTC
  RenewAuto: True
```

List all A records under `example2.com` domain:

```bash
$ godaddy example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"
  TTL:      600

  one
  Type:     A
  Data:     "192.168.1.20"
  TTL:      600

  one
  Type:     A
  Data:     "192.168.1.21"
  TTL:      600
```

List all CNAMEs pointing to `example2.com`:

```bash
$ godaddy example2.com @
Records:
  www
  Type:     CNAME
  Data:     "@"
  TTL:      600

  www1
  Type:     CNAME
  Data:     @
  TTL:      600
```

List all CNAMEs pointing to `one.example2.com`:

```bash
$ godaddy example2.com one
Records:
  ftp
  Type:     CNAME
  Data:     one.example2.com
  TTL:      600

  afp
  Type:     CNAME
  Data:     one.example2.com
  TTL:      600
```

List all records pointing to `one.example2.com` (A, CNAME, NS, TXT, etc.):

```bash
$ godaddy example2.com --records
```

## Add or delete A records

```bash
$ godaddy example2.com
Records:

$ godaddy example2.com --add @:192.168.1.10
$ godaddy example2.com --add @:192.168.1.11
$ godaddy example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"
  TTL:      600

$ godaddy example2.com --add www:192.168.1.20
$ godaddy example2.com --add www:192.168.1.21
$ godaddy example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"
  TTL:      600

  www
  Type:     A
  Data:     "192.168.1.20"
  TTL:      600

  www
  Type:     A
  Data:     "192.168.1.21"
  TTL:      600

$ godaddy example2.com --delete www
$ godaddy example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"
  TTL:      600
```

Trying to add a duplicate A record produces an error:
```bash
$ godaddy example2.com --add www:192.168.1.20
Got an error from Godaddy: [422] "Unprocessable Entity":
[DUPLICATE_RECORD] Another record with the same attributes already exists:
  field "records": A record name [www] conflicts with another record.
```

Deleting an nonexistent record does not produce an error.

## Add or delete CNAME records

```bash
$ godaddy example2.com @
Records:

$ godaddy example2.com @ --add root1
$ godaddy example2.com @ --add root2
$ godaddy example2.com @
Records:
  root1
  Type:     CNAME
  Data:     "@"
  TTL:      600

  root2
  Type:     CNAME
  Data:     "@"
  TTL:      600

$ godaddy example2.com www
Records:

$ godaddy example2.com www --add www1
$ godaddy example2.com www --add www2
$ godaddy example2.com www
Records:
  www1
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

  www2
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

$ godaddy example2.com www --delete www2
$ godaddy example2.com www
Records:
  www1
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

```

Trying to add a duplicate CNAME record produces an error:
```bash
$ godaddy example2.com www --add www1
Got an error from Godaddy: [422] "Unprocessable Entity":
[DUPLICATE_RECORD] Another record with the same attributes already exists:
  field "records": CNAME record name [www1] conflicts with another record.
```

Deleting a nonexistent record does not produce an error.

# Installation

## From Source

For now, that's all there is.

### Using Nix

```bash
$ nix-build

# Executable is in ./result/bin/haskell-godaddy-exe
```

### Using stack

```bash
stack build
```

Or

```bash
stack --nix build
```

# Use the Godaddy Test OTE Endpoint

Set the following environment variable:

```bash
GODADDY_TEST_ENDPOINT=true
```

And then do requests using this executable.

# Introspect API Calls

You can also use a custom 'SC.BaseUrl', for example to intercept
calls to Godaddy with mitmproxy. Start mitmproxy with:

```bash
mitmproxy --mode reverse:https://api.godaddy.com
```

Set the following environment variable:

```bash
GODADDY_CUSTOM_ENDPOINT=127.0.0.1:8080
```

And then do requests using this executable.
