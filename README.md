# Haskell Godaddy

This project is a command-line interface to configure a domain managed
by Godaddy. It also provides a library that can be integrated into
other projects. It uses [Godaddy's
API](https://developer.godaddy.com/getstarted).

On top of providing means to create and delete records on Godaddy,
this tool provides dyndns functionality and can help handling Let's
Encrypt DNS01 challenge.


# Example Usage

We start with usage examples but before all this can work, you need to
set your credentials as environment variables. See the next section.

## List all domains

```bash
$ godaddy domains
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

## List all records of a domain

```bash
godaddy records example2.com
Records:
  mycname1
  Type:     CNAME
  Data:     "mycname1.example2.com"
  TTL:      3600

  mg
  Type:     MX
  Data:     "mxb.mailgun.org"
  Priority: 10
  TTL:      3600

  mg
  Type:     MX
  Data:     "mxa.mailgun.org"
  Priority: 10
  TTL:      3600

  mg
  Type:     TXT
  Data:     "v=spf1 include:mailgun.org ~all"
  TTL:      360
```

## Create a record of a domain

```bash
$ godaddy create --help
Usage: godaddy create DOMAIN RECORDTYPE NAME DATA [--port PORT]
                      [--priority PRIORITY] [--protocol PROTOCOL]
                      [--service SERVICE] [--ttl TTL]
                      [--weight WEIGHT]

$ godaddy create example2.com TXT mytxt blahblah

$ godaddy records example2.com
Records:
[...]
  mytxt
  Type:     TXT
  Data:     "blahblah"
  TTL:      600
```

## Delete a record of a domain

```bash
$ godaddy delete example2.com TXT mytxt
```


## DynDns
Assuming your Godaddy's domain is `mydomain.com`:

Serve a domain `mydomain.com` under a dynamic IP:

```bash
$ godaddy dyndns example2.com DOMAIN
```

This will automatically query the external IP through ipify. A more
manual way would be:

```bash
$ ip="$(curl https://api.ipify.org)"
$ godaddy servers replace DOMAIN @
```


## Let's Encrypt DNS01 challenge

Retrieve a wildcard certificate on the `*.example2.com` subdomain and
`example2.com` domain.

```bash
$ certbot certonly --preferred-challenges=dns --manual -n \
    --manual-auth-hook godaddy-certbot-dns01-auth-hook \
    --manual-cleanup-hook godaddy-certbot-dns01-cleanup-hook \
	-d example2.com
```

# Opinionated commands

This project provides opinionated commands. Everything can be done of
course with the commands provided above, but if you want something a
bit more specific, continue reading.

The following commands assume a server is a physical machine
accessible through an IP address. This definition is mapped exactly to
an A record. Listing servers is equivalent to listing A records:

```bash
$ godaddy servers list example2.com
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

List all subdomains pointing to `example2.com`:

```bash
$ godaddy subdomains list example2.com @
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

List all subdomains pointing to `one.example2.com`:

```bash
$ godaddy subdomains list example2.com one
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
$ godaddy records example2.com
```

## Add or delete servers

```bash
$ godaddy servers list example2.com
Records:

$ godaddy servers add example2.com @:192.168.1.10 @:192.168.1.11
$ godaddy servers list example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"
  TTL:      600

$ godaddy servers add example2.com www:192.168.1.20 www:192.168.1.21
$ godaddy servers list example2.com
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

$ godaddy servers delete example2.com www
$ godaddy servers list example2.com
Records:
  @
  Type:     A
  Data:     "192.168.1.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.1.11"

$ godaddy servers replace example2.com @:192.168.2.10 @:192.168.2.11
$ godaddy servers list example2.com
Records:
  @
  Type:     A
  Data:     "192.168.2.10"
  TTL:      600

  @
  Type:     A
  Data:     "192.168.2.11"
  TTL:      600
  TTL:      600
```

Trying to add a duplicate A record produces an error:
```bash
$ godaddy servers add example2.com www:192.168.1.20
Got an error from Godaddy: [422] "Unprocessable Entity":
[DUPLICATE_RECORD] Another record with the same attributes already exists:
  field "records": A record name [www] conflicts with another record.
```

Deleting an nonexistent record does not produce an error.

## Add or delete subdomains

```bash
$ godaddy subdomains list example2.com @
Records:

$ godaddy subdomains add example2.com @ root1 root2
$ godaddy subdomains list example2.com @
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

$ godaddy subdomains add example2.com www www1 www2
$ godaddy subdomains list example2.com www
Records:
  www1
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

  www2
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

$ godaddy subdomains delete example2.com www www2
$ godaddy subdomains list example2.com www
Records:
  www1
  Type:     CNAME
  Data:     "www.example2.com"
  TTL:      600

```

Trying to add a duplicate CNAME record produces an error:
```bash
$ godaddy subdomains add example2.com www www1
Got an error from Godaddy: [422] "Unprocessable Entity":
[DUPLICATE_RECORD] Another record with the same attributes already exists:
  field "records": CNAME record name [www1] conflicts with another record.
```

Deleting a nonexistent record does not produce an error.

# Set Credentials

Before any of the commands above work, Godaddy credentials will need
to be set. The binary will search for them, in order, in a command
line argument `--credentials`, an environment variable
`GODADDY_API_CREDENTIALS` or a configuration file.

The first two expect the credentials to be given in the `KEY:SECRET`
format.


# Configuration File

The binary searches for a configuration file `./godaddy.conf` in the
current directory or in `/etc/godaddy/godaddy.conf` if the first one
does not exist. If the `--config=FILEPATH` argument is given, it will
read the configuration file in the given location.

The configuration file contains the following:

```
[CREDENTIALS]
apikey=KEY
apisecret=SECRET
```

The KEY and SECRET should not be quoted and the config file must
contain a trailing newline.


# Installation

## Archlinux

```bash
yay -S haskell-godaddy
```

## From Source

Both options will produce an executable where the linker is in
/nix/store. Use the `build-release` Makefile target to produce an
executable linked to the default linux linker.

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
